port module Harness exposing (Model, Msg, init, subscriptions, update)

{-| A scriptable lamdera/websocket client used by the browser and Node tests.

The test drives it through the `command` port and reads back every Elm
message it received through the `event` port, in the order `update` saw them.

Risky operations on values that come from the kernel (`String.length` on a
close reason, `String.toUpper` on received data) run in a separate follow-up
message ("probe"), so the raw event is always reported before anything that
could throw. A throw inside `update` kills the Elm scheduler, which the tests
see as a page error plus missing probe events. When the throw happens inside a
kernel event listener, the kernel's own try/catch swallows it, so there is no
page error: the tests also send a `ping` at the end, which a dead scheduler
never answers.

-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Task
import Websocket


port command : (D.Value -> msg) -> Sub msg


port event : E.Value -> Cmd msg


type alias Model =
    { handles : Dict Int Handle
    , listening : Set Int
    , nextId : Int
    , reconnect : Bool
    }


type alias Handle =
    { connection : Websocket.Connection
    , url : String
    }


type Msg
    = GotCommand D.Value
    | CreatedHandle String Bool Websocket.Connection
    | GotData Int String
    | ProbeData Int String
    | GotClose Int { code : Websocket.CloseEventCode, reason : String }
    | ProbeClose Int String
    | Sent Int String (Result Websocket.SendError ())
    | CloseDone Int
    | Pong


init : ( Model, Cmd Msg )
init =
    ( { handles = Dict.empty, listening = Set.empty, nextId = 0, reconnect = False }
    , emit "ready" []
    )


emit : String -> List ( String, E.Value ) -> Cmd msg
emit name fields =
    event (E.object (( "ev", E.string name ) :: fields))


type Command
    = Create String Bool
    | Listen Int
    | Unlisten Int
    | Send Int String
    | Close Int
    | SetReconnect Bool
    | Ping
    | Shutdown


commandDecoder : D.Decoder Command
commandDecoder =
    D.field "cmd" D.string
        |> D.andThen
            (\cmd ->
                case cmd of
                    "create" ->
                        D.map2 Create
                            (D.field "url" D.string)
                            (D.oneOf [ D.field "listen" D.bool, D.succeed False ])

                    "listen" ->
                        D.map Listen (D.field "h" D.int)

                    "unlisten" ->
                        D.map Unlisten (D.field "h" D.int)

                    "send" ->
                        D.map2 Send (D.field "h" D.int) (D.field "data" D.string)

                    "close" ->
                        D.map Close (D.field "h" D.int)

                    "reconnect" ->
                        D.map SetReconnect (D.field "on" D.bool)

                    "ping" ->
                        D.succeed Ping

                    "shutdown" ->
                        D.succeed Shutdown

                    _ ->
                        D.fail ("unknown command " ++ cmd)
            )


codeToString : Websocket.CloseEventCode -> String
codeToString code =
    case code of
        Websocket.NormalClosure ->
            "NormalClosure"

        Websocket.GoingAway ->
            "GoingAway"

        Websocket.ProtocolError ->
            "ProtocolError"

        Websocket.UnsupportedData ->
            "UnsupportedData"

        Websocket.NoStatusReceived ->
            "NoStatusReceived"

        Websocket.AbnormalClosure ->
            "AbnormalClosure"

        Websocket.InvalidFramePayloadData ->
            "InvalidFramePayloadData"

        Websocket.PolicyViolation ->
            "PolicyViolation"

        Websocket.MessageTooBig ->
            "MessageTooBig"

        Websocket.MissingExtension ->
            "MissingExtension"

        Websocket.InternalError ->
            "InternalError"

        Websocket.ServiceRestart ->
            "ServiceRestart"

        Websocket.TryAgainLater ->
            "TryAgainLater"

        Websocket.BadGateway ->
            "BadGateway"

        Websocket.TlsHandshake ->
            "TlsHandshake"

        Websocket.UnknownCode n ->
            "UnknownCode " ++ String.fromInt n


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCommand value ->
            case D.decodeValue commandDecoder value of
                Ok cmd ->
                    runCommand cmd model

                Err err ->
                    ( model, emit "badCommand" [ ( "error", E.string (D.errorToString err) ) ] )

        CreatedHandle url listen connection ->
            let
                h =
                    model.nextId
            in
            ( { model
                | handles = Dict.insert h { connection = connection, url = url } model.handles
                , nextId = h + 1
                , listening =
                    if listen then
                        Set.insert h model.listening

                    else
                        model.listening
              }
            , emit "created" [ ( "h", E.int h ) ]
            )

        GotData h data ->
            ( model
            , Cmd.batch
                [ emit "data" [ ( "h", E.int h ), ( "data", E.string data ) ]
                , Task.perform (\_ -> ProbeData h data) (Task.succeed ())
                ]
            )

        ProbeData h data ->
            ( model, emit "dataProbe" [ ( "h", E.int h ), ( "upper", E.string (String.toUpper data) ) ] )

        GotClose h { code, reason } ->
            let
                reconnectCmd =
                    case ( model.reconnect, Dict.get h model.handles ) of
                        ( True, Just handle ) ->
                            -- The reconnect pattern from examples/echo: every close
                            -- creates a fresh handle and listens to it instead.
                            Websocket.createHandle handle.url |> Task.perform (CreatedHandle handle.url True)

                        _ ->
                            Cmd.none
            in
            ( if model.reconnect then
                { model | listening = Set.remove h model.listening }

              else
                model
            , Cmd.batch
                [ emit "closed" [ ( "h", E.int h ), ( "code", E.string (codeToString code) ), ( "reason", E.string reason ) ]
                , Task.perform (\_ -> ProbeClose h reason) (Task.succeed ())
                , reconnectCmd
                ]
            )

        ProbeClose h reason ->
            ( model, emit "closeProbe" [ ( "h", E.int h ), ( "reasonLength", E.int (String.length reason) ) ] )

        Sent h data result ->
            ( model
            , emit "sent"
                [ ( "h", E.int h )
                , ( "data", E.string data )
                , ( "result"
                  , E.string
                        (case result of
                            Ok () ->
                                "Ok"

                            Err Websocket.ConnectionClosed ->
                                "ConnectionClosed"
                        )
                  )
                ]
            )

        CloseDone h ->
            ( model, emit "closeDone" [ ( "h", E.int h ) ] )

        Pong ->
            ( model, emit "pong" [] )


runCommand : Command -> Model -> ( Model, Cmd Msg )
runCommand cmd model =
    let
        withHandle h f =
            case Dict.get h model.handles of
                Just handle ->
                    f handle

                Nothing ->
                    ( model, emit "badCommand" [ ( "error", E.string ("no handle " ++ String.fromInt h) ) ] )
    in
    case cmd of
        Create url listen ->
            ( model, Websocket.createHandle url |> Task.perform (CreatedHandle url listen) )

        Listen h ->
            ( { model | listening = Set.insert h model.listening }, Cmd.none )

        Unlisten h ->
            ( { model | listening = Set.remove h model.listening }, Cmd.none )

        Send h data ->
            withHandle h
                (\handle ->
                    ( model, Websocket.sendString handle.connection data |> Task.attempt (Sent h data) )
                )

        Close h ->
            withHandle h
                (\handle ->
                    ( model, Websocket.close handle.connection |> Task.perform (\() -> CloseDone h) )
                )

        SetReconnect on ->
            ( { model | reconnect = on }, Cmd.none )

        Ping ->
            -- Answered through a Task, so a dead scheduler cannot answer.
            ( model, Task.perform (\_ -> Pong) (Task.succeed ()) )

        Shutdown ->
            -- Stop reconnecting, stop listening and close every handle, so a
            -- finished test leaves nothing running.
            ( { model | reconnect = False, listening = Set.empty }
            , Dict.toList model.handles
                |> List.map (\( h, handle ) -> Websocket.close handle.connection |> Task.perform (\() -> CloseDone h))
                |> Cmd.batch
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (command GotCommand
            :: (Set.toList model.listening
                    |> List.filterMap
                        (\h ->
                            Dict.get h model.handles
                                |> Maybe.map (\handle -> Websocket.listen handle.connection (GotData h) (GotClose h))
                        )
               )
        )
