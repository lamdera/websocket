module Main exposing (main)

{-| An app that connects to an echo server (a server that just replies with the same data we send to it) and lets the user send messages to it.
-}

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task
import Websocket exposing (SendError)


type Msg
    = CreatedHandle Websocket.Connection
    | GotWebsocketData String
    | WebsocketClosed { code : Websocket.CloseEventCode, reason : String }
    | PressedSend
    | SentData String (Result SendError ())
    | TypedInput String


type alias Model =
    { handle : Maybe Websocket.Connection, history : List WebSocketEvent, input : String }


type WebSocketEvent
    = SentText String
    | ReceivedText String
    | SendTextError SendError
    | Disconnected { code : Websocket.CloseEventCode, reason : String }
    | Connected


main : Program {} Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { handle = Nothing
      , input = ""
      , history = []
      }
    , Websocket.createHandle websocketServer |> Task.perform CreatedHandle
    )


websocketServer : String
websocketServer =
    "wss://echo.websocket.org"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        CreatedHandle handle ->
            ( { model | handle = Just handle, history = Connected :: model.history }, Cmd.none )

        GotWebsocketData data ->
            ( { model | history = ReceivedText data :: model.history }, Cmd.none )

        WebsocketClosed closeData ->
            ( { model | handle = Nothing, history = Disconnected closeData :: model.history }
            , -- If we lost connection, create a new handle
              Websocket.createHandle websocketServer |> Task.perform CreatedHandle
            )

        PressedSend ->
            case Debug.log "handle" model.handle of
                Just handle ->
                    ( { model | input = "" }
                    , Websocket.sendString handle model.input |> Task.attempt (SentData model.input)
                    )

                Nothing ->
                    ( model, Cmd.none )

        SentData data result ->
            case result of
                Ok () ->
                    ( { model | history = SentText data :: model.history }, Cmd.none )

                Err error ->
                    ( { model | history = SendTextError error :: model.history }, Cmd.none )

        TypedInput text ->
            ( { model | input = text }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "padding" "16px" ]
        [ Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.value model.input
            , Html.Events.onInput TypedInput
            ]
            []
        , Html.button
            [ Html.Events.onClick PressedSend
            , Html.Attributes.disabled (model.handle == Nothing)
            ]
            [ Html.text "Send" ]
        , Html.div
            []
            [ case model.history of
                [] ->
                    Html.i [] [ Html.text "No data received" ]

                _ ->
                    List.reverse model.history
                        |> List.map
                            (\event ->
                                Html.div
                                    []
                                    [ Html.text
                                        (case event of
                                            SentText text ->
                                                "Sent: " ++ text

                                            ReceivedText text ->
                                                "Received: " ++ text

                                            SendTextError _ ->
                                                "Send error"

                                            Disconnected disconnect ->
                                                "Disconnected: " ++ disconnect.reason

                                            Connected ->
                                                "Connected"
                                        )
                                    ]
                            )
                        |> Html.div []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.handle of
        Just handle ->
            Websocket.listen handle GotWebsocketData WebsocketClosed

        Nothing ->
            Sub.none
