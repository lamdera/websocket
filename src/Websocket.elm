effect module Websocket where { command = MyCmd, subscription = MySub } exposing (Connection, SendError(..), close, createHandle, listen, sendString)

{-|

@docs Connection, SendError, close, createHandle, listen, sendString

-}

import Dict exposing (Dict)
import Elm.Kernel.LamderaWebsocket
import Process
import Task exposing (Task)


{-| A websocket connection
-}
type Connection
    = Connection String String


{-| Create a websocket handle that you can then open by calling listen or sendString.
-}
createHandle : String -> Task Never Connection
createHandle url =
    Elm.Kernel.LamderaWebsocket.createHandle () url


{-| Errors that might happen when sending data.
-}
type SendError
    = ConnectionClosed


connectionClosed : SendError
connectionClosed =
    ConnectionClosed


{-| Send a string
-}
sendString : Connection -> String -> Task SendError ()
sendString connection_ data =
    Elm.Kernel.LamderaWebsocket.sendString () connection_ data
        |> Task.map (\_ -> ())


{-| Close the websocket connection
-}
close : Connection -> Task Never ()
close connection_ =
    Elm.Kernel.LamderaWebsocket.close () connection_
        |> Task.map (\_ -> ())


{-| Listen for incoming messages through a websocket connection. You'll also get notified if the connection closes.
-}
listen : Connection -> (String -> msg) -> msg -> Sub msg
listen connection_ onData onClose =
    subscription (Listen connection_ onData onClose)


init : Task Never (State msg)
init =
    Task.succeed { connections = Dict.empty }


type alias State msg =
    { connections : Dict String ( Process.Id, List { onData : String -> msg, onClose : msg } ) }


type alias EmMsg =
    ( Connection, MyEvent )


type MyEvent
    = DataEvent String
    | ClosedEvent


dataEvent : String -> MyEvent
dataEvent =
    DataEvent


closedEvent : MyEvent
closedEvent =
    ClosedEvent


connection : String -> String -> Connection
connection =
    Connection



--| Closed


onSelfMsg : Platform.Router msg EmMsg -> EmMsg -> State msg -> Task Never (State msg)
onSelfMsg router ( Connection connectionId _, event ) state =
    case Dict.get connectionId state.connections of
        Just ( _, msgs ) ->
            case event of
                ClosedEvent ->
                    List.map (\{ onClose } -> Platform.sendToApp router onClose) msgs
                        |> Task.sequence
                        |> Task.map (\_ -> state)

                DataEvent data ->
                    List.map (\{ onData } -> Platform.sendToApp router (onData data)) msgs
                        |> Task.sequence
                        |> Task.map (\_ -> state)

        Nothing ->
            Task.succeed state


type MySub msg
    = Listen Connection (String -> msg) msg


type MyCmd msg
    = SentData Connection String
    | OpenConnection String (Connection -> msg)
    | CloseConnection Connection


onEffects :
    Platform.Router msg EmMsg
    -> List (MyCmd msg)
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router _ subs state =
    let
        handleSubs remainingSubs newDict =
            case remainingSubs of
                [] ->
                    Task.succeed newDict

                (Listen ((Connection connectionId _) as connection_) onData onClose) :: rest ->
                    case Dict.get connectionId newDict of
                        Just ( pid, msgs ) ->
                            handleSubs
                                rest
                                (Dict.insert
                                    connectionId
                                    ( pid, { onData = onData, onClose = onClose } :: msgs )
                                    newDict
                                )

                        Nothing ->
                            Process.spawn (Elm.Kernel.LamderaWebsocket.listen router connection_)
                                |> Task.andThen
                                    (\pid ->
                                        handleSubs
                                            rest
                                            (Dict.insert
                                                connectionId
                                                ( pid, [ { onData = onData, onClose = onClose } ] )
                                                newDict
                                            )
                                    )
    in
    handleSubs subs (Dict.map (\_ ( pid, _ ) -> ( pid, [] )) state.connections)
        |> Task.map (\dict -> { state | connections = dict })


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        Listen url onData onClose ->
            Listen url (onData >> func) (func onClose)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        SentData connection_ data ->
            SentData connection_ data

        OpenConnection url onOpen ->
            OpenConnection url (onOpen >> f)

        CloseConnection connection_ ->
            CloseConnection connection_
