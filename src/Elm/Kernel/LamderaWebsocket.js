/*

import Dict exposing (empty, update)
import Elm.Kernel.Scheduler exposing (binding, fail, rawSpawn, succeed)
import Elm.Kernel.Utils exposing (Tuple2)
import Maybe exposing (Just, Nothing, isJust)
import Platform exposing (sendToApp, sendToSelf)
import Result exposing (map, isOk)
import Websocket exposing (EmMsg, Connection, Event, dataEvent, closedEvent, connection, connectionClosed)

*/

var _LamderaWebsocket_websockets = {};

var _LamderaWebsocket_uuid4 = function() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
    return v.toString(16);
  });
}

var _LamderaWebsocket_createHandle = F2(function(router, url)
{
	return __Scheduler_binding(function(callback)
	{
	    var id = _LamderaWebsocket_uuid4();

	    _LamderaWebsocket_websockets[id] =
            { websocket : null
            , hasListener : false
            , isClosed : false
            }

        console.log("Websocket: created handle");

        callback(__Scheduler_succeed(
            A2(__Websocket_connection, id, url)
        ));
	});
});



var _LamderaWebsocket_sendString = F3(function(router, connection, data)
{
	return __Scheduler_binding(function(callback)
	{
	    var websocketData = _LamderaWebsocket_websockets[connection.a];
	    if (websocketData || websocketData.isClosed) {
	        if (websocketData.websocket) {
                websocketData.websocket.send(data);
            }
            else {
                websocketData.websocket = new WebSocket(connection.b);

                websocketData.websocket.addEventListener('close', function (event) {
                    websocketData.websocket.isClosed = true;
                    console.log("Websocket: close happened in _LamderaWebsocket_sendString");
                    console.log(event);
                });

                websocketData.websocket.addEventListener('open', function (event) {
                    console.log("open");
                    console.log(event);

                    websocketData.websocket.send(data);
                });

                callback(__Scheduler_succeed(0));
            }
	    }
	    else {
	        callback(__Scheduler_fail(__Websocket_connectionClosed));
	    }
	});
});

var _LamderaWebsocket_listen = F2(function(router, connection)
{
	return __Scheduler_binding(function(callback)
	{
        var websocketData = _LamderaWebsocket_websockets[connection.a];

        function addListener() {
            websocketData.hasListener = true;

            websocketData.websocket.addEventListener('close', function (event) {
                websocketData.websocket.isClosed = true;
                console.log("Websocket: close happened in _LamderaWebsocket_listen");
                console.log(event);
                __Scheduler_rawSpawn(A2(__Platform_sendToSelf, router, __Utils_Tuple2(
                    connection,
                    A2(__Websocket_closedEvent, event.code, event.reason)
                )));
            });

            websocketData.websocket.addEventListener('message', function (event) {
                __Scheduler_rawSpawn(A2(__Platform_sendToSelf, router, __Utils_Tuple2(
                    connection,
                    __Websocket_dataEvent(event.data)
                )));
            });
        }

        if (websocketData) {
            if (websocketData.isClosed === false) {
                if (websocketData.websocket) {
                    if (websocketData.hasListener === false) {
                        addListener();
                    }
                }
                else {
                    websocketData.websocket = new WebSocket(connection.b);
                    addListener();
                }
            }
        }
        else {
            _LamderaWebsocket_websockets[connection.a] =
                { websocket : null
                , hasListener : false
                , isClosed : true
                }
            console.log("Websocket: close happened due to reset");
            __Scheduler_rawSpawn(A2(__Platform_sendToSelf, router, __Utils_Tuple2(
                connection,
                A2(__Websocket_closedEvent, 1005, "")
            )));
        }

	});
});

var _LamderaWebsocket_close = F2(function(router, connection) {
    return __Scheduler_binding(function(callback)
    {
        var websocketData = _LamderaWebsocket_websockets[connection.a];

        console.log("Websocket: connection closed by user");

        if (websocketData) {
            websocketData.websocket.close();
            websocketData.isClosed = true;

            callback(__Scheduler_succeed(0));
        }
        else {
            callback(__Scheduler_succeed(0));
        }
    });
});