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
	    try {

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

        }
        catch (error) {
            console.log("Websocket: Exception in _LamderaWebsocket_createHandle. " + error);
        }
	});
});



// The API only has String, so a binary message is decoded as UTF-8 text
// (invalid bytes become U+FFFD). It used to hand Elm the Blob (browsers) or
// Buffer (node's ws) itself as a String, and the first String function on it
// threw and killed the Elm scheduler. The sockets ask for ArrayBuffers
// (binaryType) so this can decode synchronously; anything else that is not a
// string, which could only come from a WebSocket that ignores binaryType, is
// dropped (null).
var _LamderaWebsocket_text = function(data)
{
    if (typeof data === 'string') {
        return data;
    }
    if (typeof TextDecoder !== 'undefined' && (data instanceof ArrayBuffer || ArrayBuffer.isView(data))) {
        return new TextDecoder().decode(data);
    }
    return null;
}

// Queues data sent while the socket is still connecting, and sends it, in
// order, once it opens. Like the first send (which opens the socket), the
// task has already succeeded by then; if the socket never opens, the data is
// dropped, and the app gets the close message.
var _LamderaWebsocket_sendWhenOpen = function(websocketData, data)
{
    if (!websocketData.queue) {
        websocketData.queue = [];
        websocketData.websocket.addEventListener('open', function (event) {
            try {

            var queue = websocketData.queue;
            websocketData.queue = null;
            for (var i = 0; i < queue.length; i++) {
                websocketData.websocket.send(queue[i]);
            }

            }
            catch (error) {
                console.log("Websocket: Exception in _LamderaWebsocket_sendWhenOpen open listener. " + error);
            }
        });
    }
    websocketData.queue.push(data);
}

var _LamderaWebsocket_sendString = F3(function(router, connection, data)
{
	return __Scheduler_binding(function(callback)
	{
	    try {

	    var websocketData = _LamderaWebsocket_websockets[connection.a];
	    // This used to be `websocketData || websocketData.isClosed`, which is
	    // true for every known handle, closed or not, and throws for an unknown
	    // one (a handle from before a page reload), so the task never completed.
	    if (websocketData && !websocketData.isClosed) {
	        if (websocketData.websocket) {
                // This used to call send() whatever the state and never call
                // back, so the task never completed, and on a CONNECTING socket
                // send() throws, so the data was lost as well.
                var readyState = websocketData.websocket.readyState;
                if (readyState === 1) {
                    // OPEN
                    websocketData.websocket.send(data);
                    callback(__Scheduler_succeed(0));
                }
                else if (readyState === 0) {
                    // CONNECTING: send once open, like the first send below.
                    _LamderaWebsocket_sendWhenOpen(websocketData, data);
                    callback(__Scheduler_succeed(0));
                }
                else {
                    // CLOSING or CLOSED, before the close event has arrived.
                    callback(__Scheduler_fail(__Websocket_connectionClosed));
                }
            }
            else {
                websocketData.websocket = new WebSocket(connection.b);
                // See _LamderaWebsocket_text.
                websocketData.websocket.binaryType = 'arraybuffer';

                websocketData.websocket.addEventListener('close', function (event) {
                    try {

                    websocketData.isClosed = true;
                    console.log("Websocket: close happened in _LamderaWebsocket_sendString");
                    console.log(event);

                    }
                    catch (error) {
                        console.log("Websocket: Exception in _LamderaWebsocket_sendString close listener. " + error);
                    }
                });

                websocketData.websocket.addEventListener('error', function (event) {
                    try {

                    websocketData.isClosed = true;
                    console.log("Websocket: error happened in _LamderaWebsocket_sendString");
                    console.log(event);

                    }
                    catch (error) {
                        console.log("Websocket: Exception in _LamderaWebsocket_sendString error listener. " + error);
                    }
                });

                websocketData.websocket.addEventListener('open', function (event) {
                    try {

                    console.log("open");
                    console.log(event);

                    websocketData.websocket.send(data);

                    }
                    catch (error) {
                        console.log("Websocket: Exception in _LamderaWebsocket_sendString open listener. " + error);
                    }
                });

                callback(__Scheduler_succeed(0));
            }
	    }
	    else {
	        callback(__Scheduler_fail(__Websocket_connectionClosed));
	    }


        }
        catch (error) {
            console.log("Websocket: Exception in _LamderaWebsocket_sendString. " + error);
        }
	});
});

var _LamderaWebsocket_listen = F2(function(router, connection)
{
	return __Scheduler_binding(function(callback)
	{
	    try {
        var websocketData = _LamderaWebsocket_websockets[connection.a];

        function addListener() {
            websocketData.hasListener = true;

            websocketData.websocket.addEventListener('close', function (event) {
                try {

                websocketData.isClosed = true;
                console.log("Websocket: close happened in _LamderaWebsocket_listen");
                console.log(event);
                __Scheduler_rawSpawn(A2(__Platform_sendToSelf, router, __Utils_Tuple2(
                    connection,
                    A2(__Websocket_closedEvent, event.code, event.reason)
                )));

                }
                catch (error) {
                    console.log("Websocket: Exception in _LamderaWebsocket_listen close listener. " + error);
                }
            });

            websocketData.websocket.addEventListener('error', function (event) {
                try {

                websocketData.isClosed = true;
                console.log("Websocket: error happened in _LamderaWebsocket_listen");
                console.log(event);
                // Error events are not CloseEvents — they don't have .code/.reason.
                // Use 1006 (AbnormalClosure) as the appropriate fallback.
                //
                // That fallback was for a close message sent from here, and it
                // is no longer sent: an error event is always followed by a
                // close event (browsers and node's ws alike), which the close
                // listener above reports with the real code and reason (1006
                // for a failed connection), so the app got two close messages
                // for one connection. This listener still matters though: on a
                // Lamdera backend WebSocket is node's ws, which throws an error
                // event nobody listens to and then never fires close at all.

                }
                catch (error) {
                    console.log("Websocket: Exception in _LamderaWebsocket_listen error listener. " + error);
                }
            });

            websocketData.websocket.addEventListener('message', function (event) {
                try {

                var text = _LamderaWebsocket_text(event.data);
                if (text === null) {
                    console.log("Websocket: dropped a binary message that could not be decoded as text");
                    return;
                }

                __Scheduler_rawSpawn(A2(__Platform_sendToSelf, router, __Utils_Tuple2(
                    connection,
                    __Websocket_dataEvent(text)
                )));

                }
                catch (error) {
                    console.log("Websocket: Exception in _LamderaWebsocket_listen message listener. " + error);
                }
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
                    // See _LamderaWebsocket_text.
                    websocketData.websocket.binaryType = 'arraybuffer';
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

        }
        catch (error) {
            console.log("Websocket: Exception in _LamderaWebsocket_listen. " + error);
        }

	});
});

var _LamderaWebsocket_close = F2(function(router, connection) {
    return __Scheduler_binding(function(callback)
    {
        try {

        var websocketData = _LamderaWebsocket_websockets[connection.a];

        console.log("Websocket: connection closed by user");

        if (websocketData) {
            // A handle that was never listened to or sent on has no socket yet.
            // close() used to throw on it, so the task never completed, the
            // handle was not marked closed, and a later listen opened it.
            if (websocketData.websocket) {
                websocketData.websocket.close();
            }
            websocketData.isClosed = true;

            callback(__Scheduler_succeed(0));
        }
        else {
            callback(__Scheduler_succeed(0));
        }

        }
        catch (error) {
            console.log("Websocket: Exception in _LamderaWebsocket_close. " + error);
        }
    });
});
