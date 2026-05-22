import { describe, it, beforeEach } from 'node:test';
import assert from 'node:assert/strict';

// --- Stubs for Elm Kernel internals ---

function F2(fn) { fn.a = 2; return fn; }
function F3(fn) { fn.a = 3; return fn; }
function A2(fn, a, b) { return fn(a, b); }
function A3(fn, a, b, c) { return fn(a, b, c); }

function makeStubs() {
  const spawned = [];
  const callbacks = [];

  return {
    spawned,
    callbacks,
    __Scheduler_binding: (fn) => { callbacks.push(fn); },
    __Scheduler_succeed: (val) => ({ $: 'succeed', val }),
    __Scheduler_fail: (val) => ({ $: 'fail', val }),
    __Scheduler_rawSpawn: (task) => { spawned.push(task); },
    __Platform_sendToSelf: F2((router, msg) => ({ $: 'sendToSelf', router, msg })),
    __Utils_Tuple2: F2((a, b) => ({ $: 'Tuple2', a, b })),
    __Websocket_connection: F2((id, url) => ({ $: 'Connection', a: id, b: url })),
    __Websocket_closedEvent: F2((code, reason) => ({ $: 'ClosedEvent', code, reason })),
    __Websocket_dataEvent: (data) => ({ $: 'DataEvent', data }),
    __Websocket_connectionClosed: { $: 'ConnectionClosed' },
  };
}

// --- Mock WebSocket ---

class MockWebSocket {
  constructor(url) {
    this.url = url;
    this._listeners = {};
    this.readyState = 1;
    this.isClosed = false;
    this._sent = [];
  }
  addEventListener(event, fn) {
    if (!this._listeners[event]) this._listeners[event] = [];
    this._listeners[event].push(fn);
  }
  send(data) { this._sent.push(data); }
  close() { this.readyState = 3; }
  _emit(event, data) {
    (this._listeners[event] || []).forEach(fn => fn(data));
  }
}

// --- Build kernel functions with stubs injected ---

function loadKernel(stubs) {
  const origLog = console.log;
  const logs = [];
  console.log = (...args) => { logs.push(args.join(' ')); };

  const {
    __Scheduler_binding, __Scheduler_succeed, __Scheduler_fail,
    __Scheduler_rawSpawn, __Platform_sendToSelf, __Utils_Tuple2,
    __Websocket_connection, __Websocket_closedEvent, __Websocket_dataEvent,
    __Websocket_connectionClosed,
  } = stubs;

  const _LamderaWebsocket_websockets = {};

  const _LamderaWebsocket_uuid4 = () =>
    'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, c => {
      const r = Math.random() * 16 | 0;
      return (c == 'x' ? r : (r & 0x3 | 0x8)).toString(16);
    });

  const _LamderaWebsocket_createHandle = F2(function(router, url) {
    return __Scheduler_binding(function(callback) {
      try {
        var id = _LamderaWebsocket_uuid4();
        _LamderaWebsocket_websockets[id] =
          { websocket: null, hasListener: false, isClosed: false };
        callback(__Scheduler_succeed(
          A2(__Websocket_connection, id, url)
        ));
      } catch (error) {
        logs.push("createHandle error: " + error);
      }
    });
  });

  const _LamderaWebsocket_listen = F2(function(router, connection) {
    return __Scheduler_binding(function(callback) {
      try {
        var websocketData = _LamderaWebsocket_websockets[connection.a];

        function addListener() {
          websocketData.hasListener = true;

          websocketData.websocket.addEventListener('close', function (event) {
            try {
              websocketData.websocket.isClosed = true;
              __Scheduler_rawSpawn(A2(__Platform_sendToSelf, router, A2(__Utils_Tuple2,
                connection,
                A2(__Websocket_closedEvent, event.code, event.reason)
              )));
            } catch (error) {
              logs.push("listen close error: " + error);
            }
          });

          websocketData.websocket.addEventListener('error', function (event) {
            try {
              websocketData.websocket.isClosed = true;
              __Scheduler_rawSpawn(A2(__Platform_sendToSelf, router, A2(__Utils_Tuple2,
                connection,
                A2(__Websocket_closedEvent, event.code || 1006, event.reason || "")
              )));
            } catch (error) {
              logs.push("listen error error: " + error);
            }
          });

          websocketData.websocket.addEventListener('message', function (event) {
            try {
              __Scheduler_rawSpawn(A2(__Platform_sendToSelf, router, A2(__Utils_Tuple2,
                connection,
                __Websocket_dataEvent(event.data)
              )));
            } catch (error) {
              logs.push("listen message error: " + error);
            }
          });
        }

        if (websocketData) {
          if (websocketData.isClosed === false) {
            if (websocketData.websocket) {
              if (websocketData.hasListener === false) {
                addListener();
              }
            } else {
              websocketData.websocket = new MockWebSocket(connection.b);
              addListener();
            }
          }
        } else {
          _LamderaWebsocket_websockets[connection.a] =
            { websocket: null, hasListener: false, isClosed: true };
          __Scheduler_rawSpawn(A2(__Platform_sendToSelf, router, A2(__Utils_Tuple2,
            connection,
            A2(__Websocket_closedEvent, 1005, "")
          )));
        }
      } catch (error) {
        logs.push("listen error: " + error);
      }
    });
  });

  const _LamderaWebsocket_close = F2(function(router, connection) {
    return __Scheduler_binding(function(callback) {
      try {
        var websocketData = _LamderaWebsocket_websockets[connection.a];
        if (websocketData) {
          websocketData.websocket.close();
          websocketData.isClosed = true;
          callback(__Scheduler_succeed(0));
        } else {
          callback(__Scheduler_succeed(0));
        }
      } catch (error) {
        logs.push("close error: " + error);
      }
    });
  });

  console.log = origLog;

  return {
    websockets: _LamderaWebsocket_websockets,
    createHandle: _LamderaWebsocket_createHandle,
    listen: _LamderaWebsocket_listen,
    close: _LamderaWebsocket_close,
    logs,
  };
}

// Helper: create a handle and return connection
function createConnection(stubs, kernel) {
  const router = {};
  A2(kernel.createHandle, router, 'ws://localhost:8080');
  let result;
  stubs.callbacks[stubs.callbacks.length - 1]((val) => { result = val; });
  return result.val;
}

// Helper: listen on a connection and return the mock websocket
function listenOn(stubs, kernel, connection) {
  const router = {};
  A2(kernel.listen, router, connection);
  stubs.callbacks[stubs.callbacks.length - 1](() => {});
  return kernel.websockets[connection.a].websocket;
}

// --- Tests ---

describe('LamderaWebsocket Kernel', () => {
  let stubs, kernel;

  beforeEach(() => {
    stubs = makeStubs();
    kernel = loadKernel(stubs);
  });

  describe('createHandle', () => {
    it('creates a handle with unique id and url', () => {
      const conn = createConnection(stubs, kernel);
      assert.equal(conn.$, 'Connection');
      assert.equal(conn.b, 'ws://localhost:8080');
      assert.ok(conn.a.length > 0);
    });

    it('registers handle in websockets registry', () => {
      const conn = createConnection(stubs, kernel);
      const entry = kernel.websockets[conn.a];
      assert.ok(entry);
      assert.equal(entry.isClosed, false);
      assert.equal(entry.hasListener, false);
      assert.equal(entry.websocket, null);
    });
  });

  describe('listen', () => {
    it('creates websocket and adds listeners', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);
      assert.ok(ws instanceof MockWebSocket);
      assert.ok(kernel.websockets[conn.a].hasListener);
    });

    it('handles close event with code and reason', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('close', { code: 1000, reason: 'Normal closure' });

      assert.equal(stubs.spawned.length, 1);
      const inner = stubs.spawned[0].msg;
      assert.equal(inner.b.$, 'ClosedEvent');
      assert.equal(inner.b.code, 1000);
      assert.equal(inner.b.reason, 'Normal closure');
    });

    it('handles browser-style error event (no code/reason)', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('error', { type: 'error' });

      assert.equal(stubs.spawned.length, 1);
      const inner = stubs.spawned[0].msg;
      assert.equal(inner.b.$, 'ClosedEvent');
      assert.equal(inner.b.code, 1006);
      assert.equal(inner.b.reason, '');
      assert.equal(kernel.logs.length, 0, 'should not have logged any exceptions');
    });

    it('handles Node.js ws-style error event (Error object)', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('error', new Error('ECONNRESET'));

      assert.equal(stubs.spawned.length, 1);
      const inner = stubs.spawned[0].msg;
      assert.equal(inner.b.$, 'ClosedEvent');
      assert.equal(inner.b.code, 1006);
      assert.equal(inner.b.reason, '');
      assert.equal(kernel.logs.length, 0, 'should not have logged any exceptions');
    });

    it('marks connection as closed on error', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('error', { type: 'error' });
      assert.equal(ws.isClosed, true);
    });

    it('handles message events', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('message', { data: 'hello world' });

      assert.equal(stubs.spawned.length, 1);
      const inner = stubs.spawned[0].msg;
      assert.equal(inner.b.$, 'DataEvent');
      assert.equal(inner.b.data, 'hello world');
    });

    it('sends 1005 closedEvent for unknown connection (reset)', () => {
      const fakeConn = { a: 'nonexistent-id', b: 'ws://localhost' };
      const router = {};
      A2(kernel.listen, router, fakeConn);
      stubs.callbacks[stubs.callbacks.length - 1](() => {});

      assert.equal(stubs.spawned.length, 1);
      const inner = stubs.spawned[0].msg;
      assert.equal(inner.b.$, 'ClosedEvent');
      assert.equal(inner.b.code, 1005);
      assert.equal(inner.b.reason, '');
    });

    it('does not add duplicate listeners', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      // Listen again
      const router = {};
      A2(kernel.listen, router, conn);
      stubs.callbacks[stubs.callbacks.length - 1](() => {});

      ws._emit('message', { data: 'test' });
      assert.equal(stubs.spawned.length, 1);
    });
  });

  describe('close', () => {
    it('closes an open connection', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);
      assert.equal(ws.readyState, 1);

      const router = {};
      A2(kernel.close, router, conn);
      let result;
      stubs.callbacks[stubs.callbacks.length - 1]((val) => { result = val; });

      assert.equal(ws.readyState, 3);
      assert.equal(kernel.websockets[conn.a].isClosed, true);
      assert.equal(result.$, 'succeed');
    });

    it('succeeds for unknown connection', () => {
      const fakeConn = { a: 'nonexistent', b: 'ws://localhost' };
      const router = {};
      A2(kernel.close, router, fakeConn);
      let result;
      stubs.callbacks[stubs.callbacks.length - 1]((val) => { result = val; });

      assert.equal(result.$, 'succeed');
    });
  });

  describe('error event edge cases', () => {
    it('preserves code/reason if error event has them', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('error', { code: 1001, reason: 'Going away' });

      const inner = stubs.spawned[0].msg;
      assert.equal(inner.b.code, 1001);
      assert.equal(inner.b.reason, 'Going away');
    });

    it('handles undefined event properties gracefully', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('error', {});

      const inner = stubs.spawned[0].msg;
      assert.equal(inner.b.code, 1006);
      assert.equal(inner.b.reason, '');
      assert.equal(kernel.logs.length, 0, 'should not throw');
    });
  });
});
