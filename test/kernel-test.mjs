import { describe, it, beforeEach } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';

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

// --- Load the real kernel file with stubs injected ---
//
// src/Elm/Kernel/LamderaWebsocket.js is evaluated as is, with the Elm kernel
// names it uses (__Scheduler_binding and friends), WebSocket and console passed
// in as parameters, so these tests exercise the code the compiler ships rather
// than a copy of it.

const kernelSource = readFileSync(new URL('../src/Elm/Kernel/LamderaWebsocket.js', import.meta.url), 'utf8');

function loadKernel(stubs, WebSocketImpl = MockWebSocket) {
  const logs = [];
  const fakeConsole = {
    log: (...args) => {
      const text = args.map(String).join(' ');
      // Only the kernel's "Exception in ..." lines: those are swallowed errors.
      if (text.includes('Exception')) logs.push(text);
    },
  };

  const names = Object.keys(stubs).filter((k) => k.startsWith('__'));
  const factory = new Function(
    'F2', 'F3', 'A2', 'A3', 'WebSocket', 'console', ...names,
    kernelSource + `
    return {
      websockets: _LamderaWebsocket_websockets,
      createHandle: _LamderaWebsocket_createHandle,
      sendString: _LamderaWebsocket_sendString,
      listen: _LamderaWebsocket_listen,
      close: _LamderaWebsocket_close,
    };`,
  );

  return { ...factory(F2, F3, A2, A3, WebSocketImpl, fakeConsole, ...names.map((n) => stubs[n])), logs };
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

    // A failed connection fires `error` and then `close` (the HTML spec, and
    // node's ws). The app must get exactly one close message, from `close`.
    it('handles browser-style error event (no code/reason)', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('error', { type: 'error' });
      assert.equal(stubs.spawned.length, 0, 'the error event alone sends nothing');

      ws._emit('close', { code: 1006, reason: '' });

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
      ws._emit('close', { code: 1006, reason: '' });

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
      assert.equal(kernel.websockets[conn.a].isClosed, true);
    });

    it('marks connection as closed on close', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('close', { code: 1000, reason: '' });
      assert.equal(kernel.websockets[conn.a].isClosed, true);
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

    it('succeeds for a handle that was never opened, and keeps it closed', () => {
      const conn = createConnection(stubs, kernel);
      A2(kernel.close, {}, conn);
      let result;
      stubs.callbacks[stubs.callbacks.length - 1]((val) => { result = val; });

      assert.equal(result && result.$, 'succeed');
      assert.equal(kernel.logs.length, 0, 'should not throw');

      // Listening afterwards must not open a connection.
      A2(kernel.listen, {}, conn);
      stubs.callbacks[stubs.callbacks.length - 1](() => {});
      assert.equal(kernel.websockets[conn.a].websocket, null);
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

  describe('sendString', () => {
    function send(conn, data) {
      A3(kernel.sendString, {}, conn, data);
      let result;
      stubs.callbacks[stubs.callbacks.length - 1]((val) => { result = val; });
      return result;
    }

    it('sends on an open socket and completes', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      const result = send(conn, 'hello');
      assert.equal(result && result.$, 'succeed');
      assert.deepEqual(ws._sent, ['hello']);
    });

    it('queues sends while connecting and sends them in order once open', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);
      ws.readyState = 0;
      ws.send = function () { throw new Error('InvalidStateError'); };

      assert.equal(send(conn, 'a').$, 'succeed');
      assert.equal(send(conn, 'b').$, 'succeed');

      delete ws.send;
      ws.readyState = 1;
      ws._emit('open', {});
      assert.deepEqual(ws._sent, ['a', 'b']);
      assert.equal(kernel.logs.length, 0, 'should not throw');
    });

    it('fails with ConnectionClosed on a closing socket', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);
      ws.readyState = 2;

      const result = send(conn, 'late');
      assert.equal(result && result.$, 'fail');
      assert.deepEqual(ws._sent, []);
    });

    it('fails with ConnectionClosed after the connection closed', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);
      ws._emit('close', { code: 4000, reason: 'bye' });

      const result = send(conn, 'late');
      assert.equal(result && result.$, 'fail');
      assert.equal(result.val.$, 'ConnectionClosed');
      assert.deepEqual(ws._sent, []);
    });

    it('fails with ConnectionClosed after close', () => {
      const conn = createConnection(stubs, kernel);
      listenOn(stubs, kernel, conn);
      A2(kernel.close, {}, conn);
      stubs.callbacks[stubs.callbacks.length - 1](() => {});

      const result = send(conn, 'late');
      assert.equal(result && result.$, 'fail');
    });

    it('fails with ConnectionClosed for an unknown handle', () => {
      const result = send({ a: 'nonexistent', b: 'ws://localhost' }, 'x');
      assert.equal(result && result.$, 'fail');
      assert.equal(kernel.logs.length, 0, 'should not throw');
    });
  });

  describe('error event edge cases', () => {
    it('preserves code/reason if error event has them', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('error', { code: 1001, reason: 'Going away' });
      ws._emit('close', { code: 1001, reason: 'Going away' });

      assert.equal(stubs.spawned.length, 1);
      const inner = stubs.spawned[0].msg;
      assert.equal(inner.b.code, 1001);
      assert.equal(inner.b.reason, 'Going away');
    });

    it('handles undefined event properties gracefully', () => {
      const conn = createConnection(stubs, kernel);
      const ws = listenOn(stubs, kernel, conn);

      ws._emit('error', {});

      assert.equal(stubs.spawned.length, 0, 'the error event alone sends nothing');
      assert.equal(kernel.logs.length, 0, 'should not throw');
    });
  });
});
