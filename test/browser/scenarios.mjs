// Scenarios shared by the browser (Playwright) and Node (backend) runners.
//
// Each scenario drives the harness app through a driver `d`:
//
//   d.cmd(...cmds)       send commands to the app's `command` port, all in the
//                        same JS task (so nothing else runs in between)
//   d.until(pred, ms)    wait until pred(seq) is true, or ms passes (no throw)
//   d.settle(ms)         wait, to catch late or duplicate messages
//   d.stats()            the server's view of this test's connections
//   d.ws(path, params)   a ws:// URL on the test server, tagged with the test id
//   d.refusedUrl, d.tlsUrl, d.dnsUrl
//   d.extra(obj)         record extra facts (server-side counts) to compare
//
// and the runner then compares the app's message sequence with `expect`.
//
// `expect` is the sequence the package should produce. Versions that do
// something else have it pinned in `known`, keyed by version, or by
// "version@platform" when it differs per platform (chromium, firefox, webkit
// or node). Keys may list several, comma separated, and `known` may also be a
// function (version, platform) => sequence | undefined. A pinned sequence may
// be { anyOf: [sequence, ...] } where a version's behaviour is racy. `crashes`, `expectExtra`
// and `knownExtra` work the same way. The pinned sequences are what each
// version actually does, as observed by these tests, so the default run
// characterises every version, and any change in what one of them does fails
// the run. With STRICT=1 the runner ignores the pins and holds every version
// to `expect`, which is how the old versions' bugs show up as failures.

// The undefined reason an error event gives master's close message.
const UNDEFINED = { raw: '<undefined>' };
const show = (v) => (v && v.raw ? v.raw : JSON.stringify(v));

const created = (h = 0) => `created ${h}`;
const sent = (value, result = 'Ok', h = 0) => `sent ${h} ${JSON.stringify(value)} ${result}`;
const data = (value, h = 0) => `data ${h} ${show(value)}`;
const closed = (code, reason = '', h = 0) => `closed ${h} ${code} ${show(reason)}`;
const closeDone = (h = 0) => `closeDone ${h}`;
const welcome = (h = 0) => data('welcome', h);

// Formats one event from the app's `event` port. Probe events are not part of
// the sequence: the runner checks them separately (see probeProblems).
export function format(e) {
  const val = (v) => (typeof v === 'string' ? JSON.stringify(v) : v && v.nonString ? `<${v.nonString}>` : `<${typeof v}>`);
  switch (e.ev) {
    case 'created':
      return `created ${e.h}`;
    case 'data':
      return `data ${e.h} ${val(e.data)}`;
    case 'closed':
      return `closed ${e.h} ${e.code} ${val(e.reason)}`;
    case 'sent':
      return `sent ${e.h} ${val(e.data)} ${e.result}`;
    case 'closeDone':
      return `closeDone ${e.h}`;
    default:
      return `${e.ev} ${JSON.stringify(e)}`;
  }
}

export function sequence(events) {
  return events.filter((e) => !['ready', 'dataProbe', 'closeProbe', 'pong'].includes(e.ev)).map(format);
}

// Every data and close message is followed by a probe message that runs
// String functions on what the kernel handed Elm. A missing probe means that
// probe threw, which kills the Elm scheduler.
export function probeProblems(events) {
  const problems = [];
  const n = (ev) => events.filter((e) => e.ev === ev).length;
  if (n('dataProbe') !== n('data')) problems.push(`${n('data')} data messages but ${n('dataProbe')} data probes`);
  if (n('closeProbe') !== n('closed')) problems.push(`${n('closed')} close messages but ${n('closeProbe')} close probes`);
  return problems;
}

const count = (seq, prefix) => seq.filter((s) => s.startsWith(prefix)).length;
const has = (seq, s) => seq.includes(s);

// Waits until handle h has received the server's "welcome", so the client
// side is OPEN. (The server sees the connection before the client gets its
// open event, so asking the server is not enough: sending then races.)
const welcomed = (d, h = 0) => d.until((s) => has(s, welcome(h)), 3000);

const SETTLE = 400;

const OLD = 'served,master,pr2';
const BROWSERS = ['chromium', 'firefox', 'webkit'];
const onBrowsers = (versions) => versions.flatMap((v) => BROWSERS.map((b) => `${v}@${b}`)).join(',');

// A connection that fails fires `error` and then `close`, in every browser and
// in node's ws. master turns the error into a close message whose code and
// reason are undefined: the first String function on the reason throws, and
// as that happens inside the kernel's listener, its try/catch swallows it and
// leaves the Elm scheduler dead. pr2 turns the error into a second close
// message, 1006 "".
//
// On node, served has no `error` listener at all on a socket `listen` opened,
// and ws throws an error event nobody listens to: that escapes to the process
// (the Lamdera runtime logs it to Bugsnag and carries on), and for most
// failures ws then never fires `close` either, so the app is never told the
// connection failed. `servedOnNode` is what served delivers there.
function errorThenClose(before, { servedOnNode = before } = {}) {
  return (version, platform) => {
    if (platform === 'node' && version === 'served') return servedOnNode;
    // master on node dies at a racy point: before or after the app has
    // handled the close message with the undefined reason.
    if (platform === 'node' && version === 'master') {
      return { anyOf: [[], [...before], [...before, closed('UnknownCode undefined', UNDEFINED)]] };
    }
    if (version === 'master') return [...before, closed('UnknownCode undefined', UNDEFINED)];
    if (version === 'pr2') return [...before, closed('AbnormalClosure'), closed('AbnormalClosure')];
    return undefined;
  };
}
const masterCrashes = (version) => (version === 'master' ? true : undefined);
// served on node: the uncaught error event counts as a crash.
const masterOrNodeServedCrashes = (version, platform) => (version === 'master' || (version === 'served' && platform === 'node') ? true : undefined);

export const scenarios = [
  // --- Sending ----------------------------------------------------------------

  {
    name: 'echo: send after the connection is open',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/echo', { greet: 1 }), listen: true });
      await welcomed(d);
      await d.cmd({ cmd: 'send', h: 0, data: 'hello' });
      await d.until((s) => has(s, data('hello')) && count(s, 'sent') > 0, 2000);
      await d.settle(SETTLE);
    },
    expect: [created(), welcome(), sent('hello'), data('hello')],
    known: {
      // sendString on an open socket sends, but never calls back: the task
      // never completes, so the app never gets its result message.
      [OLD]: [created(), welcome(), data('hello')],
    },
  },

  {
    name: 'send while connecting (listen opened the socket)',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/echo'), listen: true }, { cmd: 'send', h: 0, data: 'early' });
      await d.until((s) => has(s, data('early')) && count(s, 'sent') > 0, 2000);
      await d.settle(SETTLE);
    },
    expect: [created(), sent('early'), data('early')],
    known: {
      // ws.send() on a CONNECTING socket throws InvalidStateError; the kernel
      // swallows it, so the message is lost and the task never completes.
      [OLD]: [created()],
    },
  },

  {
    name: 'send first, then listen (sendString opened the socket)',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/echo') }, { cmd: 'send', h: 0, data: 'first' }, { cmd: 'listen', h: 0 });
      await d.until((s) => has(s, data('first')), 2000);
      await d.settle(SETTLE);
    },
    expect: [created(), sent('first'), data('first')],
  },

  {
    name: 'two sends while connecting (sendString opened the socket)',
    async run(d) {
      await d.cmd(
        { cmd: 'create', url: d.ws('/echo') },
        { cmd: 'send', h: 0, data: 'a' },
        { cmd: 'send', h: 0, data: 'b' },
        { cmd: 'listen', h: 0 },
      );
      await d.until((s) => has(s, data('b')), 2000);
      await d.settle(SETTLE);
    },
    expect: [created(), sent('a'), sent('b'), data('a'), data('b')],
    known: {
      // The second send hits the CONNECTING socket: lost, and no result.
      [OLD]: [created(), sent('a'), data('a')],
    },
  },

  {
    name: 'send after the server closed the connection',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/close', { code: 4000, reason: 'bye' }), listen: true });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.settle(100);
      await d.cmd({ cmd: 'send', h: 0, data: 'late' });
      await d.until((s) => count(s, 'sent') > 0, 1000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('UnknownCode 4000', 'bye'), sent('late', 'ConnectionClosed')],
    known: {
      // The close listener sets `isClosed` on the WebSocket object instead of
      // the handle, so the handle never counts as closed: the send goes to a
      // CLOSED socket, which drops it, and the task never completes.
      [OLD]: [created(), closed('UnknownCode 4000', 'bye')],
    },
  },

  {
    name: 'send after close',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/echo', { greet: 1 }), listen: true });
      await welcomed(d);
      await d.cmd({ cmd: 'close', h: 0 });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.cmd({ cmd: 'send', h: 0, data: 'late' });
      await d.until((s) => count(s, 'sent') > 0, 1000);
      await d.settle(SETTLE);
    },
    expect: [created(), welcome(), closeDone(), closed('NoStatusReceived'), sent('late', 'ConnectionClosed')],
    known: {
      // `websocketData || websocketData.isClosed` is true for every known
      // handle, so the send goes to the CLOSED socket and never completes.
      [OLD]: [created(), welcome(), closeDone(), closed('NoStatusReceived')],
    },
  },

  // --- Receiving --------------------------------------------------------------

  {
    name: 'burst of 50 messages then a normal close, in order',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/burst', { n: 50 }), listen: true });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.settle(SETTLE);
    },
    expect: [created(), ...Array.from({ length: 50 }, (_, i) => data(`m${i}`)), closed('NormalClosure', 'done')],
  },

  {
    name: 'binary frame',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/binary'), listen: true });
      await d.until((s) => has(s, data('after-binary')), 2000);
      await d.settle(SETTLE);
    },
    // The API only has String, so a binary frame is decoded as UTF-8 text
    // (here the bytes DE AD BE EF).
    expect: [created(), data('ޭ��'), data('after-binary')],
    known: {
      // The Blob (browser) or Buffer (node) itself is handed to Elm as a
      // String. The probe's String.toUpper throws, which kills the Elm
      // scheduler, so the next frame is never delivered.
      [onBrowsers(['served', 'master', 'pr2'])]: [created(), data({ raw: '<[object Blob]>' })],
      // On node the frame can arrive, and the scheduler die, before the app
      // has even reported its created message.
      'served@node,master@node,pr2@node': { anyOf: [[created(), data({ raw: '<[object Uint8Array]>' })], []] },
    },
    crashes: { [OLD]: true },
  },

  {
    name: 'unlisten stops delivery, listen again resumes it',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/echo', { greet: 1 }), listen: true });
      await welcomed(d);
      await d.cmd({ cmd: 'unlisten', h: 0 }, { cmd: 'send', h: 0, data: 'unheard' });
      await d.settle(300);
      await d.cmd({ cmd: 'listen', h: 0 }, { cmd: 'send', h: 0, data: 'heard' });
      await d.until((s) => has(s, data('heard')), 2000);
      await d.settle(SETTLE);
    },
    expect: [created(), welcome(), sent('unheard'), sent('heard'), data('heard')],
    known: {
      [OLD]: [created(), welcome(), data('heard')],
    },
  },

  {
    name: 'two handles to the same server are independent',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/echo', { greet: 1 }), listen: true });
      await welcomed(d, 0);
      await d.cmd({ cmd: 'create', url: d.ws('/echo', { greet: 1 }), listen: true });
      await welcomed(d, 1);
      await d.cmd({ cmd: 'close', h: 1 });
      await d.until((s) => count(s, 'closed') > 0, 2000);
      await d.cmd({ cmd: 'send', h: 0, data: 'still here' });
      await d.until((s) => has(s, data('still here')), 2000);
      await d.settle(SETTLE);
    },
    expect: [created(0), welcome(0), created(1), welcome(1), closeDone(1), closed('NoStatusReceived', '', 1), sent('still here'), data('still here')],
    known: {
      [OLD]: [created(0), welcome(0), created(1), welcome(1), closeDone(1), closed('NoStatusReceived', '', 1), data('still here')],
    },
  },

  // --- Closing: server side ---------------------------------------------------

  {
    name: 'server closes with 4000 "bye"',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/close', { code: 4000, reason: 'bye' }), listen: true });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('UnknownCode 4000', 'bye')],
  },

  {
    name: 'server closes with 1000 "ok"',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/close', { code: 1000, reason: 'ok' }), listen: true });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('NormalClosure', 'ok')],
  },

  {
    name: 'server closes with no status code',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/close'), listen: true });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('NoStatusReceived')],
  },

  {
    name: 'server closes with 1011 and a unicode reason',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/close', { code: 1011, reason: 'ünïcødé ✓' }), listen: true });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('InternalError', 'ünïcødé ✓')],
  },

  {
    name: 'server drops the TCP connection after open',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/drop', { after: 100 }), listen: true });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('AbnormalClosure')],
    // Only WebKit fires `error` before `close` for an abrupt drop, so only
    // there do master and pr2 misbehave. This is what a server restart or a
    // network change looks like to Safari users.
    known: (version, platform) => (platform === 'webkit' ? errorThenClose([created()])(version, platform) : undefined),
    crashes: (version, platform) => (platform === 'webkit' ? masterCrashes(version) : undefined),
  },

  // --- Failing to connect -----------------------------------------------------

  {
    name: 'connection refused',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.refusedUrl, listen: true });
      await d.until((s) => count(s, 'closed') > 0, 5000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('AbnormalClosure')],
    known: errorThenClose([created()]),
    crashes: masterOrNodeServedCrashes,
  },

  {
    name: 'handshake rejected with HTTP 403',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/reject', { status: 403 }), listen: true });
      await d.until((s) => count(s, 'closed') > 0, 5000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('AbnormalClosure')],
    known: errorThenClose([created()], { servedOnNode: [created(), closed('AbnormalClosure')] }),
    crashes: masterOrNodeServedCrashes,
  },

  {
    name: 'handshake hangs, then the server drops it',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/hang', { ms: 800 }), listen: true });
      await d.until((s) => count(s, 'closed') > 0, 5000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('AbnormalClosure')],
    known: errorThenClose([created()]),
    crashes: masterOrNodeServedCrashes,
  },

  {
    name: 'TLS handshake fails (wss:// to a plain port)',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.tlsUrl, listen: true });
      await d.until((s) => count(s, 'closed') > 0, 5000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('AbnormalClosure')],
    known: errorThenClose([created()]),
    crashes: masterOrNodeServedCrashes,
  },

  {
    name: 'DNS lookup fails',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.dnsUrl, listen: true });
      await d.until((s) => count(s, 'closed') > 0, 8000);
      await d.settle(SETTLE);
    },
    expect: [created(), closed('AbnormalClosure')],
    known: errorThenClose([created()]),
    crashes: masterOrNodeServedCrashes,
  },

  // --- Closing: client side ---------------------------------------------------

  {
    name: 'close after open',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/echo', { greet: 1 }), listen: true });
      await welcomed(d);
      await d.cmd({ cmd: 'close', h: 0 });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.settle(SETTLE);
      d.extra({ serverSaw: (await d.stats()).closed.map((c) => c.code) });
    },
    expect: [created(), welcome(), closeDone(), closed('NoStatusReceived')],
    expectExtra: { serverSaw: [1005] },
  },

  {
    name: 'close while connecting',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/hang', { ms: 3000 }), listen: true }, { cmd: 'close', h: 0 });
      await d.until((s) => count(s, 'closed') > 0, 3000);
      await d.settle(SETTLE);
    },
    expect: [created(), closeDone(), closed('AbnormalClosure')],
    known: (version, platform) => {
      // Chromium and WebKit fire `error` synchronously inside close(), so
      // master's crash happens around the time close() returns (racy:
      // sometimes the close task's message still gets out). ws on node does
      // too, so served's close() throws there (the kernel swallows it, and
      // the close task never completes), and master dies before the app has
      // even handled its created message.
      if (version === 'master' && platform === 'node') return { anyOf: [[], [created()]] };
      if (version === 'master' && platform !== 'firefox') return { anyOf: [[created()], [created(), closeDone()]] };
      if (version === 'served' && platform === 'node') return [created(), closed('AbnormalClosure')];
      return errorThenClose([created(), closeDone()])(version, platform);
    },
    crashes: masterCrashes,
  },

  {
    name: 'close a handle that was never opened',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/echo') }, { cmd: 'close', h: 0 });
      await d.until((s) => count(s, 'closeDone') > 0, 1000);
      await d.settle(SETTLE);
    },
    expect: [created(), closeDone()],
    known: {
      // websocket is null: close() throws, the kernel swallows it, and the
      // task never completes.
      [OLD]: [created()],
    },
  },

  {
    name: 'listen to a closed handle opens nothing',
    async run(d) {
      await d.cmd({ cmd: 'create', url: d.ws('/echo') }, { cmd: 'close', h: 0 });
      await d.settle(100);
      await d.cmd({ cmd: 'listen', h: 0 });
      await d.settle(SETTLE);
      d.extra({ serverConnections: (await d.stats()).upgrades });
    },
    expect: [created(), closeDone()],
    expectExtra: { serverConnections: 0 },
    known: {
      // The close threw before marking the handle closed, so listen then
      // opens a connection the app thinks it closed.
      [OLD]: [created()],
    },
    knownExtra: { [OLD]: { serverConnections: 1 } },
  },

  // --- An app that reconnects on close (the pattern in examples/echo) ---------

  {
    name: 'reconnect on close leaves exactly one live connection',
    async run(d) {
      await d.cmd({ cmd: 'reconnect', on: true }, { cmd: 'create', url: d.ws('/drop', { after: 150, once: 1 }), listen: true });
      await d.until((s) => count(s, 'created') >= 2, 3000);
      await d.settle(800);
      const st = await d.stats();
      d.extra({ serverOpened: st.opened, serverOpen: st.open });
    },
    // The app stops listening to the old handle when it reconnects, so even
    // pr2's second close message for it is dropped before it reaches the app.
    expect: [created(0), closed('AbnormalClosure'), created(1)],
    expectExtra: { serverOpened: 2, serverOpen: 1 },
    known: (version, platform) => (version === 'master' && platform === 'webkit' ? [created(0), closed('UnknownCode undefined', UNDEFINED)] : undefined),
    knownExtra: (version, platform) => (version === 'master' && platform === 'webkit' ? { serverOpened: 1, serverOpen: 0 } : undefined),
    crashes: (version, platform) => (platform === 'webkit' ? masterCrashes(version) : undefined),
  },
];

// The pinned sequence to compare an actual sequence with: for { anyOf }, the
// alternative it equals, if any.
export function sequenceToCompare(expected, actual) {
  if (!expected || !expected.anyOf) return expected;
  return expected.anyOf.find((alt) => JSON.stringify(alt) === JSON.stringify(actual)) || expected.anyOf[0];
}

// Looks up what a version is expected to produce on a platform.
export function expectedFor(scenario, version, platform, strict) {
  const pick = (table, fallback) => {
    if (strict || !table) return fallback;
    if (typeof table === 'function') return table(version, platform) ?? fallback;
    for (const key of [`${version}@${platform}`, version]) {
      for (const [k, v] of Object.entries(table)) {
        if (k.split(',').map((x) => x.trim()).includes(key)) return v;
      }
    }
    return fallback;
  };
  return {
    sequence: pick(scenario.known, scenario.expect),
    extra: pick(scenario.knownExtra, scenario.expectExtra),
    crashes: pick(scenario.crashes, false),
  };
}
