// Runs the same scenarios under Node, the way a Lamdera backend runs the
// package: the Lamdera runtime (runtime/backend.js) sets
//
//   global.WebSocket = require('isomorphic-ws')   // which is `ws` 7 on node
//
// so the kernel's `new WebSocket(url)` gets a `ws` client, whose
// addEventListener shim hands it ws's own Event classes.
//
//   node node/backend.mjs     (after `node build.mjs`; exits 1 on any failure)
//
// The runtime installs process-wide uncaughtException and unhandledRejection
// handlers that log and carry on, so an exception escaping the kernel does not
// take a backend down; it does still abort whatever ws was doing when it threw.
// This runner does the same, records them, and counts them as a crash. (It is
// a plain script rather than a node:test file because node:test claims
// uncaught exceptions for itself.)

import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import WebSocket from 'ws';
import { startServer } from '../server.mjs';
import { scenarios, sequence, probeProblems, expectedFor, sequenceToCompare } from '../scenarios.mjs';
import { selectedVersions } from '../versions.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const strict = !!process.env.STRICT;

globalThis.WebSocket = WebSocket;

const server = await startServer();

const uncaught = [];
process.on('uncaughtException', (err) => uncaught.push(String(err && err.message ? err.message : err)));
process.on('unhandledRejection', (err) => uncaught.push(String(err && err.message ? err.message : err)));

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Each test gets a fresh copy of the bundle, so the kernel's handle registry
// starts empty.
function boot(version) {
  const code = fs.readFileSync(path.join(here, '..', 'dist', `${version}.worker.js`), 'utf8');
  const scope = {};
  new Function(code).call(scope);
  const app = scope.Elm.Worker.init();
  const events = [];
  app.ports.event.subscribe((e) => {
    const copy = {};
    for (const k in e) {
      const v = e[k];
      copy[k] = (k === 'data' || k === 'reason') && typeof v !== 'string'
        ? { nonString: v === undefined ? 'undefined' : Object.prototype.toString.call(v) }
        : v;
    }
    events.push(copy);
  });
  return { app, events };
}

let tidCounter = 0;
const failures = [];
let passed = 0;

for (const version of selectedVersions()) {
  for (const scenario of scenarios) {
    const title = `[node-${version}] ${scenario.name}`;
    try {
      const tid = `node-${version}-${tidCounter++}-${Date.now()}`;

      const kernelExceptions = [];
      const consoleLines = [];
      const origLog = console.log;
      console.log = (...args) => {
        const text = args.map((a) => (typeof a === 'string' ? a : (a && a.constructor && a.constructor.name) || String(a))).join(' ');
        consoleLines.push(text);
        if (text.includes('Websocket: Exception')) kernelExceptions.push(text);
      };
      uncaught.length = 0;

      let extra = {};
      let result;
      let app;
      try {
        let events;
        ({ app, events } = boot(version));
        const d = {
          ws: (p, params = {}) => `ws://127.0.0.1:${server.port}${p}?${new URLSearchParams({ ...params, tid })}`,
          refusedUrl: `ws://127.0.0.1:${server.refusedPort}/`,
          tlsUrl: `wss://127.0.0.1:${server.port}/echo?tid=${tid}`,
          dnsUrl: 'ws://lamdera-websocket-test.invalid/',
          cmd: async (...cmds) => {
            // An Elm crash can throw straight out of a port send; the runtime's
            // process-wide handler would absorb it, so record it the same way.
            try {
              cmds.forEach((c) => app.ports.command.send(c));
            } catch (err) {
              uncaught.push(String(err && err.message ? err.message : err));
            }
          },
          settle: sleep,
          async until(pred, ms) {
            const end = Date.now() + ms;
            while (Date.now() < end) {
              if (pred(sequence(events))) return true;
              await sleep(20);
            }
            return false;
          },
          stats: async () => JSON.parse(JSON.stringify(server.stats(tid))),
          extra: (x) => Object.assign(extra, x),
        };

        await scenario.run(d);

        await d.cmd({ cmd: 'ping' });
        await d.until(() => events.some((e) => e.ev === 'pong'), 1000);
        const alive = events.some((e) => e.ev === 'pong');

        const problems = probeProblems(events);
        if (!alive) problems.push('the app did not answer a ping: its scheduler is dead');
        result = {
          scenario: scenario.name,
          version,
          bundle: version,
          platform: 'node',
          sequence: sequence(events),
          crashed: uncaught.length > 0 || problems.length > 0,
          pageErrors: [...uncaught],
          probeProblems: problems,
          kernelExceptions,
          extra,
          console: consoleLines,
        };
      } finally {
        if (app) app.ports.command.send({ cmd: 'shutdown' });
        await sleep(50);
        console.log = origLog;
      }

      const slug = scenario.name.replace(/[^a-z0-9]+/gi, '-').toLowerCase();
      const out = path.join(here, '..', 'results', 'node', version, `${slug}.json`);
      fs.mkdirSync(path.dirname(out), { recursive: true });
      fs.writeFileSync(out, JSON.stringify(result, null, 2));

      const expected = expectedFor(scenario, version, 'node', strict);
      assert.deepEqual(result.sequence, sequenceToCompare(expected.sequence, result.sequence), 'Elm message sequence');
      if (expected.extra) assert.deepEqual(result.extra, expected.extra, 'server-side view');
      assert.equal(result.crashed, !!expected.crashes, `Elm runtime crash: ${JSON.stringify({ uncaught: result.pageErrors, problems: result.probeProblems })}`);
      if (strict || version === 'current') assert.deepEqual(kernelExceptions, [], 'exceptions swallowed by the kernel');
      passed++;
      console.log(`  ok  ${title}`);
    } catch (err) {
      failures.push(title);
      console.log(`  FAIL ${title}\n${String(err.message).replace(/^/gm, '       ')}`);
    }
  }
}

await server.close();
console.log(`\n${passed} passed, ${failures.length} failed`);
process.exit(failures.length ? 1 : 0);
