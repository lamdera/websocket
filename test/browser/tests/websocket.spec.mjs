import { test as base, expect } from '@playwright/test';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { scenarios, sequence, probeProblems, expectedFor, sequenceToCompare } from '../scenarios.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const strict = !!process.env.STRICT;

const test = base.extend({
  wsVersion: ['current', { option: true }],
  wsBundle: ['current', { option: true }],
  wsPort: [9753, { option: true }],
});

let tidCounter = 0;

for (const scenario of scenarios) {
  test(scenario.name, async ({ page, browserName, wsVersion, wsBundle, wsPort }, testInfo) => {
    const tid = `${browserName}-${wsBundle}-${process.pid}-${tidCounter++}-${Date.now()}`;
    const origin = `http://127.0.0.1:${wsPort}`;
    const config = await (await fetch(`${origin}/config`)).json();

    const pageErrors = [];
    const kernelExceptions = [];
    const consoleLines = [];
    page.on('pageerror', (err) => pageErrors.push(String(err.message || err)));
    page.on('console', (msg) => {
      const text = msg.text();
      consoleLines.push(`${msg.type()}: ${text}`);
      if (text.includes('Websocket: Exception')) kernelExceptions.push(text);
    });

    await page.goto(`/?v=${encodeURIComponent(wsBundle)}`);
    await page.waitForFunction(() => window.__send || window.__loadError);
    expect(await page.evaluate(() => window.__loadError || null)).toBeNull();

    let extra = {};
    const events = () => page.evaluate(() => window.__events);
    const d = {
      ws: (p, params = {}) => {
        const q = new URLSearchParams({ ...params, tid });
        return `ws://127.0.0.1:${wsPort}${p}?${q}`;
      },
      refusedUrl: `ws://127.0.0.1:${config.refusedPort}/`,
      tlsUrl: `wss://127.0.0.1:${wsPort}/echo?tid=${tid}`,
      dnsUrl: 'ws://lamdera-websocket-test.invalid/',
      cmd: (...cmds) => page.evaluate((cs) => cs.forEach((c) => window.__send(c)), cmds),
      settle: (ms) => page.waitForTimeout(ms),
      async until(pred, ms) {
        const end = Date.now() + ms;
        while (Date.now() < end) {
          if (pred(sequence(await events()))) return true;
          await page.waitForTimeout(25);
        }
        return false;
      },
      stats: async () => (await fetch(`${origin}/stats?tid=${encodeURIComponent(tid)}`)).json(),
      extra: (x) => Object.assign(extra, x),
    };

    await scenario.run(d);

    // Liveness: a dead Elm scheduler never answers.
    await d.cmd({ cmd: 'ping' });
    const alive = await page
      .waitForFunction(() => window.__events.some((e) => e.ev === 'pong'), null, { timeout: 1000 })
      .then(() => true, () => false);

    const evs = await events();
    const seq = sequence(evs);
    const problems = probeProblems(evs);
    if (!alive) problems.push('the app did not answer a ping: its scheduler is dead');
    const crashed = pageErrors.length > 0 || problems.length > 0;

    const result = {
      scenario: scenario.name,
      version: wsVersion,
      bundle: wsBundle,
      platform: browserName,
      sequence: seq,
      crashed,
      pageErrors,
      probeProblems: problems,
      kernelExceptions,
      extra,
      console: consoleLines,
    };
    const slug = scenario.name.replace(/[^a-z0-9]+/gi, '-').toLowerCase();
    const out = path.join(here, '..', 'results', browserName, wsBundle, `${slug}.json`);
    fs.mkdirSync(path.dirname(out), { recursive: true });
    fs.writeFileSync(out, JSON.stringify(result, null, 2));
    await testInfo.attach('result', { body: JSON.stringify(result, null, 2), contentType: 'application/json' });

    const expected = expectedFor(scenario, wsVersion, browserName, strict);
    expect(seq, 'Elm message sequence').toEqual(sequenceToCompare(expected.sequence, seq));
    if (expected.extra) expect(extra, 'server-side view').toEqual(expected.extra);
    expect(crashed, `Elm runtime crash: ${JSON.stringify({ pageErrors, problems })}`).toBe(!!expected.crashes);
    if (strict || wsVersion === 'current') expect(kernelExceptions, 'exceptions swallowed by the kernel').toEqual([]);
  });
}
