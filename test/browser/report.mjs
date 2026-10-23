// Tabulates results/<platform>/<bundle>/<scenario>.json (written by every
// browser and Node test run) as Markdown: per scenario, what each version
// delivered on each platform. Identical cells are merged so differences stand out.
//
//   node report.mjs [results-dir]

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { scenarios } from './scenarios.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const dir = process.argv[2] || path.join(here, 'results');

const rows = [];
for (const platform of fs.existsSync(dir) ? fs.readdirSync(dir) : []) {
  for (const bundle of fs.readdirSync(path.join(dir, platform))) {
    for (const file of fs.readdirSync(path.join(dir, platform, bundle))) {
      rows.push(JSON.parse(fs.readFileSync(path.join(dir, platform, bundle, file), 'utf8')));
    }
  }
}

const platforms = ['chromium', 'firefox', 'webkit', 'node'].filter((p) => rows.some((r) => r.platform === p));
const bundles = [...new Set(rows.map((r) => r.bundle))].sort((a, b) => order(a) - order(b));

function order(b) {
  const i = ['served', 'master', 'pr2', 'current'].indexOf(b.split('.')[0]);
  return i * 10 + (b.includes('.') ? 1 : 0);
}

// Drops the leading "created 0" and compresses runs of data messages.
function cell(r) {
  if (!r) return '';
  let seq = r.sequence.filter((s, i) => !(i === 0 && s === 'created 0'));
  const out = [];
  for (let i = 0; i < seq.length; i++) {
    const m = /^data 0 "m(\d+)"$/.exec(seq[i]);
    if (m) {
      let j = i;
      while (j + 1 < seq.length && /^data 0 "m\d+"$/.test(seq[j + 1])) j++;
      if (j > i) {
        out.push(`data m${m[1]}..${/m(\d+)/.exec(seq[j])[1]}`);
        i = j;
        continue;
      }
    }
    out.push(seq[i]);
  }
  let text = out.join(' / ') || '(nothing)';
  if (r.crashed) text += ' **CRASH**';
  if (r.kernelExceptions && r.kernelExceptions.length) text += ` (${r.kernelExceptions.length} swallowed exception${r.kernelExceptions.length > 1 ? 's' : ''})`;
  if (r.extra && Object.keys(r.extra).length) text += ` ${JSON.stringify(r.extra)}`;
  return text.replace(/\|/g, '\\|');
}

const names = scenarios.map((s) => s.name).filter((n) => rows.some((r) => r.scenario === n));
let md = '';
for (const name of names) {
  md += `\n### ${name}\n\n| version | ${platforms.join(' | ')} |\n|---|${platforms.map(() => '---').join('|')}|\n`;
  for (const b of bundles) {
    const cells = platforms.map((p) => cell(rows.find((r) => r.scenario === name && r.platform === p && r.bundle === b)));
    if (cells.every((c) => c === '')) continue;
    md += `| ${b} | ${cells.join(' | ')} |\n`;
  }
}
process.stdout.write(md);
