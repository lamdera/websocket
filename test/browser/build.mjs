// Compiles the harness app once per lamdera/websocket version in versions.mjs:
//
//   dist/<version>.js         Browser.element (Main.elm), --optimize
//   dist/<version>.dev.js     Browser.element (Main.elm), dev mode (no --optimize)
//   dist/<version>.worker.js  Platform.worker (Worker.elm), --optimize, for Node
//
// Each version's package source is written into an ELM_HOME package cache as
// lamdera/websocket 1.0.0, exactly where the compiler would have unpacked the
// published zip, so the build goes through the compiler's real handling of
// Kernel code in lamdera/* packages.
//
//   LAMDERA=/path/to/lamdera   compiler binary (default: `lamdera` on PATH)
//   ELM_HOME=/path             package cache (default: test/browser/.elm-home)
//   WS_VERSIONS=served,current which versions to build (default: all)

import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { versions, selectedVersions } from './versions.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, '..', '..');
const appDir = path.join(here, 'app');
const dist = path.join(here, 'dist');
const lamdera = process.env.LAMDERA || 'lamdera';
const elmHome = path.resolve(process.env.ELM_HOME || path.join(here, '.elm-home'));

// The package files the compiler reads.
const packageFiles = ['elm.json', 'src/Websocket.elm', 'src/Elm/Kernel/LamderaWebsocket.js', 'LICENSE', 'README.md'];

function packageSource(ref, file) {
  if (ref === null) return fs.readFileSync(path.join(repo, file));
  return execFileSync('git', ['show', `${ref}:${file}`], { cwd: repo, maxBuffer: 1 << 26 });
}

// Different compiler builds read different trees (0.19.1, 0.19.2, 0.19.3), so
// install into all of them.
const trees = ['0.19.1', '0.19.2', '0.19.3'];

function installPackage(ref) {
  for (const tree of trees) {
    const dir = path.join(elmHome, tree, 'packages', 'lamdera', 'websocket', '1.0.0');
    fs.rmSync(dir, { recursive: true, force: true });
    for (const file of packageFiles) {
      const dest = path.join(dir, file);
      fs.mkdirSync(path.dirname(dest), { recursive: true });
      fs.writeFileSync(dest, packageSource(ref, file));
    }
  }
}

function compile(main, output, flags) {
  fs.rmSync(path.join(appDir, 'elm-stuff'), { recursive: true, force: true });
  execFileSync(lamdera, ['make', `src/${main}`, `--output=${output}`, ...flags], {
    cwd: appDir,
    env: { ...process.env, ELM_HOME: elmHome, LAMDERA_NO_TELEMETRY: '1' },
    stdio: ['ignore', 'inherit', 'inherit'],
  });
}

fs.mkdirSync(dist, { recursive: true });

for (const name of selectedVersions()) {
  const v = versions[name];
  if (!v) throw new Error(`unknown version ${name}`);
  console.log(`== ${name} (${v.ref ?? 'working tree'})`);
  installPackage(v.ref);
  compile('Main.elm', path.join(dist, `${name}.js`), ['--optimize']);
  compile('Main.elm', path.join(dist, `${name}.dev.js`), []);
  compile('Worker.elm', path.join(dist, `${name}.worker.js`), ['--optimize']);
}
