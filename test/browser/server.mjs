// A WebSocket server the tests control, plus a static file server for the
// harness page and the compiled Elm bundles.
//
// Every scenario is chosen by the URL path, and every connection carries a
// `tid` (test id) query parameter so parallel tests can read back their own
// server-side view through GET /stats?tid=...
//
//   /echo?greet=1              echo text frames as text, binary as binary
//                              (with greet=1, first send "welcome")
//   /close?code=&reason=&after=   close with that code and reason `after` ms
//                              after the open (no code: a close frame with no
//                              status, which the client sees as 1005)
//   /drop?after=&once=         destroy the TCP socket `after` ms after the open,
//                              without a close frame (with once=1, only the
//                              test's first connection; later ones echo)
//   /binary                    send one binary frame, then the text "after-binary"
//   /burst?n=                  send n text frames "m0".."m{n-1}", then close 1000 "done"
//   /reject?status=            answer the upgrade request with that HTTP status
//   /hang?ms=                  accept the TCP connection and never answer the
//                              upgrade; destroy it after `ms` ms if given
//
// Refused connections and TLS failures need no route: the harness exposes a
// port nothing listens on (`refusedPort`), and a wss:// URL to this plain-HTTP
// port fails the TLS handshake.

import http from 'node:http';
import net from 'node:net';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import WebSocket from 'ws';

const here = path.dirname(fileURLToPath(import.meta.url));

const stats = new Map();

function statsFor(tid) {
  if (!stats.has(tid)) {
    stats.set(tid, { opened: 0, open: 0, closed: [], received: [], upgrades: 0 });
  }
  return stats.get(tid);
}

async function freePort() {
  return new Promise((resolve, reject) => {
    const s = net.createServer();
    s.on('error', reject);
    s.listen(0, '127.0.0.1', () => {
      const { port } = s.address();
      s.close(() => resolve(port));
    });
  });
}

const contentTypes = { '.html': 'text/html', '.js': 'text/javascript', '.mjs': 'text/javascript' };

export async function startServer({ port = 0, host = '127.0.0.1' } = {}) {
  const refusedPort = await freePort();
  const wss = new WebSocket.Server({ noServer: true });
  const sockets = new Set();

  const server = http.createServer((req, res) => {
    const url = new URL(req.url, 'http://x');
    if (url.pathname === '/stats') {
      res.setHeader('content-type', 'application/json');
      res.end(JSON.stringify(statsFor(url.searchParams.get('tid') || '')));
      return;
    }
    if (url.pathname === '/config') {
      res.setHeader('content-type', 'application/json');
      res.end(JSON.stringify({ refusedPort }));
      return;
    }
    // Static files: the harness page and the compiled bundles in dist/.
    const rel = url.pathname === '/' ? '/index.html' : url.pathname;
    const file = path.join(here, rel.startsWith('/dist/') ? '' : 'static', rel);
    if (!file.startsWith(here) || !fs.existsSync(file) || fs.statSync(file).isDirectory()) {
      res.statusCode = 404;
      res.end('not found');
      return;
    }
    res.setHeader('content-type', contentTypes[path.extname(file)] || 'application/octet-stream');
    res.setHeader('cache-control', 'no-store');
    fs.createReadStream(file).pipe(res);
  });

  server.on('connection', (socket) => {
    sockets.add(socket);
    socket.on('close', () => sockets.delete(socket));
  });

  // A wss:// connection to this plain-HTTP port sends a TLS ClientHello, which
  // node answers with a 400 and closes: the client sees a TLS failure.
  server.on('clientError', (err, socket) => {
    socket.destroy();
  });

  server.on('upgrade', (req, socket, head) => {
    const url = new URL(req.url, 'http://x');
    const p = url.searchParams;
    const s = statsFor(p.get('tid') || '');
    s.upgrades++;

    if (url.pathname === '/reject') {
      const status = Number(p.get('status') || 403);
      socket.end(`HTTP/1.1 ${status} ${http.STATUS_CODES[status] || 'Rejected'}\r\nContent-Length: 0\r\nConnection: close\r\n\r\n`);
      return;
    }

    if (url.pathname === '/hang') {
      const ms = p.get('ms');
      if (ms) setTimeout(() => socket.destroy(), Number(ms));
      return;
    }

    wss.handleUpgrade(req, socket, head, (ws) => {
      s.opened++;
      s.open++;
      ws.on('close', (code, reason) => {
        s.open--;
        s.closed.push({ code, reason: String(reason) });
      });
      ws.on('error', () => {});
      ws.on('message', (data) => {
        s.received.push(typeof data === 'string' ? data : `<binary ${data.length}>`);
      });

      const after = Number(p.get('after') || 0);

      let route = url.pathname;
      if (route === '/drop' && p.get('once') && s.opened > 1) route = '/echo';

      switch (route) {
        case '/echo':
          if (p.get('greet')) ws.send('welcome');
          ws.on('message', (data) => {
            if (ws.readyState === WebSocket.OPEN) ws.send(data);
          });
          break;

        case '/close':
          setTimeout(() => {
            if (p.has('code')) ws.close(Number(p.get('code')), p.get('reason') || '');
            else ws.close();
          }, after);
          break;

        case '/drop':
          setTimeout(() => ws._socket.destroy(), after);
          break;

        case '/binary':
          ws.send(Buffer.from([0xde, 0xad, 0xbe, 0xef]));
          ws.send('after-binary');
          break;

        case '/burst': {
          const n = Number(p.get('n') || 50);
          for (let i = 0; i < n; i++) ws.send(`m${i}`);
          ws.close(1000, 'done');
          break;
        }

        default:
          ws.close(4404, 'unknown path');
      }
    });
  });

  await new Promise((resolve) => server.listen(port, host, resolve));
  const actualPort = server.address().port;

  return {
    port: actualPort,
    refusedPort,
    stats: (tid) => statsFor(tid),
    async close() {
      for (const c of wss.clients) c.terminate();
      for (const s of sockets) s.destroy();
      await new Promise((resolve) => server.close(resolve));
    },
  };
}

// Run standalone: `node server.mjs [port]` (used by Playwright's webServer).
if (process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1])) {
  const port = Number(process.argv[2] || process.env.WS_TEST_PORT || 9753);
  startServer({ port }).then((s) => {
    console.log(`websocket test server on http://127.0.0.1:${s.port} (refused port ${s.refusedPort})`);
  });
}
