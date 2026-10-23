import { defineConfig, devices } from '@playwright/test';
import { selectedVersions } from './versions.mjs';

// The matrix: every browser against every lamdera/websocket version that
// build.mjs compiled (WS_VERSIONS narrows both). WS_MODES=opt,dev adds the
// non-optimized bundles too.
const port = Number(process.env.WS_TEST_PORT || 9753);
const browsers = (process.env.WS_BROWSERS || 'chromium,firefox,webkit').split(',');
const modes = (process.env.WS_MODES || 'opt').split(',');
const deviceFor = { chromium: 'Desktop Chrome', firefox: 'Desktop Firefox', webkit: 'Desktop Safari' };

const projects = [];
for (const browser of browsers) {
  for (const version of selectedVersions()) {
    for (const mode of modes) {
      projects.push({
        name: `${browser}-${version}${mode === 'opt' ? '' : '-' + mode}`,
        use: { ...devices[deviceFor[browser]], wsVersion: version, wsBundle: mode === 'opt' ? version : `${version}.${mode}` },
      });
    }
  }
}

export default defineConfig({
  testDir: './tests',
  fullyParallel: true,
  workers: process.env.CI ? 4 : 8,
  retries: 0,
  timeout: 30_000,
  reporter: process.env.CI ? [['list'], ['github']] : [['list']],
  use: { baseURL: `http://127.0.0.1:${port}`, wsPort: port },
  projects,
  webServer: {
    command: `node server.mjs ${port}`,
    url: `http://127.0.0.1:${port}/config`,
    reuseExistingServer: !process.env.CI,
    stdout: 'ignore',
  },
});
