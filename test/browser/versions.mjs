// The lamdera/websocket sources the tests run against. `ref: null` is this
// working tree; the others are read out of git history, so CI needs a full
// clone (fetch-depth: 0).
export const versions = {
  // What static.lamdera.com serves today as lamdera/websocket 1.0.0.
  served: { ref: '3f35635' },
  // master: PR #1 "Add error handlers" (9b4099c) plus docs and example work.
  master: { ref: '3a219fd' },
  // PR #2, fix/error-event-crash.
  pr2: { ref: 'f1b2cca' },
  // This branch.
  current: { ref: null },
};

export function selectedVersions() {
  const env = process.env.WS_VERSIONS;
  return env ? env.split(',').map((v) => v.trim()).filter(Boolean) : Object.keys(versions);
}
