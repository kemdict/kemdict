import { spawnSync } from "node:child_process";

export function toPOJ(text) {
  return spawnSync(
    "node",
    ["hack/postSync.mjs", "https://pojtl.kemdict.com/toPOJ"],
    { input: text }
  ).stdout;
}

export function toTL(text) {
  return spawnSync(
    "node",
    ["hack/postSync.mjs", "https://pojtl.kemdict.com/toTL"],
    { input: text }
  ).stdout;
}
