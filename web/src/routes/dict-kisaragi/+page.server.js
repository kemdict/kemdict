export const prerender = false;

import { db } from "$lib/server/db.js";

export function load() {
  const stmt = db.prepare(
    `SELECT DISTINCT title FROM heteronyms WHERE "from" = 'kisaragi_dict'`
  );
  stmt.pluck(true);
  return { titles: stmt.all() };
}
