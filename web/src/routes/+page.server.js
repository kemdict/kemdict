export const prerender = false;

import { db } from "$lib/server/db.js";

export function load() {
  const stmt = db.prepare(
    `SELECT DISTINCT title FROM heteronyms WHERE "from" = 'kisaragi_dict' LIMIT 12`
  );
  stmt.pluck(true);
  return { kisaragi_dict_titles: stmt.all() };
}
