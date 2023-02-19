export const prerender = false;

import { db } from "$lib/server/db.js";

const stmt = db.prepare(
  `SELECT DISTINCT title FROM heteronyms WHERE "from" = 'kisaragi_dict' LIMIT 12`
);
stmt.pluck(true);
export function load() {
  return { kisaragi_dict_titles: stmt.all() };
}
