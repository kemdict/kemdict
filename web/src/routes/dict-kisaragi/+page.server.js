import { db } from "$lib/server/db.js";

const stmt = db.prepare(
  `SELECT DISTINCT title FROM heteronyms WHERE "from" = 'kisaragi_dict'`
);
stmt.pluck(true);
export function load() {
  return { titles: stmt.all() };
}
