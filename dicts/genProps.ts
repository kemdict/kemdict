import JsonToTS from "json-to-ts";
import { Database } from "bun:sqlite";
import { writeFile } from "node:fs/promises";

const db = new Database("entries.db", { readonly: true });

const dicts = (() => {
  const dictsStmt = db.query("select id from dicts");
  return dictsStmt.values().map((v) => v[0]) as string[];
})();

for (const dict of ["chhoetaigi_taihoa"]) {
  const stmt = db.query(`select props from heteronyms where "from" = ?`);
  const propsObjs = stmt.values(dict).map((v) => JSON.parse(v[0] as string));
  await writeFile("props.ts", JsonToTS(propsObjs).join("\n"));
}
