import JsonToTS from "../../json-to-ts/src/index.ts";
import { Database } from "bun:sqlite";
import { writeFile, mkdir } from "node:fs/promises";

const db = new Database("entries.db", { readonly: true });

const dicts = (() => {
  const dictsStmt = db.query("select id from dicts");
  return dictsStmt.values().map((v) => v[0]) as string[];
})();
// const dicts = ["chhoetaigi_taihoa"]

await mkdir("props", { recursive: true });
const stmt = db.query(`select props from heteronyms where "from" = ?`);
for (let i = 0; i < dicts.length; i++) {
  const dict = dicts[i];
  console.log(`Calculating schema for ${dict} (${i + 1}/${dicts.length})...`);
  const propsObjs = stmt.values(dict).map((v) => JSON.parse(v[0] as string));
  await writeFile(
    `props/${dict}.ts`,
    JsonToTS(propsObjs, {
      rootName: "HetProps",
      export: true,
    }).join("\n"),
  );
}
