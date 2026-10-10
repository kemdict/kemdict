import JsonToTS from "@kemdict/json-to-ts";
import { DatabaseSync } from "node:sqlite";
import { writeFile, mkdir } from "node:fs/promises";

const db = new DatabaseSync("entries.db", { readOnly: true });

const dicts = (() => {
  const dictsStmt = db.prepare("select id from dicts");
  return dictsStmt.all().map(({ id }) => id);
})();
// const dicts = ["chhoetaigi_taihoa"]

await mkdir("props", { recursive: true });
const stmt = db.prepare(`select props from heteronyms where "from" = ?`);
for (let i = 0; i < dicts.length; i++) {
  const dict = dicts[i];
  console.log(`Calculating schema for ${dict} (${i + 1}/${dicts.length})...`);
  const propsObjs = stmt
    .all(dict)
    .map(({ props }) => JSON.parse(props as string));
  await writeFile(
    `props/${dict}.ts`,
    JsonToTS(propsObjs, {
      rootName: "HetProps",
      export: true,
    }).join("\n"),
  );
}
