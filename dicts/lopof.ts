/** Generate a Taiwanese names index from our digitization result of
 * List of Plants of Formosa. */

import { Database } from "bun:sqlite";
import { plantsData } from "./list-of-plants-of-formosa/src/schema.ts";
import { writeFileSync } from "fs";

const db = new Database("./list-of-plants-of-formosa/data/plants.sqlite", {
  readonly: true,
  strict: true,
  create: false,
});

/**
 * The same values as in the database's names.lang column:
 *   - taigi-poj and taigi-han for Taigi
 *   - hakka-poj and hakka-han for Hakka
 *   - ja-kana and ja-romaji for Japanese
 *   - Other indigenous languages haven't been normalized yet
 */
type Lang =
  | "ja-kana"
  | "ja-romaji"
  | "taigi-poj"
  | "taigi-han"
  | "タイヤル"
  | "パイワン"
  | "hakka-poj"
  | "hakka-han"
  | "サイセツト"
  | "アミス"
  | "ヤミー"
  | "ツオー"
  | "アミー";

/**
 * Return plant objects that have names matching any of `langs`.
 * `langs` is sliced directly into SQL. Do not pass untrusted input.
 * `langs` are the same values as in the database's names.lang column:
 *   - taigi-poj and taigi-han for Taigi
 *   - hakka-poj and hakka-han for Hakka
 *   - ja-kana and ja-romaji for Japanese
 *   - Other indigenous languages haven't been normalized yet
 */
function plantsWithLang(langs: Lang[]) {
  return plantsData.parse(
    db
      .query(
        `
SELECT obj FROM plants
WHERE plants.title IN (
  SELECT scientificName
  FROM names
  WHERE ${langs.map((l) => `lang = '${l}'`).join(" OR ")}
)`,
      )
      .values()
      .map((v) => JSON.parse(v[0] as string)),
  );
}

// Because kemdict assumes one dictionary is one language, we need different
// "dictionary" json files for different languages.

const plantsWithTaigi = plantsWithLang(["taigi-poj", "taigi-han"]);
const plantsWithHakka = plantsWithLang(["hakka-poj", "hakka-han"]);

interface HetHakka {
  // han as title, like how we process other sources
  title: string;
  poj: string;
  // scientificName + by whom
  scientificName: string;
  page: number;
  family: string;
  indigenous: boolean;
  where: string | undefined;
}
interface HetTaigi {
  // han as title, like how we process other sources
  title: string;
  poj: string;
  // scientificName + by whom
  scientificName: string;
  page: number;
  family: string;
  indigenous: boolean;
  where: string | undefined;
}

const hakkaHet: HetHakka[] = [];
const taigiHet: HetTaigi[] = [];
for (const plant of plantsWithTaigi) {
  if (!plant) continue;
  if (!plant.names) continue;
  for (const name of plant.names) {
    if (!("poj" in name)) continue;
    if (name.hakka) {
      hakkaHet.push({
        title: name.han,
        poj: name.poj,
        scientificName: `${plant.title}, ${plant.by}`,
        page: plant.page,
        family: plant.family,
        indigenous: plant.indigenous,
        where: plant.where,
      });
    } else {
      taigiHet.push({
        title: name.han,
        poj: name.poj,
        scientificName: `${plant.title}, ${plant.by}`,
        page: plant.page,
        family: plant.family,
        indigenous: plant.indigenous,
        where: plant.where,
      });
    }
  }
}

writeFileSync("lopof-nan_TW.json", JSON.stringify(taigiHet, null, 2));
writeFileSync("lopof-hak_TW.json", JSON.stringify(hakkaHet, null, 2));
