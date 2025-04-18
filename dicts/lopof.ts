/** Generate a Taiwanese names index from our digitization result of
 * List of Plants of Formosa. */

import { Database } from "bun:sqlite";
import {
  plantsData,
  type PlantName,
} from "./list-of-plants-of-formosa/src/schema.ts";
import { writeFileSync } from "fs";

const db = new Database("./list-of-plants-of-formosa/data/plants.sqlite", {
  readonly: true,
  strict: true,
  create: false,
});

/**
 * Replace old Han characters within `str`.
 */
function normalizeHanCharacters(str: string) {
  return str
    .replaceAll("猪", "豬")
    .replaceAll("脚", "腳")
    .replaceAll("猫", "貓")
    .replaceAll("籐", "藤")
    .replaceAll("蘚", "癬")
    .replaceAll("鷄", "雞")
    .replaceAll("囘", "回")
    .replaceAll("恒", "恆")
    .replaceAll("𠻳", "嗽")
    .replaceAll("𣏌", "杞")
    .replaceAll("覇", "霸")
    .replaceAll("兎", "兔");
}

/**
 * Annotate `name` with language info to better placate TypeScript.
 */
function nameWithLang(name: PlantName) {
  if ("romaji" in name) {
    return {
      lang: "ja",
      ...name,
    };
  } else if ("hakka" in name && name.hakka) {
    return {
      lang: "hakka",
      ...name,
    };
  } else if ("poj" in name) {
    return {
      lang: "taigi",
      ...name,
    };
  } else if ("native" in name) {
    return {
      lang: name.group,
      ...name,
    };
  } else {
    return {
      lang: "note",
      ...name,
    };
  }
}

// Because kemdict assumes one dictionary is one language, we need different
// "dictionary" json files for different languages.

const plants = plantsData.parse(
  db
    .query(`SELECT obj FROM plants`)
    .values()
    .map((v) => JSON.parse(v[0] as string)),
);

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
  otherNames: unknown;
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
  otherNames: unknown;
}

const hakkaHet: HetHakka[] = [];
const taigiHet: HetTaigi[] = [];
for (const plant of plants) {
  if (!plant) continue;
  if (!plant.names) continue;
  for (const name of plant.names) {
    if (!("poj" in name)) continue;
    (name.hakka ? hakkaHet : taigiHet).push({
      title: name.han,
      poj: name.poj.toLowerCase(),
      scientificName: `${plant.title}, ${plant.by}`,
      page: plant.page,
      family: plant.family,
      indigenous: plant.indigenous,
      where: plant.where,
      otherNames: Object.groupBy(
        plant.names
          .filter(
            (other) =>
              !(
                "poj" in other &&
                other.poj === name.poj &&
                other.han === name.han
              ),
          )
          .map(nameWithLang),
        (o) => o.lang,
      ),
    });
  }
}

writeFileSync(
  "lopof-nan_TW.json",
  normalizeHanCharacters(JSON.stringify(taigiHet, null, 2)),
);
writeFileSync(
  "lopof-hak_TW.json",
  normalizeHanCharacters(JSON.stringify(hakkaHet, null, 2)),
);
