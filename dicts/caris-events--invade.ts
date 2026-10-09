import { readFileSync, readdirSync, writeFileSync } from "node:fs";
import { load } from "js-yaml";
import { z } from "zod";

const vocabDir = "caris-events--invade/database/vocabs";
const words = readdirSync(vocabDir).map((f) => f.replace(".yml", ""));
const wordFiles = words.map((w) => `${vocabDir}/${w}.yml`);

// taken from cmd/build/entity/vocab.go
const categoryMap = {
  ADJECTIVE: "形容詞",
  ADVERB: "副詞",
  ANIMAL: "動物",
  CLOTHING: "服飾",
  FINANCE: "金融與消費",
  FOOD: "食物",
  GAME: "遊戲",
  HARDWARE: "硬體與設備",
  INTERNET: "網路用語",
  MEDIA: "影音媒體",
  NOUN: "名詞",
  PLACE: "地點",
  PRONOUN: "代名詞",
  SLANG: "俚語",
  SWEAR: "髒話",
  TECHNOLOGY: "科技",
  VEHICLE: "交通工具",
  VERB: "動詞",
  WEAPON: "武器",
} as const;

const explicitMap = {
  LANGUAGE: "粗暴語言",
  SEXUAL: "性相關",
} as const;

const word = z.object({
  word: z.string(),
  bopomofo: z.string().nullable(),
  deprecation: z.optional(z.string()),
  category: z
    .enum([
      "ADJECTIVE",
      "ADVERB",
      "ANIMAL",
      "CLOTHING",
      "FINANCE",
      "FOOD",
      "GAME",
      "HARDWARE",
      "INTERNET",
      "MEDIA",
      "NOUN",
      "PLACE",
      "PRONOUN",
      "SLANG",
      "SWEAR",
      "TECHNOLOGY",
      "VEHICLE",
      "VERB",
      "WEAPON",
    ])
    .transform((it) => categoryMap[it]),
  explicit: z
    .enum(["LANGUAGE", "SEXUAL"])
    .transform((it) => explicitMap[it])
    .nullable(),
  description: z.optional(z.string()).nullable(),
  notice: z.optional(z.string()).nullable(),
  examples: z.array(
    z.object({
      words: z.array(z.string()),
      correct: z.string(),
      incorrect: z.optional(z.string()),
    }),
  ),
});

function loadWord(wordFile: string) {
  const value = load(readFileSync(wordFile, { encoding: "utf-8" }));
  try {
    const parsed = word.parse(value);
    return parsed;
  } catch (e) {
    console.log("error: ", e);
    console.log(value);
    throw e;
  }
}
const loadedWords = wordFiles.map(loadWord);

writeFileSync(
  "caris-events--invade.json",
  JSON.stringify(loadedWords, null, 2),
);
