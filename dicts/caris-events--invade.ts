import { readFileSync, readdirSync, writeFileSync } from "node:fs";
import { load } from "js-yaml";
import { z } from "zod";

const vocabDir = "caris-events--invade/database/vocabs";
const words = readdirSync(vocabDir).map((f) => f.replace(".yml", ""));
const wordFiles = words.map((w) => `${vocabDir}/${w}.yml`);

const word = z.object({
  word: z.string(),
  bopomofo: z.string().nullable(),
  category: z.enum([
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
  ]),
  explicit: z.enum(["LANGUAGE", "SEXUAL"]).nullable(),
  description: z.optional(z.string()).nullable(),
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
