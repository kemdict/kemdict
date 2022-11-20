import { redirect } from "@sveltejs/kit";
import { db, processWord } from "$lib/server/db.js";

export function load({ url }) {
  const needle = url.searchParams.get("s");
  if (typeof needle !== "string") {
    throw redirect(301, "/");
  }
  const stmt = db.prepare(`SELECT * FROM entries WHERE title LIKE ?`);
  const words = stmt.all(`${needle}%`);
  // Redirect on the only exact match
  if (words && words.length === 1 && words[0]?.title === needle) {
    throw redirect(301, encodeURI(`/word/${words[0].title}`));
  } else {
    return { needle: needle, words: words.map(processWord) };
  }
}
