import { uniqBy, chunk, sortBy } from "lodash-es";
import { CrossDB, parseQuery, parseStringQuery, type Mtch } from "./crossdb";
import { processPn } from "$lib/processing";
import type { Heteronym, LangId } from "common";
import { groupByProp, joinLast } from "common";
import { Database } from "bun:sqlite";

export async function readDB() {
  const fs = await import("node:fs");
  const path = [
    "../kemdict.db",
    "../entries.db",
    "../dicts/entries.db",
    "../../dicts/entries.db",
    "./entries.db",
  ].find((f) => fs.existsSync(f));
  if (!path) throw new Error("DB not found!");
  const db = new Database(path, {
    readonly: true,
  });
  return db;
}

export const DB = new CrossDB("bun", readDB);

export function getSearchTitle(
  mtch: Mtch,
  query: string,
  markup?: boolean,
): string {
  let ret = "";
  const parsed = parseQuery(query);
  const tokens = parseStringQuery(parsed.text);
  function wrap(s: string) {
    if (markup) {
      return `「<span class="font-bold">${s}</span>」`;
    } else {
      return `「${s}」`;
    }
  }
  if (markup) {
    query = `<span class="font-bold">${query}</span>`;
  }
  if (mtch === "contains") {
    if (tokens.length === 0) {
      ret = `符合「${query}」的詞`;
    } else {
      ret = `包含${joinLast(tokens.map(wrap), "、", "及")}的詞`;
    }
    // } else if (mtch === "content") {
    //   if (tokens.length === 0) {
    //     ret = `內文包含「${query}」的詞`;
    //   } else {
    //     ret = `內文包含${joinLast(tokens.map(wrap), "、", "及")}的詞`;
    //   }
  } else if (mtch === "prefix") {
    if (tokens.length === 0) {
      ret = `開頭符合「${query}」的詞`;
    } else if (tokens.length === 1) {
      ret = `以${wrap(tokens[0])}開頭的詞`;
    } else {
      ret = `以${wrap(tokens[0])}開頭、且包含${joinLast(
        tokens.slice(1).map(wrap),
        "、",
        "及",
      )}的詞`;
    }
  } else if (mtch === "suffix") {
    if (tokens.length === 0) {
      ret = `結尾符合「${query}」的詞`;
    } else if (tokens.length === 1) {
      ret = `以${wrap(tokens[tokens.length - 1])}結尾的詞`;
    } else {
      ret = `以${wrap(tokens[tokens.length - 1])}結尾且包含${joinLast(
        tokens.slice(0, -1).map(wrap),
        "、",
        "及",
      )}的詞`;
    }
  } else {
    ret = `完全符合「${query}」的詞`;
  }

  return ret;
}

/**
 * Return true if `het` can be an exact match and `query` matches it
 * exactly.
 */
export function hetExactMatch(het: Heteronym, query: string | undefined) {
  return !!(het.exact && query && het.title === query);
}

/**
 * Compare `a` and `b`, sorting the one that is an exact match first.
 * Returns 0 if they are both exact matches, just like a normal sort function should.
 */
function hetMoreExact(
  query: string,
  a: [Heteronym, string | undefined],
  b: [Heteronym, string | undefined],
) {
  const aExactMatch = hetExactMatch(a[0], query);
  const bExactMatch = hetExactMatch(b[0], query);
  if (aExactMatch && !bExactMatch) return -1;
  if (bExactMatch && !aExactMatch) return 1;
  return 0;
}

/**
 * Compare two heteronyms.
 * `aPn` and `bPn` are precomputed results of `processPn(a)` or `processPn(b)`.
 * `lang` being "zh_TW" will turn off sorting based on aPn and bPn.
 */
function hetLessThan(
  lang: string | undefined,
  [a, aPn]: [Heteronym, string | undefined],
  [b, bPn]: [Heteronym, string | undefined],
) {
  if (lang !== "zh_TW") {
    const aPnOrTitle = (aPn || a.title).toLowerCase();
    const bPnOrTitle = (bPn || b.title).toLowerCase();
    if (aPnOrTitle < bPnOrTitle) return -1;
    if (aPnOrTitle > bPnOrTitle) return 1;
  }
  if (a.title < b.title) return -1;
  if (a.title > b.title) return 1;
  return 0;
}

/**
 * Compare two heteronyms by length.
 * `aPn` and `bPn` are precomputed results of `processPn(a)` or `processPn(b)`.
 * `lang` being "zh_TW" will turn off sorting based on aPn and bPn.
 */
function hetLengthLessThan(
  lang: string | undefined,
  [a, aPn]: [Heteronym, string | undefined],
  [b, bPn]: [Heteronym, string | undefined],
) {
  if (lang !== "zh_TW") {
    const aPnOrTitle = aPn || a.title;
    const bPnOrTitle = bPn || b.title;
    if (aPnOrTitle.length < bPnOrTitle.length) return -1;
    if (aPnOrTitle.length > bPnOrTitle.length) return 1;
  }
  if (a.title.length < b.title.length) return -1;
  if (a.title.length > b.title.length) return 1;
  return 0;
}

/**
 * Like search/index.astro's load() function.
 */
export async function getHetFromUrl(
  url: URL,
  lang?: string,
): Promise<
  | [
      true,
      {
        heteronymsAndPn?: undefined;
        mtch?: undefined;
        query?: undefined;
        originalQuery?: undefined;
        langSet?: undefined;
        langCountObj?: undefined;
        /** This page has no suffix.
            Redirect to /search if the page isn't /search. */
        root: true;
      },
    ]
  | [
      true,
      {
        heteronymsAndPn: [Heteronym, string | undefined][];
        mtch: string;
        query: string;
        originalQuery: string | undefined;
        langSet: Set<LangId>;
        langCountObj: Record<LangId, number>;
        root: false;
      },
    ]
  // when the first item is false, the second element is a string
  | [false, string]
> {
  if (url.search === "") {
    return [true, { root: true }];
  }
  /** Query text as written in the URL */
  const originalQuery = url.searchParams.get("q")?.trim();
  /** Unicode normalized query */
  const query = originalQuery?.normalize("NFC");
  const mtch: Mtch = url.searchParams.get("m") || "prefix";

  const sort = (url.searchParams.get("s") || "desc") as
    | "desc"
    | "length-desc"
    | "length-asc"
    // autocomplete with arbitrary string magic
    | (string & {});
  /**
   * When this flag is provided, if there is only one match, we
   * redirect to it.
   */
  const redirectOnSingleResult = url.searchParams.has("r");
  // Invalid: redirect to root
  if (typeof query !== "string" || query.length === 0) {
    return [false, "/search"];
  }
  const { presentLangSet, heteronyms, langCountObj, parsed } =
    await DB.getHeteronyms(query, {
      mtch,
      langs: lang,
    });
  // Redirect if all matched heteronyms belong to the same title
  if (
    redirectOnSingleResult &&
    mtch === "prefix" &&
    heteronyms &&
    // The query is just text, no filters and no exclusions
    Object.keys(parsed).length === 1 &&
    // The query does not invoke the "second token uses contains
    // matching" logic
    !query.match(/\s/) &&
    heteronyms.length > 0 &&
    heteronyms.length < 10 &&
    heteronyms.every((x) => x.title === heteronyms[0].title)
  ) {
    return [
      false,
      encodeURI(
        heteronyms.length === 1
          ? `/word/${heteronyms[0].title}?lang=${heteronyms[0].lang}`
          : `/word/${heteronyms[0].title}`,
      ),
    ];
  }
  const heteronymsAndPn = heteronyms.map(
    (het) => [het, processPn(het)] as [Heteronym, string | undefined],
  );
  // Negative -> a comes first
  // Positive -> b comes first
  // 0 -> keep
  if (sort === "length-desc") {
    heteronymsAndPn.sort((a, b) => {
      return hetMoreExact(query, a, b) || hetLengthLessThan(lang, a, b);
    });
  } else if (sort === "length-asc") {
    heteronymsAndPn.sort((a, b) => {
      return hetMoreExact(query, a, b) || -hetLengthLessThan(lang, a, b);
    });
  } else if (sort === "desc") {
    heteronymsAndPn.sort((a, b) => {
      return hetMoreExact(query, a, b) || hetLessThan(lang, a, b);
    });
  } else {
    heteronymsAndPn.sort((a, b) => {
      return hetMoreExact(query, a, b) || -hetLessThan(lang, a, b);
    });
  }
  return [
    true,
    {
      root: false,
      heteronymsAndPn: uniqBy(heteronymsAndPn, ([het, pn]) => {
        return (
          het.title + het.lang + het.from + pn + `${hetExactMatch(het, query)}`
        );
      }),
      mtch,
      query,
      originalQuery,
      langSet: presentLangSet,
      langCountObj,
    },
  ];
}

/**
 * Append the POJ version of a heteronym to the title if applicable.
 */
export function taigiTitle(het: Heteronym): string {
  // title is set to kip in the dicts Makefile;
  // Show POJ in title if they're not equal.
  if (het.props.titlePoj && het.title !== het.props.titlePoj) {
    return `${het.title} (${het.props.titlePoj})`;
  } else {
    return het.title;
  }
}

/** The data for the initials page. Held indefinitely. */
let groupedChars: {
  without_stroke: string[];
  with_stroke_grouped: [
    number,
    {
      title: string;
      sc: number;
    }[],
  ][][];
};
/** Initialize and return `groupedChars`. */
export async function getGroupedChars() {
  if (groupedChars) return groupedChars;
  const { with_stroke, without_stroke } = await DB.getChars();
  const with_stroke_grouped = chunk(sortBy(with_stroke, "sc"), 1200).map(
    (page) => groupByProp(page, "sc"),
  );
  groupedChars = { without_stroke, with_stroke_grouped };
  return groupedChars;
}
