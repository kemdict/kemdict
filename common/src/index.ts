import { uniq, escapeRegExp } from "lodash-es";
import { escape as sqlEscape } from "sqlstring";

import searchQueryParser from "search-query-parser";
import type { SearchParserResult } from "search-query-parser";

// function printfdebug(value: any) {
//   console.log(JSON.stringify(value, null, 2));
// }

/**
 * If `value` is not undefined, return `value` as an array.
 */
function ensureArray<T>(value: T[] | T): T[] | undefined {
  if (Array.isArray(value)) {
    return value;
  } else if (typeof value === "undefined") {
    // Don't box undefined
    return value;
  } else {
    return [value];
  }
}

// sqlstring's escapes single quotes with a backslash, but SQLite
// expects it to be doubled instead.
function escape(thing: any) {
  return sqlEscape(thing).replace("\\'", "''").normalize("NFD");
}

export const site = {
  oneLineDesc: "Kemdict 是一個免費且無廣告的辭典搜尋服務。",
  title: "Kemdict",
};

export interface Dict {
  id: string;
  name: string;
  url: string;
  lang: string;
  meta?: {
    author?: string;
    year: number;
    desc_short: string;
    desc_long: string;
    license: {
      name: string;
      url: string;
    };
    /** Where I got the data from, like ChhoeTaigiDatabase for iTaigi. */
    source: string;
    /** The original website */
    original: string;
  };
}
export interface Heteronym {
  title: string;
  from: string | undefined;
  lang: string;
  props: any;
  exact?: boolean;
}

export type DictId = string;
export type LangId = string;

// Yes, this works, it is bundled properly.
import { dicts as origDicts, langs as origLangs } from "../../dicts/data.mjs";
export const dicts = origDicts as Dict[];
export const langs = origLangs;

export function langIdName(langId: LangId): string {
  return langs[langId];
}

export function dictIdToDict(dictId: DictId): Dict {
  return dictsObj[dictId];
}

function dictsToObj(dictionaries: Dict[]): Record<string, Dict> {
  const tmp = {};
  dictionaries.forEach((dict) => (tmp[dict.id] = dict));
  return tmp;
}
export const dictsObj = dictsToObj(dicts);

/**
 * Return a new array which is `arr` whose objects are grouped by `property`.
 *
 * [["value", [...]], ["value2", [...]]]
 *
 * Items without `property` are grouped under `fallback`. (More
 * accurately, they are grouped under the string representation of
 * `fallback`, so eg. `false` and "false" are equivalent.)
 */
// This is more or less seq-group-by ported over, except the fallback part.
export function groupByProp<T>(
  arr: T[],
  property: string,
  fallback?: any
): Array<[any, T[]]> {
  function reducer(acc: Record<any, T[]>, elt: T) {
    const key = elt[property];
    const cell = acc[key];
    if (cell) {
      cell.push(elt);
    } else {
      if (typeof key === "undefined") {
        acc[fallback] = [elt];
      } else {
        acc[key] = [elt];
      }
    }
    return acc;
  }
  return Object.entries(arr.reduce(reducer, {}));
}

/**
 * Format `str` using `template`.
 * $1 in `template` stands for `str`.
 */
export function format(template: string, str: string): string {
  return str.replace(RegExp(`(${escapeRegExp(str)})`), template);
}

/**
 * Return number of characters in `str`.
 *
 * From
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length
 */
export function strLen(str: string): number {
  return [...str].length;
}

export type Mtch = "prefix" | "suffix" | "contains" | "exact" | string;

export function getSearchTitle(
  mtch: Mtch,
  query: string,
  markup?: boolean
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
  } else if (mtch === "prefix") {
    if (tokens.length === 0) {
      ret = `開頭符合「${query}」的詞`;
    } else if (tokens.length === 1) {
      ret = `以${wrap(tokens[0])}開頭的詞`;
    } else {
      ret = `以${wrap(tokens[0])}開頭、且包含${joinLast(
        tokens.slice(1).map(wrap),
        "、",
        "及"
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
        "及"
      )}的詞`;
    }
  } else {
    ret = `完全符合「${query}」的詞`;
  }

  return ret;
}

/**
 * Parse the page param into a valid integer.
 * If the param is not present (=== null), treat it as page 1.
 * If the param is not valid, return false.
 * Note that page numbers are 1-based.
 * @param param -- the value of the url param
 * @param maximum -- the maximum number of pages
 */
export function parsePageParam(param: string | null, maximum: number) {
  const p = Number(param);
  // Param not present = page 1
  if (param === null) {
    return 1;
  } else if (!param || !p || p > maximum || p < 1 || !Number.isInteger(p)) {
    return false;
  } else {
    return p;
  }
}

export function parseLangParam(param: string | null, langIds: Set<string>) {
  // Param not present = first lang
  if (param === null) {
    return [...langIds][0];
  } else if (!langIds.has(param)) {
    return false;
  } else {
    return param;
  }
}

/**
 * Split `text` on whitespace to be processed later.`
 */
function parseStringQuery(text: string | string[] | undefined): string[] {
  if (Array.isArray(text)) {
    return text.join(" ").split(/\s+/);
  } else {
    return text?.split(/\s+/) || [];
  }
}
export function parseQuery(inputQuery: string) {
  // FIXME: use alwaysArray
  const result = searchQueryParser.parse(inputQuery, {
    keywords: ["lang", "title", "from"],
    offsets: false,
  });
  if (typeof result === "string") {
    return { text: result } as SearchParserResult;
  } else {
    return result;
  }
}

function parsedQueryToSQL(parsed: SearchParserResult, mtch: Mtch) {
  const operator = mtch === "exact" ? "=" : "LIKE";
  // printfdebug({ parsed });
  const exprs: string[] = [];
  const sqlArgs: string[] = [];
  ensureArray(parsed.text as string[] | string)?.forEach((text) => {
    // FIXME: use search-query-parser's tokenize option. That also
    // allows negation to work on non-keyword terms.
    const tokens =
      mtch === "exact"
        ? [text] // disable tokenizing in exact mode
        : parseStringQuery(text);
    tokens.forEach((s, i) => {
      exprs.push(`AND aliases.alias ${operator} ?`);
      sqlArgs.push(tokenToLIKEInput(s, mtch, i === 0, i === tokens.length - 1));
    });
  });
  ensureArray(parsed.exclude?.text as string[] | string)?.forEach((text) => {
    const tokens = parseStringQuery(text);
    tokens.forEach((s, i) => {
      exprs.push(`AND aliases.alias NOT ${operator} ?`);
      sqlArgs.push(tokenToLIKEInput(s, mtch, i === 0, i === tokens.length - 1));
    });
  });
  // TODO: This will tell SQLite to return heteronyms that are eg.
  // both in language A and language B, which is not possible.
  ensureArray(parsed.lang)?.forEach((lang) => {
    exprs.push(`AND lang LIKE '%${lang}%'`);
  });
  ensureArray(parsed.exclude?.lang)?.forEach((lang) => {
    exprs.push(`AND lang NOT LIKE '%${lang}%'`);
  });
  ensureArray(parsed.from)?.forEach((dict) => {
    exprs.push(`AND "from" LIKE '%${dict}%'`);
  });
  ensureArray(parsed.exclude?.from)?.forEach((dict) => {
    exprs.push(`AND "from" NOT LIKE '%${dict}%'`);
  });
  ensureArray(parsed.title)?.forEach((title, i, titles) => {
    exprs.push(`AND title ${operator} ?`);
    sqlArgs.push(
      tokenToLIKEInput(title, mtch, i === 0, i === titles.length - 1)
    );
  });
  ensureArray(parsed.exclude?.title)?.forEach((title, i, titles) => {
    exprs.push(`AND title NOT ${operator} ?`);
    sqlArgs.push(
      tokenToLIKEInput(title, mtch, i === 0, i === titles.length - 1)
    );
  });

  return {
    sqlExprs: exprs.join("\n").normalize("NFD"),
    sqlArgs: sqlArgs.map((s) => s.normalize("NFD")),
  };
}

function tokenToLIKEInput(
  token: string,
  mtch: Mtch,
  first = false,
  last = false
): string {
  let m = mtch;
  if ((mtch === "prefix" && !first) || (mtch === "suffix" && !last)) {
    m = "contains";
  }
  if (m === "prefix") {
    return `${token}%`;
  } else if (m === "suffix") {
    return `%${token}`;
  } else if (m === "contains") {
    return `%${token}%`;
  } else {
    return token;
  }
}

export function joinLast(
  strings: string[],
  separator: string = "",
  lastSeparator: string = separator
) {
  let buf = "";
  for (let i = 0; i < strings.length; i++) {
    if (i !== 0) {
      if (i === strings.length - 1) {
        buf += lastSeparator;
      } else {
        buf += separator;
      }
    }
    buf += strings[i];
  }
  return buf;
}

/**
 * Decode the JSON in the props field in a heteronym object.
 */
function processHet(het: Heteronym): Heteronym {
  het.title = het.title.normalize("NFC");
  het.from = het.from?.normalize("NFC");
  het.lang = het.lang.normalize("NFC");
  if (typeof het.props === "string") {
    het.props = JSON.parse(het.props.normalize("NFC"));
  }
  return het;
}

/**
 * Shared DB instance to support both expo-sqlite and better-sqlite3.
 *
 * runtime: "web" or "rn" (React Native)
 * readDB: the function that returns the DB instance. Called once
 * on first use; the db instance is reused afterwards.
 */
export class CrossDB {
  readonly #runtime: "web" | "rn";
  readonly #readDB: () => any;
  #db: any = undefined;
  constructor(runtime: "web" | "rn", readDB: () => any) {
    this.#runtime = runtime;
    this.#readDB = readDB;
  }
  // async constructor workaround
  async getDB() {
    if (!this.#db) {
      this.#db = await this.#readDB();
    }
    return this.#db;
  }
  async crossDbAll(
    source: string,
    args: unknown[] = [],
    pluck?: boolean
  ): Promise<unknown[]> {
    if (this.#runtime === "web") {
      const db = await this.getDB();
      // printfdebug({ source, args });
      const stmt = db.prepare(source);
      if (pluck) stmt.pluck(pluck);
      return stmt.all(...args);
    } else {
      const db = await this.getDB();
      return new Promise((resolve) => {
        db.transaction((tx) =>
          tx.executeSql(
            source,
            args as Array<string | number>,
            (_, resultSet) => resolve(resultSet.rows._array)
          )
        );
      });
    }
  }

  /**
   * Return heteronyms which match every TOKENS in its title or pronunciations.
   *
   * If MTCH is:
   * - "prefix": match heteronyms starting with the first TOKEN
   * - "suffix": match heteronyms ending with the last TOKEN
   * - "contains": match heteronyms that contain TOKEN
   * - any thing else (including "exact"): match heteronyms exactly
   *
   * Returns {presentDicts, presentLangSet, heteronyms, langCountObj}
   */
  async getHeteronyms(
    parsed: string | SearchParserResult,
    options?: {
      mtch?: string;
      langs?: string[] | string;
      limit?: number;
    }
  ): Promise<{
    presentDicts: DictId[];
    presentLangSet: Set<LangId>;
    heteronyms: Heteronym[];
    langCountObj: Record<LangId, number>;
  }> {
    // printfdebug({
    //   parsed,
    //   options,
    // });
    if (typeof parsed === "string") {
      parsed = { text: parsed.normalize("NFD") };
    }
    const mtch = options?.mtch || "exact";
    const limit = options?.limit;
    const langs = ensureArray(options?.langs);
    const hasLangs = langs && langs.length > 0;
    const { sqlExprs, sqlArgs } = parsedQueryToSQL(parsed, mtch);
    const hets = (await this.crossDbAll(
      `
SELECT DISTINCT title, "from", lang, props, aliases.exact as exact
FROM heteronyms
INNER JOIN aliases ON aliases.het_id = heteronyms.id
WHERE "from" IS NOT NULL
${sqlExprs}
${limit ? `LIMIT ?` : ""}
`,
      [...sqlArgs, ...(limit ? [limit] : [])]
    )) as Heteronym[];
    let applicableHets = hets;
    if (hasLangs) {
      applicableHets = hets.filter((het) => langs.includes(het.lang));
    }
    const dictSet: Set<DictId> = new Set();
    const langSet: Set<LangId> = new Set();
    const langCountObj: Record<LangId, number> = {};
    // Across all hets, not just filtered
    for (const het of hets || []) {
      if (het.from) {
        dictSet.add(het.from);
      }
      langSet.add(het.lang);
      langCountObj[het.lang] = (langCountObj[het.lang] || 0) + 1;
    }
    return {
      presentDicts: [...dictSet],
      presentLangSet: langSet,
      heteronyms: applicableHets?.map(processHet),
      langCountObj,
    };
  }

  async getBacklinks(...titles: string[]): Promise<string[]> {
    return (await this.crossDbAll(
      `
SELECT DISTINCT "from" FROM links
WHERE "to" IN (${escape(titles)})`,
      [],
      true
    )) as string[];
  }

  async getDictTitles(from: string, limit?: number): Promise<string[]> {
    if (limit) {
      return (await this.crossDbAll(
        `SELECT DISTINCT title FROM heteronyms WHERE "from" = ? LIMIT ?`,
        [from, limit],
        true
      )) as string[];
    } else {
      return (await this.crossDbAll(
        `SELECT DISTINCT title FROM heteronyms WHERE "from" = ?`,
        [from],
        true
      )) as string[];
    }
  }

  async getChars(): Promise<{
    with_stroke: Array<{
      title: string;
      sc: number;
    }>;
    without_stroke: Array<string>;
  }> {
    const with_stroke = (await this.crossDbAll(
      `
SELECT DISTINCT
  heteronyms.title,
  cast(json_tree.value as integer) AS 'sc'
FROM heteronyms, json_tree(heteronyms.props)
WHERE length("title") = 1
  AND json_tree.key = 'sc'
GROUP BY title
HAVING group_concat("from") != 'unihan'
ORDER BY 'sc';
`
    )) as Array<{
      title: string;
      sc: number;
    }>;
    const nostroke = (await this.crossDbAll(
      `
SELECT initial FROM (
  SELECT DISTINCT
    substr(heteronyms.title, 0, 2) AS 'initial',
    group_concat("from") as dicts
  FROM heteronyms
)
WHERE dicts != 'unihan'`,
      [],
      true
    )) as string[];
    const pn = (await this.crossDbAll(
      `
SELECT DISTINCT
  substr(alias, 0, 2) AS 'pnInitial'
FROM aliases
WHERE exact IS NULL -- exact = comes from "title"
AND "pnInitial" IS NOT NULL
`,
      [],
      true
    )) as string[];
    const s = new Set(with_stroke.map((x) => x.title));
    const without_stroke: string[] = uniq([...nostroke, ...pn])
      .filter((x: string) => !s.has(x))
      .sort();
    return { with_stroke, without_stroke };
  }

  async getCharsByRadical(radical: string) {
    return await this.crossDbAll(
      `
  SELECT DISTINCT
    title,
    nrsc
  FROM han
  WHERE radical = ?
  ORDER BY nrsc
`,
      [radical]
    );
  }

  async getRadicals() {
    return await this.crossDbAll(
      `
    SELECT DISTINCT radical, sc
    FROM han
    WHERE nrsc = 0
    ORDER BY radical
`
    );
  }
}
