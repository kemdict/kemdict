import { uniq, escapeRegExp } from "lodash-es";
import { escape as sqlEscape } from "sqlstring";

export const langs = {
  zh_TW: "華語",
  nan_TW: "台語",
  hak_TW: "客語",
  han: "漢字",
};

export function langIdName(langId: LangId): string {
  return langs[langId];
}

export interface Dict {
  id: string;
  name: string;
  url: string;
  lang: string;
}

export interface Heteronym {
  title: string;
  from: string | undefined;
  lang: string;
  props: any;
}

export type DictId = string;
export type LangId = string;

// This no longer defines the order in word pages.
export const dicts: Dict[] = [
  {
    id: "unihan",
    name: "Unihan 資料庫",
    url: "https://www.unicode.org/cgi-bin/GetUnihanData.pl?codepoint=$1",
    lang: "han",
  },
  {
    id: "chhoetaigi_taioanpehoekichhoogiku",
    name: "台灣白話基礎語句",
    url: "https://chhoe.taigi.info/TaioanPehoeKichhooGiku/$1",
    lang: "nan_TW",
  },
  {
    id: "kisaragi_dict",
    name: "如月的現代台灣華語補足典",
    url: "/dict-kisaragi",
    lang: "zh_TW",
  },
  {
    id: "dict_concised",
    name: "國語辭典簡編本",
    url: "https://dict.concised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh_TW",
  },
  {
    id: "dict_revised",
    name: "重編國語辭典修訂本",
    url: "https://dict.revised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh_TW",
  },
  {
    id: "chhoetaigi_taijittoasutian",
    name: "台日大辭典台語譯本 (1932)",
    url: "https://taigi.fhl.net/dict/search.php?DETAIL=1&LIMIT=id=$1",
    lang: "nan_TW",
  },
  {
    id: "moedict_twblg",
    name: "臺灣閩南語常用詞辭典",
    url: "https://twblg.dict.edu.tw/holodict_new/result_main.jsp?radiobutton=1&limit=20&querytarget=1&sample=$1",
    lang: "nan_TW",
  },
  {
    id: "chhoetaigi_itaigi",
    name: "iTaigi 華台對照典",
    url: "https://itaigi.tw/k/$1",
    lang: "nan_TW",
  },
  {
    id: "hakkadict",
    name: "臺灣客家語常用詞辭典",
    url: 'https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi?o=dalldb&s=id="$1".&searchmode=basic',
    lang: "hak_TW",
  },
  {
    id: "dict_idioms",
    name: "成語典",
    url: "https://dict.idioms.moe.edu.tw/idiomList.jsp?idiom=$1",
    lang: "zh_TW",
  },
];

export function dictIdToDict(dictId: DictId): Dict {
  return dictsObj[dictId];
}

function dictsToObj(dictionaries: Dict[]): Record<string, Dict> {
  const tmp = {};
  dictionaries.forEach((dict) => (tmp[dict.id] = dict));
  return tmp;
}
export const dictsObj = dictsToObj(dicts);

export const WordSortFns = {
  // These return numbers because that's what Array.sort wants.
  ascend: (a: Heteronym, b: Heteronym): 1 | -1 => {
    if (a.title > b.title) {
      return 1;
    } else {
      return -1;
    }
  },
  descend: (a: Heteronym, b: Heteronym): 1 | -1 => {
    if (a.title < b.title) {
      return 1;
    } else {
      return -1;
    }
  },
};

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
  const tokens = parseQueryToTokens(query);
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
    return `包含${joinLast(tokens.map(wrap), "、", "及")}的詞`;
  } else if (mtch === "prefix") {
    if (tokens.length === 1) {
      return `以${wrap(tokens[0])}開頭的詞`;
    } else {
      return `以${wrap(tokens[0])}開頭、且包含${joinLast(
        tokens.slice(1).map(wrap),
        "、",
        "及"
      )}的詞`;
    }
  } else if (mtch === "suffix") {
    if (tokens.length === 1) {
      return `以${wrap(tokens[tokens.length - 1])}結尾的詞`;
    } else {
      return `以${wrap(tokens[tokens.length - 1])}結尾且包含${joinLast(
        tokens.slice(0, -1).map(wrap),
        "、",
        "及"
      )}的詞`;
    }
  } else {
    // if (mtch === "exact") {
    // }
    return `完全符合「${query}」的詞`;
  }
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

export function parseQueryToTokens(inputQuery: string): string[] {
  return inputQuery.split(/\s+/);
}

export function tokenToLIKEInput(
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
  if (typeof het.props === "string") {
    het.props = JSON.parse(het.props);
  }
  return het;
}

/**
 * Shared DB instance to support both expo-sqlite and better-sqlite3.
 *
 * runtime: "web" or "rn" (React Native)
 * readDB: the function that returns the DB instance. Called once
 * during initialization; the db instance is reused afterwards.
 */
export class CrossDB {
  readonly runtime: "web" | "rn";
  readonly _db: unknown;
  constructor(runtime: "web" | "rn", readDB: () => any) {
    this.runtime = runtime;
    this._db = await readDB();
  }
  async crossDbAll(
    source: string,
    args: unknown[] = [],
    pluck?: boolean
  ): Promise<unknown[]> {
    if (this.runtime === "web") {
      const db = this._db;
      const stmt = db.prepare(source);
      if (pluck) stmt.pluck(pluck);
      return stmt.all(...args);
    } else {
      const db = this._db;
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
   * Returns [matchingDicts, Heteronyms, langCountObj]
   */
  async getHeteronyms(
    tokens: string | string[],
    options?: {
      mtch?: string;
      langs?: string[];
    }
  ): Promise<{
    presentDicts: DictId[];
    presentLangSet: Set<LangId>;
    heteronyms: Heteronym[];
    langCountObj: Record<LangId, number>;
  }> {
    if (typeof tokens === "string") {
      tokens = [tokens];
    }
    const mtch = options?.mtch || "prefix";
    const langs = options?.langs;
    const hasLangs = langs && langs.length > 0;
    const operator = mtch ? "LIKE" : "=";
    const hets = (await this.crossDbAll(
      `
SELECT DISTINCT title, "from", langs.id as lang, props
FROM heteronyms
LEFT JOIN dicts ON heteronyms."from" = dicts.id
LEFT JOIN langs ON dicts.lang = langs.id
INNER JOIN pns ON pns.het_id = heteronyms.id
WHERE "from" IS NOT NULL
${tokens
  .map(() => `AND (title ${operator} ? OR pns.pn ${operator} ?)`)
  .join("\n")}
`,
      (() => {
        const arr: string[] = [];
        const tokenCount = tokens.length;
        tokens.forEach((token, index) => {
          const query = tokenToLIKEInput(
            token,
            mtch,
            index === 0,
            index === tokenCount - 1
          );
          arr.push(query);
          arr.push(query);
        });
        return arr;
      })()
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
WHERE "to" IN (${sqlEscape(titles)})`,
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
group_concat("from") as dicts,
cast(json_tree.value as integer) AS 'sc'
FROM heteronyms, json_tree(heteronyms.props)
WHERE length("title") = 1
AND json_tree.key = 'sc'
GROUP BY title
HAVING dicts != 'unihan'
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
    substr(pn, 0, 2) AS 'pnInitial'
  FROM pns
  WHERE "pnInitial" IS NOT NULL
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
