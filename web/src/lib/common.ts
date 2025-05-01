import { escapeRegExp, chunk, sortBy } from "lodash-es";

// * Helpers

export function printfdebug(value: any) {
  console.log(JSON.stringify(value, null, 2));
}

/**
 * If `value` is not undefined, return `value` as an array.
 */
export function ensureArray<T>(value: T[] | T): T[] | undefined {
  if (Array.isArray(value)) {
    return value;
  } else if (typeof value === "undefined") {
    // Don't box undefined
    return value;
  } else {
    return [value];
  }
}

/**
 * Return a new array which is `arr` whose objects are grouped by their values
 * under `property`.
 *
 * [["value", [...]], ["value2", [...]]]
 *
 * If `fallback` is provided, items without `property` are grouped under
 * `fallback`. Otherwise they are discarded.
 */
export function groupByProp<T, K extends keyof T>(
  arr: T[],
  property: K,
): [T[K], T[]][];
export function groupByProp<T, K extends keyof T, F extends string>(
  arr: T[],
  property: K,
  fallback: F,
): [F | T[K], T[]][];
export function groupByProp<T, K extends keyof T, F extends string>(
  arr: T[],
  property: K,
  fallback?: F | undefined,
): [F | T[K], T[]][] {
  const map: Map<T[K] | F, T[]> = new Map();
  for (const elem of arr) {
    // if key is not undefined, then the item does have the property
    const key = elem[property];
    const realKey = key ?? fallback;
    if (typeof realKey === "undefined") {
      // the item doesn't have it and there is no fallback
      // discard the item
      continue;
    }
    const cell = map.get(realKey);
    if (cell !== undefined) {
      cell.push(elem);
    } else {
      map.set(realKey, [elem]);
    }
  }
  return [...map.entries()];
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

// * constants

export const site = {
  oneLineDesc: "Kemdict 是一個免費且無廣告的辭典搜尋服務。",
  title: "Kemdict",
};

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

// * lang / dict

export interface Dict {
  id: string;
  name: string;
  url: string;
  lang: LangId;
  /**
   * Language list displayed on dictionary pages instead of `lang`.
   *
   * Part of the workaround for Kemdict assuming sources have a single language.
   * For sources with multiple languages:
   * - define an entry for each language
   * - choose one entry and set `displayLangs` to include all of its languages
   * - for other entries set `hidden` to true
   */
  displayLangs?: LangId[];
  /**
   * Whether to hide this entry on the dictionary list. The dictionary will also
   * not get its own description page.
   *
   * Part of the workaround for Kemdict assuming sources have a single language.
   * For sources with multiple languages:
   * - define an entry for each language
   * - choose one entry and set `displayLangs` to include all of its languages
   * - for other entries set `hidden` to true
   */
  hidden?: boolean;
  meta?: {
    version?: string;
    author?: string;
    extra?: Record<string, string>;
    year: number;
    desc: string;
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

// Yes, this works, it is bundled properly.
import { dicts as origDicts, langs as origLangs } from "../../../dicts/data.ts";
export const dicts = origDicts as Dict[];
export const langs = origLangs;

export type DictId = string;
export type LangId = keyof typeof origLangs;

export function langIdName(langId: LangId): string {
  return langs[langId];
}

export function dictIdToDict(dictId: DictId) {
  return dictsObj[dictId];
}

function dictsToObj(dictionaries: Dict[]): Record<string, Dict> {
  const tmp = {} as Record<string, Dict>;
  dictionaries.forEach((dict) => (tmp[dict.id] = dict));
  return tmp;
}
export const dictsObj = dictsToObj(dicts);

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

export function joinLast(
  strings: string[],
  separator: string = "",
  lastSeparator: string = separator,
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
