import { escapeRegExp } from "lodash-es";

export const langs = {
  zh_TW: "華語",
  nan_TW: "台語",
  hak_TW: "客語",
  han: "漢字",
};

export interface Dict {
  id: string;
  name: string;
  url: string;
  lang: string;
}

export interface Heteronym {
  title: string;
  from?: string;
  pns?: any[];
  props: any;
}

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

export function dictsToObj(dictionaries: Dict[]): Record<string, Dict> {
  const tmp = {};
  dictionaries.forEach((dict) => (tmp[dict.id] = dict));
  return tmp;
}
export const dictIds = dicts.map((x) => x.id);
export const langIds = Object.keys(langs);
// {"zh_TW": [...], "han": [...]}
export const dictsByLang = (() => {
  const res = {};
  groupByProp(dicts, "lang")
    .map(([key, objs]) => [key, objs.map((x) => x.id)])
    .forEach(([key, ids]) => (res[key] = ids));
  return res;
})();

export const version = (import.meta.env.KEMDICT_VERSION as string) || "unknown";

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
  if (markup) {
    query = `<span class="font-bold">${query}</span>`;
  }
  if (mtch === "prefix") {
    return `以「${query}」開頭的詞`;
  } else if (mtch === "suffix") {
    return `以「${query}」結尾的詞`;
  } else if (mtch === "contains") {
    return `包含「${query}」的詞`;
  } else if (mtch === "exact") {
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
