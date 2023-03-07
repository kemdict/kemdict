export const langs = {
  zh_TW: "華語",
  nan_TW: "台語",
  hak_TW: "客語",
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
    name: "教育部國語辭典簡編本",
    url: "https://dict.concised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh_TW",
  },
  {
    id: "dict_revised",
    name: "教育部重編國語辭典",
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
    name: "教育部臺灣閩南語常用詞辭典",
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
    name: "教育部臺灣客家語常用詞辭典",
    url: 'https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi?o=dalldb&s=id="$1".&searchmode=basic',
    lang: "hak_TW",
  },
  {
    id: "dict_idioms",
    name: "教育部成語典",
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
export const dictsByLang = {
  zh_TW: ["kisaragi_dict", "dict_concised", "dict_revised", "dict_idioms"],
  nan_TW: [
    "chhoetaigi_taioanpehoekichhoogiku",
    "chhoetaigi_taijittoasutian",
    "moedict_twblg",
    "chhoetaigi_itaigi",
  ],
  hak_TW: ["hakkadict"],
};

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
 *
 * @param {Array<object>} arr
 * @param {string} property
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
      if (key) {
        acc[key] = [elt];
      } else {
        acc[fallback] = [elt];
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
  return str.replace(RegExp(`(${str})`), template);
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
