export const langs = { hak_TW: "客語", nan_TW: "台語", zh_TW: "華語" };

// This also defines the order in the word page
export const dicts = [
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
    url: "https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi/ccd=qwMPHD/search?dcf=sti&extrasearch=es1&qs0=$1",
    lang: "hak_TW",
  },
  {
    id: "dict_idioms",
    name: "教育部成語典",
    url: "https://dict.idioms.moe.edu.tw/idiomList.jsp?idiom=$1",
    lang: "zh_TW",
  },
];

export const dictIds = dicts.map((x) => x.id);
export const langIds = Object.keys(langs);
export const dictsByLang = [
  {
    zh_TW: ["kisaragi_dict", "dict_concised", "dict_revised", "dict_idioms"],
  },
  {
    nan_TW: [
      "chhoetaigi_taijittoasutian",
      "moedict_twblg",
      "chhoetaigi_itaigi",
    ],
  },
  {
    hak_TW: ["hakkadict"],
  },
];

/**
 * Return array of dictionary IDs present in `word`.
 * If `full` is truthy, return dictionary objects instead.
 */
export function dictsInWord(word, full, lang) {
  let ds = dicts.filter((dict) => {
    if (lang) {
      return word[dict.id] && lang === dict.lang;
    } else {
      return word[dict.id];
    }
  });
  if (full) {
    return ds;
  } else {
    return ds.map((d) => d.id);
  }
}

// Show loading indicator after this many miliseconds.
export const showLoadingAfterMS = 250;

export const baseURL = "https://kemdict.com";

export const version = import.meta.env.KEMDICT_VERSION;

export const WordSortFns = {
  // These return numbers because that's what Array.sort wants.
  ascend: (a, b) => {
    if (a.title > b.title) {
      return 1;
    } else {
      return -1;
    }
  },
  descend: (a, b) => {
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
 * Items without `property` are grouped under `fallback`. (More
 * accurately, they are grouped under the string representation of
 * `fallback`, so eg. `false` and "false" are equivalent.)
 *
 * @param {Array<object>} arr
 * @param {string} property
 */
// This is more or less seq-group-by ported over, except the fallback part.
export function groupByProp(arr, property, fallback) {
  function reducer(acc, elt) {
    let key = elt[property];
    let cell = acc[key];
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
 * @param {string} template
 * @param {string} str
 */
export function format(template, str) {
  return str.replace(RegExp(`(${str})`), template);
}
