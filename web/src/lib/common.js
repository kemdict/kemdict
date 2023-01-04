// This also defines the order in the word page
export const dicts = [
  {
    id: "kisaragi_dict",
    name: "如月的現代台灣華語補足典",
    url: "/dict-kisaragi",
    lang: "zh",
  },
  {
    id: "dict_concised",
    name: "教育部國語辭典簡編本",
    url: "https://dict.concised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh",
  },
  {
    id: "dict_revised",
    name: "教育部重編國語辭典",
    url: "https://dict.revised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh",
  },
  {
    id: "moedict_twblg",
    name: "教育部臺灣閩南語常用詞辭典",
    url: "https://twblg.dict.edu.tw/holodict_new/result_main.jsp?radiobutton=1&limit=20&querytarget=1&sample=$1",
    lang: "taigi",
  },
  {
    id: "hakkadict",
    name: "教育部臺灣客家語常用詞辭典",
    url: "https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi/ccd=qwMPHD/search?dcf=sti&extrasearch=es1&qs0=$1",
    lang: "hakka",
  },
  {
    id: "dict_idioms",
    name: "教育部成語典",
    url: "https://dict.idioms.moe.edu.tw/idiomList.jsp?idiom=$1",
    lang: "zh",
  },
];

export const dictIds = dicts.map((x) => x.id);

export function dictsInWord(word) {
  return dictIds.filter((id) => word[id]);
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
  return arr.reduce((acc, elt) => {
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
  }, {});
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
