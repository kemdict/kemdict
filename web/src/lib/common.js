// This also defines the order in the word page
export const dicts = {
  kisaragi_dict: "如月的現代台灣華語補足典",
  dict_concised: "教育部國語辭典簡編本",
  dict_revised: "教育部重編國語辭典",
  moedict_twblg: "教育部臺灣閩南語常用詞辭典",
  hakkadict: "教育部臺灣客家語常用詞辭典",
  dict_idioms: "教育部成語典",
};

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
