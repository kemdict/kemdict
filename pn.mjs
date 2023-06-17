/**
 * Normalize pronunciation `pn`.
 *
 * If `pn` is an object, get the string from its value in \"zh-Hant\".
 *
 * Return a list of normalized strings. This is because some
 * pronunciation strings include multiple pronunciations.
 *
 * @param {string | Record<string,string>} pn
 * @returns Array<string>
 */
export function pnNormalize(pn) {
  let p;
  if (typeof pn !== "string") {
    p = pn["zh-Hant"];
  } else {
    p = pn;
  }
  return (
    p
      .normalize("NFD")
      .replace("　", " ")
      // The replacement character, which appears in one entry in
      // itaigi.
      // https://itaigi.tw/k/%E5%8D%88%E5%AE%89/
      // I'm pretty sure it's not supposed to be there.
      .replace("\uFFFD", "")
      .replace("（變）", "/")
      .split(/[ \t\n\r]*\/[ \t\n\r]*/)
      .filter((x) => x !== "")
  );
}

/**
 * Normalize `pn` such that it is searchable with an ASCII keyboard.
 * @param {string} pn
 * @returns string
 */
export function pnToInputForm(pn) {
  const arr = [...pn.replace("ⁿ", "nn").normalize("NFKD")];
  return arr.filter((x) => x.codePointAt() < 128).join("");
}

/**
 * Collect pronunciations from heteronym object HET.
 * Return them as a map from type to pronunciation.
 * @param {Heteronym} het
 */
export function pnCollect(het) {
  const keys = [
    // kemdict-data-ministry-of-education
    "bopomofo" /* "pinyin" */,
    // moedict-twblg
    "trs",
    // kisaragi-dict
    "pronunciation",

    // hakkadict
    "p_四縣",
    "p_海陸",
    "p_大埔",
    "p_饒平",
    "p_詔安",
    "p_南四縣",

    // chhoetaigi-itaigi (keys are defined in Makefile
    // in this repository)
    "poj",
    "kip",

    "kMandarin",
  ];
  /**
   * @type Map<string,string>
   */
  const ret = new Map([]);
  for (const key of keys) {
    if (het.props[key]) {
      for (const p of pnNormalize(het.props[key])) {
        ret.set(key, p);
      }
    }
  }
  return ret;
}
