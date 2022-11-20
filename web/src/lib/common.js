export const dicts = {
  dict_concised: "教育部國語辭典簡編本",
  dict_revised: "教育部重編國語辭典",
  moedict_twblg: "教育部臺灣閩南語常用詞辭典",
  hakkadict: "教育部臺灣客家語常用詞辭典",
  dict_idioms: "教育部成語典",
  kisaragi_dict: "如月的現代台灣華語補足典",
};

/**
 * Normalize spaces in `str`.
 *
 * @param {string} str
 * @returns {string}
 */
export function spc(str) {
  if (str) {
    return str.replace(/　/g, " ");
  }
}

import titles from "../../../dicts/titles.json";

/** Return a string of an HTML link to `target`.
 * If `target` already contains an <a> tag, return it unchanged.
 * @param {string} target
 * @param {string} desc
 * @returns {string}
 */
export function linkToWord(target, desc = target) {
  if (target.indexOf("<a") === -1) {
    if (titles.includes(target)) {
      return `<a data-sveltekit-reload href='/word/${target}'>${desc}</a>`;
    }
  }
  return target;
}

/**
 * Create links for all 「」 brackets if applicable.
 * @param {string} str
 * @returns {string}
 */
export function linkify_brackets(str) {
  // "$1" is a normal variable here. This is a function that
  // passes its second argument to linkToWord. We do this because
  // linkToWord needs to know the word at invocation to decide
  // whether to actually link.
  if (str) {
    return str.replace(/「(.*?)」/g, (_m, $1) => `「${linkToWord($1)}」`);
  } else {
    return str;
  }
}

/**
 * Separate ilnes into ol elements.
 * @param {string} str
 * @returns {string}
 */
export function newline_string_to_ol(str) {
  if (str) {
    return `<ol>
    ${str
      .split("\n")
      .map((s) => `<li>${s.replace(/^\d+\./, "")}</li>`)
      .join("")}
</ol>`;
  } else {
    return str;
  }
}

/**
 * Deal with Unicode rubies.
 */
export function interlinear_annotation(defs) {
  if (defs) {
    if (typeof defs === "string") {
      defs = [defs];
    }
    for (let i = 0; i < defs.length; i++) {
      if (defs[i]) {
        // Deal with the interlinear annotation characters.
        let matches = [...defs[i].matchAll("\ufff9|\ufffa|\ufffb")];
        if (matches.length !== 0 && matches.length % 3 === 0) {
          defs[i] = defs[i]
            .replace(/\ufff9/g, "<p>")
            .replace(/\ufffa/g, "</p><p>")
            .replace(/\ufffb/g, "</p>");
          // This is more "correct" i.r.t. interpreting the characters
          // as marking ruby, but I don't think POJ / TL should be
          // treated as just pronunciation annotation of Han
          // characters.
          // .replace(/\ufff9/g, "<ruby>")
          // .replace(/\ufffa/g, "<rp>(</rp><rt>")
          // .replace(/\ufffb/g, "<rp>)</rp></rt></ruby><br>");
        }
      }
    }
    defs = defs.join("\n");
  }
  return defs;
}

/**
 * Take a word list like "a,b,c" and format it with links.
 */
export function comma_word_list(str) {
  return str
    .split(",")
    .map((x) => `「${linkToWord(x)}」`)
    .join("、");
}

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

// TODO: how to run this only during build?
// export const version = (() => {
//   let tag = spawnSync("git", ["describe", "--tags"]).stdout.toString().trim();
//   let date = spawnSync("git", ["show", "-s", "--format=%ci", "HEAD"])
//     .stdout.toString()
//     .trim()
//     .replace("-", "年")
//     .replace("-", "月")
//     .replace(" ", "日")
//     .substring(0, 11);
//   return `${tag} (${date})`;
// })();

export const version = "0.7.1";
