/**
 * Normalize spaces in `str`.
 *
 * @param {string} str
 * @returns {string}
 */
export function spc(str) {
  if (typeof str !== "string") {
    throw new TypeError("str must be a string");
  }
  if (str) {
    return str.replace(/　/g, " ");
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
