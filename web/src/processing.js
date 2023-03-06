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
    let first = true;
    for (let i = 0; i < defs.length; i++) {
      if (defs[i]) {
        // Deal with the interlinear annotation characters.
        let matches = [...defs[i].matchAll("\ufff9|\ufffa|\ufffb")];
        if (matches.length !== 0 && matches.length % 3 === 0) {
          defs[i] = defs[i]
            .replace(
              /\ufff9/g,
              first ? "<blockquote><p>" : "</blockquote><blockquote><p>"
            )
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
        first = false;
      }
    }
    defs = defs.join("\n");
  }
  return defs;
}

export function radicals_and_strokes(props) {
  if (!props.radical && !props.stroke_count) return;
  let x = "";
  x += `<div class="mb-4">【`;
  if (props.radical) {
    x += `<a href="/word/${props.radical}">${props.radical}</a>部`;
  }
  if (props.stroke_count) {
    x += `，共${props.stroke_count}畫`;
  }
  x += "】</div>";
  return x;
}
