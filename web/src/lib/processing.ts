/**
 * Normalize spaces in `str`.
 *
 * @param {string} str
 * @returns {string}
 */
export function spc(str: string | undefined): string | undefined {
  if (str) {
    return str.replace(/　/g, " ");
  }
}

/**
 * Separate lines into ol elements.
 */
export function newline_string_to_ol(str: undefined): undefined;
export function newline_string_to_ol(str: string): string {
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
export function interlinear_annotation(defs: undefined): undefined;
export function interlinear_annotation(defs: string | string[]): string;
export function interlinear_annotation(defs: string[]): string {
  if (defs) {
    if (typeof defs === "string") {
      defs = [defs];
    }
    let first = true;
    for (let i = 0; i < defs.length; i++) {
      if (defs[i]) {
        // Deal with the interlinear annotation characters.
        const matches = [...defs[i].matchAll(/\ufff9|\ufffa|\ufffb/g)];
        if (matches.length !== 0 && matches.length % 3 === 0) {
          defs[i] = defs[i]
            .replace(
              /\ufff9/g,
              first ? "<blockquote><p>" : "</blockquote><blockquote><p>",
            )
            .replace(/\ufffa/g, "</p><p>")
            .replace(/\ufffb([^\ufffa\ufffb\ufff9]*)/g, "</p><p>$1</p>");
          // This is more "correct" i.r.t. interpreting the characters
          // as marking ruby, but POJ / TL should not be treated or
          // shown as simply pronunciation annotatio for Han
          // characters.
          //
          // .replace(/\ufff9/g, "<ruby>")
          // .replace(/\ufffa/g, "<rp>(</rp><rt>")
          // .replace(/\ufffb/g, "<rp>)</rp></rt></ruby><br>");
        }
        first = false;
      }
    }
    return defs.join("\n");
  }
  return undefined;
}

export function radicals_and_strokes(props: {
  radical?: string;
  sc?: string;
}): string {
  if (!props.radical && !props.sc) return;
  let x = "";
  x += `<div class="mb-4">【`;
  if (props.radical) {
    x += `<a href="/radicals/${props.radical}">${props.radical}</a>部`;
  }
  if (props.sc) {
    x += `，共${props.sc}畫`;
  }
  x += "】</div>";
  return x;
}
