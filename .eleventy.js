/* Return a string of an HTML link to `target`. */
/* If `target` already contains an <a> tag, return it unchanged. */
function linkToWord(target, desc = target) {
  if (target.indexOf("<a") === -1) {
    return `<a href='../${target}'>${desc}</a>`;
  } else {
    return target;
  }
}

module.exports = (cfg) => {
  cfg.addFilter("spc", (def) => {
    return def.replace(/　/g, " ");
  });
  cfg.addFilter("moedict_zh_process_def", (def) => {
    if (def) {
      def = def.replace(/參見「(.*?)」/g, `參見「${linkToWord("$1")}」`);
      def = def.replace(/「(.*?)」一詞/g, `「${linkToWord("$1")}」一詞`);
    }
    return def;
  });
  cfg.addFilter("kisaragi_process_def", (def) => {
    if (def) {
      def = def.replace(/「(.*?)」一詞/g, `「${linkToWord("$1")}」一詞`);
      def = def.replace(/(同|參見)「(.*?)」/g, `$1「${linkToWord("$2")}」`);
    }
    return def;
  });

  cfg.addPassthroughCopy("src/s.js");

  return {
    dir: {
      input: "src",
      output: "_site",
    },
  };
};
