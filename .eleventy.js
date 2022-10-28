const { EleventyServerlessBundlerPlugin } = require("@11ty/eleventy");

/* Return a string of an HTML link to `target`. */
/* If `target` already contains an <a> tag, return it unchanged. */
function linkToWord(target, desc = target) {
  if (target.indexOf("<a") === -1) {
    return `<a href='${target}'>${desc}</a>`;
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
  cfg.addFilter("moedict_twblg_process_defs", (defs) => {
    if (defs) {
      if (typeof defs === "string") {
        defs = [defs];
      }
      for (let i = 0; i < defs.length; i++) {
        if (defs[i]) {
          let strs = defs[i].split("\uFFF9");
          for (let j = 0; j < strs.length; j++) {
            strs[j] = `<p>${strs[j]}</p>`;
          }
          defs[i] = strs;
        }
      }
      defs = defs.join("\n");
    }
    return defs;
  });
  cfg.addFilter("kisaragi_process_def", (def) => {
    if (def) {
      def = def.replace(/<(.*?)>/g, `${linkToWord("$1")}`);
      def = def.replace(/「(.*?)」一詞/g, `「${linkToWord("$1")}」一詞`);
      def = def.replace(
        /(同|參見|亦寫做)「(.*?)」/g,
        `$1「${linkToWord("$2")}」`
      );
    }
    return def;
  });

  cfg.addPassthroughCopy("src/s.js");

  cfg.addPlugin(EleventyServerlessBundlerPlugin, {
    name: "serverless",
    functionsDir: "./netlify/functions/",
    redirects: "netlify-toml-builders",
  });

  return {
    dir: {
      input: "src",
      output: "_site",
    },
  };
};
