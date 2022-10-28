let EleventyServerlessBundlerPlugin;
if (!process.env["DEV"]) {
  ({ EleventyServerlessBundlerPlugin } = require("@11ty/eleventy"));
}

/* Return a string of an HTML link to `target`. */
/* If `target` already contains an <a> tag, return it unchanged. */
function linkToWord(target, desc = target) {
  if (target.indexOf("<a") === -1) {
    return `<a href='${target}'>${desc}</a>`;
  } else {
    return target;
  }
}

function process_def_moedict_zh(def) {
  if (def) {
    def = def.replace(/參見「(.*?)」/g, `參見「${linkToWord("$1")}」`);
    def = def.replace(/「(.*?)」一詞/g, `「${linkToWord("$1")}」一詞`);
  }
  return def;
}

function interlinear_annotation_to_ruby(defs) {
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
            .replace(/\ufff9/g, "<ruby>")
            .replace(/\ufffa/g, "<rp>(</rp><rt>")
            .replace(/\ufffb/g, "<rp>)</rp></rt></ruby><br>");
        }
      }
    }
    defs = defs.join("\n");
  }
  return defs;
}

function process_def_kisaragi(def) {
  if (def) {
    def = def.replace(/<(.*?)>/g, `${linkToWord("$1")}`);
    def = def.replace(/「(.*?)」一詞/g, `「${linkToWord("$1")}」一詞`);
    def = def.replace(
      /(同|參見|亦寫做)「(.*?)」/g,
      `$1「${linkToWord("$2")}」`
    );
  }
  return def;
}

module.exports = (cfg) => {
  cfg.addFilter("spc", (def) => {
    return def.replace(/　/g, " ");
  });
  cfg.addFilter("process_def_moedict_zh", process_def_moedict_zh);
  cfg.addFilter(
    "interlinear_annotation_to_ruby",
    interlinear_annotation_to_ruby
  );
  cfg.addFilter("process_def_kisaragi", process_def_kisaragi);

  cfg.addPassthroughCopy("src/s.js");

  if (EleventyServerlessBundlerPlugin) {
    cfg.addPlugin(EleventyServerlessBundlerPlugin, {
      name: "serverless",
      functionsDir: "./netlify/functions/",
      redirects: "netlify-toml-builders",
    });
  }

  return {
    dir: {
      input: "src",
      output: "_site",
    },
  };
};
