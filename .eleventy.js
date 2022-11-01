let EleventyServerlessBundlerPlugin;
// Do full builds for now until we really, *actually* need to avoid
// building everything
if (!process.env["DEV"]) {
  ({ EleventyServerlessBundlerPlugin } = require("@11ty/eleventy"));
}

const fs = require("node:fs");
const all_titles = JSON.parse(fs.readFileSync("src/titles.json"));

/* Return a string of an HTML link to `target`. */
/* If `target` already contains an <a> tag, return it unchanged. */
function linkToWord(target, desc = target) {
  if (target.indexOf("<a") === -1) {
    if (all_titles.includes(target)) {
      return `<a href='/word/${target}'>${desc}</a>`;
    }
  }
  return target;
}

function linkify_brackets(str) {
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

function process_def_concised(def) {
  if (def) {
    return `<ol><p class="def">
    ${def
      .split("\n")
      .map((d) => `<li><p class="def">${d.replace(/^\d+\./, "")}</p></li>`)
      .join("")}
</p></ol>`;
  } else {
    return def;
  }
}

function process_def_moedict_zh(def) {
  if (def) {
    def = linkify_brackets(def);
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
    def = def.replace(/<(.*?)>/g, (_m, $1) => `${linkToWord($1)}`);
    def = linkify_brackets(def);
  }
  return def;
}

module.exports = (cfg) => {
  cfg.addFilter("spc", (def) => {
    return def.replace(/　/g, " ");
  });
  cfg.addFilter("process_def_moedict_zh", process_def_moedict_zh);
  cfg.addFilter("process_def_concised", process_def_concised);
  cfg.addFilter(
    "interlinear_annotation_to_ruby",
    interlinear_annotation_to_ruby
  );
  cfg.addFilter("process_def_kisaragi", process_def_kisaragi);

  cfg.addPassthroughCopy("src/s.js");
  cfg.addPassthroughCopy("src/titles.json");

  if (EleventyServerlessBundlerPlugin) {
    cfg.addPlugin(EleventyServerlessBundlerPlugin, {
      name: "serverless",
      functionsDir: "./netlify/functions/",
      redirects: "netlify-toml-builders",
      copy: ["./src/titles.json"],
    });
  }

  return {
    dir: {
      input: "src",
      output: "_site",
    },
  };
};
