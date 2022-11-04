let EleventyServerlessBundlerPlugin;
// Do full builds for now until we really, *actually* need to avoid
// building everything
// if (!process.env["DEV"]) {
//   ({ EleventyServerlessBundlerPlugin } = require("@11ty/eleventy"));
// }

const fs = require("node:fs");
const { spawnSync } = require("node:child_process");
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

/**
 * Take a word list like "a,b,c" and format it with links.
 */
function comma_word_list(str) {
  return str
    .split(",")
    .map((x) => `「${linkToWord(x)}」`)
    .join("、");
}

function idioms_nuance(str) {
  /* Sure... */
  return `<table>${str
    .split("\n")
    .map((line) => {
      let s = line.split(",");
      if (s.length == 2) {
        return `<tr>${s
          .map((x) => `<th>${x.replace("ㄨ", "☓")}</th>`)
          .join("")}<th>例句</th></tr>`;
      } else {
        return `<tr>${s
          .map((x) => `<td>${x.replace("ㄨ", "☓")}</td>`)
          .join("")}</tr>`;
      }
    })
    .join("")}</table>`;
}

function idioms_source(str) {
  return str
    .split("\n")
    .map((x) => x.replace(/^\*(\d)\*(.*)/, `$2<a href="#sc$1">$1</a>`))
    .join("");
}

function idioms_source_comment(str) {
  if (str) {
    return linkify_brackets(
      `<ol>
    ${str
      .split("\n")
      .map(
        (d, i) =>
          `<li id="sc${i + 1}">${d.replace(
            /^\d+\./,
            ""
          )}<a href="#isc">↩</a></li>`
      )
      .join("")}
</ol>`
    );
  } else {
    return str;
  }
}

function newline_string_to_ol(str) {
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

function process_def_revised(def) {
  if (def) {
    let x = "";
    let match;
    for (const d of def.split("\n")) {
      if ((match = d.match(/^\[(.*)\]$/))) {
        if (x.length > 0) {
          x += `</ol>`;
        }
        x += `<p class="pos">${match[1]}</p><ol>`;
      } else {
        if (x.length == 0) {
          x += `<ol>`;
        }
        x += `<li><p class="def">${d.replace(/^\d+\./, "")}</p></li>`;
      }
    }
    x += "</ol>";
    return linkify_brackets(x);
  } else {
    return def;
  }
}

function process_def_concised(def) {
  if (def) {
    return `<ol>
    ${def
      .split("\n")
      .map((d) => `<li><p class="def">${d.replace(/^\d+\./, "")}</p></li>`)
      .join("")}
</ol>`;
  } else {
    return def;
  }
}

function interlinear_annotation(defs) {
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

function process_def_kisaragi(def) {
  if (def) {
    def = def.replace(/<(.*?)>/g, (_m, $1) => `${linkToWord($1)}`);
    def = linkify_brackets(def);
  }
  return def;
}

const getVersion = (() => {
  let version;
  return () => {
    if (!version) {
      let tag = spawnSync("git", ["describe", "--tags"])
        .stdout.toString()
        .trim();
      let date = spawnSync("git", ["show", "-s", "--format=%ci", "HEAD"])
        .stdout.toString()
        .trim()
        .replace("-", "年")
        .replace("-", "月")
        .replace(" ", "日")
        .substring(0, 11);
      version = `${tag} (${date})`;
    }
    return version;
  };
})();

function process_def_idioms(def) {
  return linkify_brackets(def.replace(/<a name.*/, ""));
}

module.exports = (cfg) => {
  cfg.addFilter("spc", (def) => {
    return def.replace(/　/g, " ");
  });
  cfg.addFilter("idioms_nuance", idioms_nuance);
  cfg.addFilter("comma_word_list", comma_word_list);
  cfg.addFilter("idioms_source", idioms_source);
  cfg.addFilter("idioms_source_comment", idioms_source_comment);
  cfg.addFilter("newline_string_to_ol", newline_string_to_ol);
  cfg.addFilter("process_def_concised", process_def_concised);
  cfg.addFilter("process_def_revised", process_def_revised);
  cfg.addFilter("linkToWord", linkToWord);
  cfg.addFilter("linkify_brackets", linkify_brackets);
  cfg.addFilter("process_def_idioms", process_def_idioms);
  cfg.addFilter("interlinear_annotation", interlinear_annotation);
  cfg.addFilter("process_def_kisaragi", process_def_kisaragi);

  cfg.addShortcode("getVersion", getVersion);

  cfg.addPassthroughCopy("src/s.js");
  cfg.addPassthroughCopy("src/titles.json");
  cfg.addPassthroughCopy({
    "dicts/ministry-of-education/license": "l",
  });

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
