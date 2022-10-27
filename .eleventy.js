const { EleventyHtmlBasePlugin } = require("@11ty/eleventy");

module.exports = (cfg) => {
  cfg.addPlugin(EleventyHtmlBasePlugin);
  cfg.addFilter("moedict_zh_process_def", (def) => {
    return def.replace(/「(.*?)」/g, "「<a href='/word/$1'>$1</a>」");
  });
  return {
    dir: {
      input: "src",
      output: "_site",
    },
  };
};
