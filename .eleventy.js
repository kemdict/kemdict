module.exports = (cfg) => {
  cfg.addFilter("moedict_zh_process_def", (def) => {
    return def.replace(/「(.*?)」/g, "「<a href='/word/$1'>$1</a>」");
  });

  cfg.addPassthroughCopy("src/s.js");

  return {
    dir: {
      input: "src",
      output: "_site",
    },
  };
};
