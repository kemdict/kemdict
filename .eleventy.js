module.exports = (cfg) => {
  cfg.addFilter("moedict_zh_process_def", (def) => {
    return def.replace(/參見「(.*?)」/g, "參見「<a href='../$1'>$1</a>」");
  });

  cfg.addPassthroughCopy("src/s.js");

  return {
    dir: {
      input: "src",
      output: "_site",
    },
  };
};
