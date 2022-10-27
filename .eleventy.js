function linkToWord(target, str = target) {
  return `<a href='../${target}'>${str}</a>`;
}

module.exports = (cfg) => {
  cfg.addFilter("moedict_zh_process_def", (def) => {
    return def.replace(/參見「(.*?)」/g, `參見「${linkToWord("$1")}」`);
  });
  cfg.addFilter("kisaragi_process_ref", (def) => {
    return def.replace(/(同)「(.*?)」/g, `$1「${linkToWord("$2")}」`);
  });

  cfg.addPassthroughCopy("src/s.js");

  return {
    dir: {
      input: "src",
      output: "_site",
    },
  };
};
