module.exports = function (eleventyConfig) {
  eleventyConfig.addShortcode("word_moedict_zh", function (word) {
    return <div class="word"></div>;
  });
};
