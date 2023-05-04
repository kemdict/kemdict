module.exports = (api) => {
  api.cache(true);
  return {
    presets: ["babel-preset-expo"],
    plugins: [
      [
        "module-resolver",
        {
          extensions: [".js", ".jsx", ".es", ".es6", ".mjs", ".ts"],
        },
      ],
    ],
  };
};
