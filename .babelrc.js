module.exports = {
  presets: ["@babel/preset-typescript"],
  plugins: [
    "@babel/plugin-syntax-jsx",
    [
      "@babel/plugin-transform-react-jsx",
      {
        pragma: "(new CommonDOMRenderer()).create",
        pragmaFrag: "(new CommonDOMRenderer()).fragment",
      },
    ],
    [
      "@wordpress/babel-plugin-import-jsx-pragma",
      {
        // HACK HACK HACK but I can't do anything about it
        scopeVariable: "{CommonDOMRenderer}",
        scopeVariableFrag: "{CommonDOMRenderer}",
        source: "render-jsx/dom",
        isDefault: "false",
      },
    ],
    // "@babel/plugin-transform-modules-commonjs",
    // [
    //   "module:babel-jsx-pragma-module-auto-import",
    //   {
    //     path: "render-jsx/dom",
    //     name: "CommonDOMRenderer",
    //   },
    // ],
  ],
};
