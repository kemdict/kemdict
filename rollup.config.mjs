import resolve from "@rollup/plugin-node-resolve";
import babel from "@rollup/plugin-babel";
import jsx from "acorn-jsx";

export default {
  input: ".eleventy.tsx",
  output: {
    file: ".eleventy.js",
    format: "cjs",
  },
  // Get Rollup to accept the JSX/TSX while it reads the imports.
  // Then resolve it from node_modules, bundle it, throw the entire
  // thing into Babel (including ES Modules), and spit out Common JS
  // which Eleventy requires.
  // AAAAAAAA
  plugins: [
    babel({
      babelHelpers: "bundled",
      // WHY CAN'T YOU PUT IT IN THE DEFAULT IF YOU
      // EVEN EXPLICITLY MENTION IT IN THE DOCS
      extensions: [".tsx"],
    }),
    resolve(),
  ],
  acornInjectPlugins: [jsx()],
};
