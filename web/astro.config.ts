import fs from "node:fs";

import { defineConfig } from "astro/config";
import svelte from "@astrojs/svelte";
import mdx from "@astrojs/mdx";
import node from "@astrojs/node";
import sitemap from "@astrojs/sitemap";

// TODO: icons with https://github.com/antfu/unplugin-icons

const baseURL = "https://kemdict.com";

// Conditional require. Because lightningcss doesn't run on Android
// but I still want to be able to develop here.
const lightningcss = (() => {
  try {
    return require("vite-plugin-lightningcss");
  } catch (_e) {
    return () => undefined;
  }
})();
import babel from "vite-plugin-babel";
import legacy from "@vitejs/plugin-legacy";

export default defineConfig({
  site: baseURL,
  integrations: [
    svelte(),
    mdx(),
    sitemap({
      customPages: [
        `${baseURL}/`,
        ...fs
          .readdirSync("src/pages")
          .filter((path) => path.match(/mdx?$/))
          .map((path) => `${baseURL}/${path.replace(/\.mdx?$/, "")}`),
      ],
    }),
  ],
  output: "server",
  adapter: node({
    mode: "standalone",
  }),
  server: {
    port: 5173,
  },
  vite: {
    plugins: [
      // This only works in plain JS, not TypeScript, because
      // tsc is used for TypeScript and it isn't Babel.
      //
      // In other words, it works in Svelte and plain .js files, but
      // not in TypeScript files and Astro components (where the JS
      // inside is actually always TypeScript).
      //
      // Even if you get Babel to process TypeScript files instead,
      // Prettier and TypeScript language server still won't
      // understand it. Such is the nature of not-yet-standardized
      // language extensions.
      babel({
        plugins: [
          [
            "@babel/plugin-proposal-pipeline-operator",
            {
              proposal: "hack",
              topicToken: "%",
            },
          ],
          "@babel/plugin-proposal-do-expressions",
        ],
      }),
      legacy({ targets: "> 0.2%, Firefox ESR" }),
      lightningcss({
        browserslist: "> 0.2%, Firefox ESR",
      }),
    ],
    clearScreen: false,
    envPrefix: "KEMDICT_",
    resolve: {
      alias: [
        {
          find: /^\$src\/(.*)/,
          replacement: `${process.cwd()}/src/$1`,
        },
      ],
    },
  },
});
