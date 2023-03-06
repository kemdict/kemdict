import fs from "node:fs";

import { defineConfig } from "astro/config";
import svelte from "@astrojs/svelte";
import mdx from "@astrojs/mdx";
import node from "@astrojs/node";
import sitemap from "@astrojs/sitemap";

const baseURL = "https://kemdict.com";

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
  vite: {
    clearScreen: false,
    envPrefix: "KEMDICT_",
    build: {
      minify: "terser",
    },
  },
});
