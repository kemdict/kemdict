import fs from "node:fs";

import { defineConfig } from "astro/config";
import svelte from "@astrojs/svelte";
import mdx from "@astrojs/mdx";
import node from "@astrojs/node";
import sitemap from "@astrojs/sitemap";

// TODO: icons with https://github.com/antfu/unplugin-icons

const baseURL = "https://kemdict.com";

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
    plugins: [legacy()],
    clearScreen: false,
    envPrefix: "KEMDICT_",
    ssr: { noExternal: ["sqlstring"] },
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
