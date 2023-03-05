import { defineConfig } from "astro/config";
import svelte from "@astrojs/svelte";
import mdx from "@astrojs/mdx";
import node from "@astrojs/node";

export default defineConfig({
  integrations: [svelte(), mdx()],
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
