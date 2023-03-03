import { defineConfig } from "astro/config";
import astroSvelte from "@astrojs/svelte";
import node from "@astrojs/node";

export default defineConfig({
  integrations: [astroSvelte()],
  output: "server",
  adapter: node({
    mode: "standalone",
  }),
});
