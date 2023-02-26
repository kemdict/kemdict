import { mdsvex } from "mdsvex";
import adapterNode from "@sveltejs/adapter-node";
import preprocess from "svelte-preprocess";

const config = {
  extensions: [".svelte", ".svelte.md", ".svx"],
  preprocess: [
    preprocess(),
    mdsvex({
      smartypants: {
        dashes: "oldschool",
      },
      extensions: [".svelte.md", ".svx"],
      layout: "src/lib/MarkdownLayout.svelte",
    }),
  ],

  kit: {
    adapter: adapterNode({
      out: "built-node",
    }),
    // New in SvelteKit 1.8.4
    output: {
      preloadStrategy: "preload-mjs",
    },
    version: {
      name: process.env.KEMDICT_VERSION,
    },
  },
};

export default config;
