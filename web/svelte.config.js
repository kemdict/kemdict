import { mdsvex } from "mdsvex";
import adapterNetlify from "@sveltejs/adapter-netlify";
import preprocess from "svelte-preprocess";

const config = {
  extensions: [".svelte", ".svelte.md", ".md", ".svx"],
  preprocess: [
    preprocess(),
    mdsvex({
      smartypants: {
        dashes: "oldschool",
      },
      extensions: [".svelte.md", ".md", ".svx"],
      layout: "src/lib/MarkdownLayout.svelte",
    }),
  ],

  kit: {
    adapter: adapterNetlify(),
    // We don't need this on Netlify.
    // trailingSlash: "always",
  },
};

export default config;
