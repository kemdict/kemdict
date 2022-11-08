import { mdsvex } from "mdsvex";
import adapter from "@sveltejs/adapter-static";
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
    adapter: adapter({ fallback: "200.html" }),
    trailingSlash: "always",
  },
};

export default config;
