import { mdsvex } from "mdsvex";
import adapterNetlify from "@sveltejs/adapter-netlify";
import adapterNode from "@sveltejs/adapter-node";
import preprocess from "svelte-preprocess";

// Do this so we can choose between them from Make
let adapter;
if (process.env.ADAPTER === "node") {
  adapter = adapterNode({
    out: "built-node",
  });
} else {
  adapter = adapterNetlify();
}

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
    adapter: adapter,
  },
};

export default config;
