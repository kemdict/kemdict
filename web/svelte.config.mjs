// vitePreprocess from vite-plugin-svelte doesn't seem to support
// Babel.
import preprocess from "svelte-preprocess";

export default {
  preprocess: preprocess({
    babel: {
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
    },
  }),
};
