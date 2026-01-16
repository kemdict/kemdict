// vitePreprocess from vite-plugin-svelte doesn't seem to support
// Babel.
import { sveltePreprocess } from "svelte-preprocess";

export default {
  preprocess: sveltePreprocess(),
};
