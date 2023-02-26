import { sveltekit } from "@sveltejs/kit/vite";

export default {
  plugins: [sveltekit()],
  server: { fs: { allow: [".."] } },
  clearScreen: false,
  envPrefix: "KEMDICT_",
  build: {
    minify: "terser",
  },
};
