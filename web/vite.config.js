import { sveltekit } from "@sveltejs/kit/vite";

const config = {
  plugins: [sveltekit()],
  server: { fs: { allow: [".."] } },
  clearScreen: false,
};

export default config;
