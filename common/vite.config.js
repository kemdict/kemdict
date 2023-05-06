import { resolve } from "node:path";
// import dts from "vite-plugin-dts";

export default {
  // plugins: [dts({ skipDiagnostics: true })],
  build: {
    lib: {
      entry: resolve(__dirname, "index.ts"),
      name: "common",
      fileName: "index",
    },
    rollupOptions: {
      external: [],
      output: { globals: {} },
    },
  },
};
