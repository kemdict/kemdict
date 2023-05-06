import { resolve } from "node:path";
import dts from "vite-plugin-dts";

export default {
  plugins: [dts({ copyDtsFiles: true, skipDiagnostics: true })],
  build: {
    lib: {
      entry: [resolve(__dirname, "index.ts"), resolve(__dirname, "db.ts")],
      name: "common",
    },
    rollupOptions: {
      external: [],
      output: { globals: {} },
    },
  },
};
