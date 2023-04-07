import type { Config } from "tailwindcss";

const cjkFallback = [
  "'Noto Sans CJK TC'",
  "'Noto Sans TC'",
  "Microsoft Jhenghei",
  "Microsoft Yahei",
  "Meiryo",
  "Malgun Gothic",
];

export default {
  // darkMode: "class",
  content: ["./src/**/*.{svelte,html}"],
  safelist: ["visible", "invisible"],
  theme: {
    fontFamily: {
      sans: ["Noto Sans", ...cjkFallback, "sans-serif"],
    },
  },
} satisfies Config;
