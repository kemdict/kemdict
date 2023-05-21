import type { Config } from "tailwindcss";
import { join } from "node:path";

const cjkFallbackSans = [
  "'Noto Sans CJK TC'",
  "'Noto Sans TC'",
  "'jf-openhuninn'",
  "'jf-openhuninn-1.1'",
  "Microsoft Jhenghei",
  "Microsoft Yahei",
  "Meiryo",
  "Malgun Gothic",
];

const cjkFallbackSerif = [
  "'Noto Serif CJK TC'",
  "'Noto Serif TC'",
  "'Iansui 094'",
  "HanaMinA",
  "HanaMinB",
  "花園明朝A",
  "花園明朝B",
  "'Droid Serif'",
];

export default {
  darkMode: "class",
  content: [
    "./src/**/*.{svelte,md,ts,js,html,astro,mdx}",
    join(
      require.resolve("@skeletonlabs/skeleton"),
      "../**/*.{html,js,svelte,ts}"
    ),
  ],
  safelist: ["visible", "invisible"],
  theme: {
    fontFamily: {
      sans: [
        "Source Sans Pro",
        "Roboto",
        "Segoe UI",
        ...cjkFallbackSans,
        ...cjkFallbackSerif,
        "sans-serif",
      ],
      serif: [
        "Noto Serif",
        ...cjkFallbackSerif,
        ...cjkFallbackSans,
        "sans-serif",
      ],
      mono: ["monospace"],
    },
    extend: {
      colors: {
        surface: {
          50: "#f9faff",
          100: "#ececfd",
          200: "#e7e7fd",
          300: "#d8d9fb",
          400: "#bbbdf9",
          500: "#9ea0f6",
          600: "#8e90dd",
          700: "#7778b9",
          800: "#5f6094",
          900: "#4d4e79",
          950: "#0B0F14",
        },
      },
    },
  },
  plugins: [
    require("@tailwindcss/typography"),
    ...require("@skeletonlabs/skeleton/tailwind/skeleton.cjs")(),
  ],
} satisfies Config;
