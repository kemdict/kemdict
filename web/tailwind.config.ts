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
      // Prose customizations are scattered around here, in src.css as
      // "@apply prose-<thing>:<utility>;" statements, and as nested
      // CSS declarations.
      //
      // This mess would be less painful if we just <expletive> fork
      // @tailwindcss/typography, considering how simple the source
      // styles.js is, but I wasn't able to get Astro to use the fork.
      // For some reason. Even though it's installed. Words alone do
      // not convey how insanity inducing it was, or how much I had
      // tried and double-checked.
      typography: {
        DEFAULT: {
          css: {
            p: { marginTop: "0.25em", marginBottom: "0.25em" },
            blockquote: {
              "p:first-of-type::before": { content: "none" },
              "p:first-of-type::after": { content: "none" },
              p: { marginTop: "0", marginBottom: "0" },
            },
          },
        },
      },
      colors: {
        surface: {
          50: "#f7f4fa",
          100: "#f4f1f8",
          200: "#f2edf6",
          300: "#e9e2f1",
          400: "#d9cde7",
          500: "#c9b7dc",
          600: "#b5a5c6",
          700: "#9789a5",
          800: "#796e84",
          900: "#625a6c",
          950: "#302c35",
        },
      },
    },
  },
  plugins: [
    require("@tailwindcss/typography"),
    require("@tailwindcss/forms"),
    ...require("@skeletonlabs/skeleton/tailwind/skeleton.cjs")(),
  ],
} satisfies Config;
