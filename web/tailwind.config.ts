import type { Config } from "tailwindcss";
import { join } from "node:path";

import { skeleton } from "@skeletonlabs/tw-plugin";

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
      "../**/*.{html,js,svelte,ts}",
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
    require("@tailwindcss/forms")({
      // I'm pretty happy with the way text input is styled, but it
      // changes if I apply the base changes. Use opt-in style instead.
      strategy: "class",
    }),
    skeleton({
      themes: {
        custom: [
          {
            name: "kemdict-theme",
            properties: {
              "--theme-font-color-base": "0 0 0",
              "--theme-font-color-dark": "255 255 255",
              "--theme-rounded-base": "4px",
              "--theme-rounded-container": "8px",
              "--theme-border-base": "1px",
              "--on-primary": "0 0 0",
              "--on-secondary": "0 0 0",
              "--on-tertiary": "0 0 0",
              "--on-success": "0 0 0",
              "--on-warning": "0 0 0",
              "--on-error": "255 255 255",
              "--on-surface": "0 0 0",
              "--color-primary-50": "247 244 250",
              "--color-primary-100": "244 241 248",
              "--color-primary-200": "242 237 246",
              "--color-primary-300": "233 226 241",
              "--color-primary-400": "217 205 231",
              "--color-primary-500": "201 183 220",
              "--color-primary-600": "181 165 198",
              "--color-primary-700": "151 137 165",
              "--color-primary-800": "121 110 132",
              "--color-primary-900": "98 90 108",
              "--color-secondary-50": "252 250 252",
              "--color-secondary-100": "251 248 252",
              "--color-secondary-200": "250 247 251",
              "--color-secondary-300": "247 241 248",
              "--color-secondary-400": "242 231 243",
              "--color-secondary-500": "236 221 238",
              "--color-secondary-600": "212 199 214",
              "--color-secondary-700": "177 166 179",
              "--color-secondary-800": "142 133 143",
              "--color-secondary-900": "116 108 117",
              "--color-tertiary-50": "253 251 253",
              "--color-tertiary-100": "253 250 252",
              "--color-tertiary-200": "252 249 252",
              "--color-tertiary-300": "250 245 249",
              "--color-tertiary-400": "247 238 245",
              "--color-tertiary-500": "243 230 241",
              "--color-tertiary-600": "219 207 217",
              "--color-tertiary-700": "182 173 181",
              "--color-tertiary-800": "146 138 145",
              "--color-tertiary-900": "119 113 118",
              "--color-success-50": "237 247 220",
              "--color-success-100": "230 245 208",
              "--color-success-200": "224 242 197",
              "--color-success-300": "206 235 162",
              "--color-success-400": "169 219 92",
              "--color-success-500": "132 204 22",
              "--color-success-600": "119 184 20",
              "--color-success-700": "99 153 17",
              "--color-success-800": "79 122 13",
              "--color-success-900": "65 100 11",
              "--color-warning-50": "252 244 218",
              "--color-warning-100": "251 240 206",
              "--color-warning-200": "250 236 193",
              "--color-warning-300": "247 225 156",
              "--color-warning-400": "240 202 82",
              "--color-warning-500": "234 179 8",
              "--color-warning-600": "211 161 7",
              "--color-warning-700": "176 134 6",
              "--color-warning-800": "140 107 5",
              "--color-warning-900": "115 88 4",
              "--color-error-50": "249 221 234",
              "--color-error-100": "246 209 228",
              "--color-error-200": "244 198 221",
              "--color-error-300": "238 163 200",
              "--color-error-400": "225 94 159",
              "--color-error-500": "212 25 118",
              "--color-error-600": "191 23 106",
              "--color-error-700": "159 19 89",
              "--color-error-800": "127 15 71",
              "--color-error-900": "104 12 58",
              "--color-surface-50": "247 244 250",
              "--color-surface-100": "244 241 248",
              "--color-surface-200": "242 237 246",
              "--color-surface-300": "233 226 241",
              "--color-surface-400": "217 205 231",
              "--color-surface-500": "201 183 220",
              "--color-surface-600": "181 165 198",
              "--color-surface-700": "151 137 165",
              "--color-surface-800": "121 110 132",
              "--color-surface-900": "98 90 108",
            },
          },
        ],
      },
    }),
  ],
} satisfies Config;
