module.exports = {
  darkMode: "class",
  content: ["./src/**/*.{svelte,md,ts,js,html,astro,mdx}"],
  safelist: ["visible", "invisible"],
  theme: {
    fontFamily: {
      sans: [
        "'Noto Sans CJK TC'",
        "'Noto Sans TC'",
        "'jf-openhuninn'",
        "'jf-openhuninn-1.1'",
        "'Iansui 094'",
        "Microsoft Jhenghei",
        "Microsoft Yahei",
        "Meiryo",
        "Malgun Gothic",
        "HanaMinA",
        "HanaMinB",
        "花園明朝A",
        "花園明朝B",
        "sans-serif",
      ],
    },
    extend: {
      // https://github.com/tailwindlabs/tailwindcss/discussions/1361
      boxShadow: {
        DEFAULT: "0 0 0.25rem #00000040",
        md: "0 0 0.25rem #00000070",
        white: "0 0 0.5rem #ffffff",
      },
    },
  },
};
