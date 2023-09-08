import fs from "node:fs";

import { defineConfig } from "astro/config";
import svelte from "@astrojs/svelte";
import mdx from "@astrojs/mdx";
import node from "@astrojs/node";
import sitemap from "@astrojs/sitemap";

const baseURL = "https://kemdict.com";
const prod = import.meta.env.PROD;

import legacy from "@vitejs/plugin-legacy";
import { purgeCss } from "vite-plugin-tailwind-purgecss";

export default defineConfig({
  compressHTML: prod,
  redirects: to302({
    "/dict-itaigi": "/dicts/chhoetaigi_itaigi",
    "/dict-kisaragi": "/dicts/kisaragi_dict",
    "/dict-moe": "/dicts/dict_concised",
    "/dict-taijittoasutian": "/dicts/chhoetaigi_taijittoasutian",
  }),
  site: baseURL,
  integrations: [
    svelte(),
    mdx(),
    sitemap({
      customPages: [
        `${baseURL}/`,
        ...fs
          .readdirSync("src/pages")
          .filter((path) => path.match(/mdx?$/))
          .map((path) => `${baseURL}/${path.replace(/\.mdx?$/, "")}`),
      ],
    }),
  ],
  output: "server",
  adapter: node({
    mode: "standalone",
  }),
  server: {
    port: 5173,
  },
  vite: {
    plugins: [legacy(), purgeCss()],
    clearScreen: false,
    envPrefix: "KEMDICT_",
    ssr: {
      noExternal: [...(prod ? ["sqlstring"] : [])],
    },
    resolve: {
      alias: [
        {
          find: /^\$src\/(.*)/,
          replacement: `${process.cwd()}/src/$1`,
        },
        {
          find: /^\$lib\/(.*)/,
          replacement: `${process.cwd()}/src/lib/$1`,
        },
        {
          find: /^common$/,
          replacement: `${process.cwd()}/src/lib/common`,
        },
      ],
    },
  },
});

/**
 * Ensure `redirects` all use 302 as the status code.
 *
 * 301 is IMO a questionable default as it is meant to be permanent,
 * and is sometimes actually permanent.
 */
function to302(redirects: Record<string, string>) {
  const tmp: Record<string, { status: 302; destination: string }> = {};
  for (const [k, v] of Object.entries(redirects)) {
    tmp[k] = { status: 302, destination: v };
  }
  return tmp;
}
