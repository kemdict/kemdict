import type { APIContext } from "astro";
import { readFileSync, existsSync } from "node:fs";
import satori from "satori";
import sharp from "sharp";

/**
 * Find the file path for fontFileName.
 */
function findFont(fontFileName: string): string | undefined {
  return [
    `${process.cwd()}/server`, // during development
    `${process.cwd()}/public`, // during development
    process.cwd(), // when deployed, running from dist/
    `${process.cwd()}/client`, // during development
  ]
    .map((d) => `${d}/${fontFileName}`)
    .find((f) => existsSync(f));
}
const openHuninn = readFileSync(findFont("jf-openhuninn-1.1.ttf"));
const SourceSans = readFileSync(findFont("SourceSans3-Regular.ttf"));
const SourceSansBold = readFileSync(findFont("SourceSans3-Bold.ttf"));

const makeElemFunc =
  (elem: string) =>
  (props: any, ...children) => {
    if (typeof props === "string") {
      props = { tw: props };
    }
    return { type: elem, props: { ...props, children } };
  };
const div = makeElemFunc("div");
const span = makeElemFunc("span");
const img = makeElemFunc("img");

// <img class="rounded-xl"
//      src="" width=75 height=75>
export async function get({ request }: APIContext) {
  const encoded = new URL(request.url).searchParams.get("title");
  const title = encoded && decodeURI(encoded);
  const svg = await satori(
    div(
      {
        lang: "zh_TW",
        tw: "flex flex-col w-full h-full items-center justify-center",
      },
      div(
        "flex items-center",
        img({
          tw: "rounded-xl",
          src: "https://kemdict.com/maskable_icon_x192.png",
          height: "75",
          width: "75",
        }),
        span({ tw: "font-bold text-4xl ml-2" }, "Kemdict")
      ),
      ...(title ? [span("text-6xl mt-8", title)] : [])
    ),
    {
      width: 1200,
      height: 628,
      fonts: [
        {
          name: "Source Sans 3",
          data: SourceSans,
          weight: 400,
          style: "normal",
        },
        {
          name: "Source Sans 3",
          data: SourceSansBold,
          weight: 700,
          style: "normal",
        },
        {
          name: "jf-openhuninn-1.1",
          data: openHuninn,
          style: "normal",
        },
      ],
    }
  );
  const pngBuf = await sharp(Buffer.from(svg)).png().toBuffer();
  return new Response(pngBuf, {
    status: 200,
    headers: {
      "Content-Type": "image/png",
    },
  });
}
