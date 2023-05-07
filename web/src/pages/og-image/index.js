import { readFileSync } from "node:fs";
import satori from "satori";
import sharp from "sharp";

const openHuninn = readFileSync(
  `${process.cwd()}/public/jf-openhuninn-1.1.ttf`
);
const Jost = readFileSync(`${process.cwd()}/public/Jost-400-Book.ttf`);
const JostBold = readFileSync(`${process.cwd()}/public/Jost-700-Bold.ttf`);

const makeElemFunc =
  (elem) =>
  (props, ...children) => {
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
export async function get({ request }) {
  const title = new URL(request.url).searchParams.get("title");
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
          name: "Jost",
          data: Jost,
          weight: 400,
          style: "normal",
        },
        {
          name: "Jost",
          data: JostBold,
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
