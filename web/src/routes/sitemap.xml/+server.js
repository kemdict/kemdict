// I used https://scottspence.com/posts/make-a-sitemap-with-sveltekit
// as a general reference.

import { baseURL } from "$lib/common";

// Root in Vite's import.meta.glob is project root (where you started Vite).
const posts = Object.entries(
  import.meta.glob("/src/routes/**/*.md", { eager: true })
).map(([path, { metadata }]) => ({ path: path, metadata: metadata }));

function toSlug(srcPath) {
  return encodeURI(srcPath.replace(/\/src\/routes\/(.*)\/\+page.*/, "$1"));
}

function toUrlElem(post) {
  return `
<url>
<loc>${baseURL}/${toSlug(post.path)}</loc>
</url>`.trim();
}

export async function GET() {
  return new Response(
    `
    <?xml version="1.0" encoding="UTF-8" ?>
    <urlset xmlns="https://www.sitemaps.org/schemas/sitemap/0.9">
    ${posts.map(toUrlElem).join("")}
    </urlset>`.trim(),
    {
      headers: {
        "Content-Type": "application/xml",
      },
    }
  );
}
