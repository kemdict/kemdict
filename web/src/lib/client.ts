export async function clientGetCompletions(prefix: string) {
  if (prefix.trim() === "") return [];
  // work around Astro middleware failing for some URLs
  // Namely, it assumes the input URL can be successfully decoded with
  // `decodeURI`, but if I give it "/%" decodeURI will fail, causing an
  // error.
  // Astro 5.7.2, @astrojs/node 9.2.0
  // This is an Astro issue because any user can type /api/compl/prefix/% into
  // the API and trigger the error. It doesn't bring down the server, but
  // there's nothing I can do for the error.
  const res = await fetch(encodeURI(`/api/compl/prefix/${prefix}`));
  const text = await res.text();
  return JSON.parse(text) as string[];
}
