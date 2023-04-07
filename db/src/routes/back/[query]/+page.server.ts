import { getHeteronyms } from "$lib/server/db";

export function load({ url, params }) {
  const mtch = url.searchParams.get("m");
  return {
    data: getHeteronyms(params.query, mtch),
    params: { query: params.query },
    searchParams: { m: mtch },
  };
}
