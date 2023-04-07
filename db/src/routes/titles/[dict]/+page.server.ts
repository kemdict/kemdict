import { getDictTitles } from "$lib/server/db";

export function load({ url, params }) {
  const limit = url.searchParams.get("limit") || undefined;
  return {
    data: getDictTitles(params.dict, limit),
    params: { dict: params.dict },
    searchParams: { limit },
  };
}
