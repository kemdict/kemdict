import { getBacklinks } from "$lib/server/db";

export function load({ params }) {
  return {
    data: getBacklinks(params.title),
    params: { title: params.title },
    searchParams: {},
  };
}
