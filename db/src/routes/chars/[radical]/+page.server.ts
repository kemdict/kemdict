import { getCharsByRadical } from "$lib/server/db";

export function load({ params }) {
  return {
    data: getCharsByRadical(params.radical),
    params: { radical: params.radical },
    searchParams: {},
  };
}
