import { getChars } from "$lib/server/db";

export function load() {
  return {
    data: getChars(),
    params: {},
    searchParams: {},
  };
}
