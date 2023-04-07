import { getRadicals } from "$lib/server/db";

export function load() {
  return {
    data: getRadicals(),
    params: {},
    searchParams: {},
  };
}
