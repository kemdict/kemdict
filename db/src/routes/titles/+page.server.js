import { redirect } from "@sveltejs/kit";

export function load() {
  throw redirect(307, encodeURI("/titles/dict_concised?limit=5"));
}
