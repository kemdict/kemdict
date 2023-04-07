import { json } from "@sveltejs/kit";
import { load } from "./+page.server";

export function GET({ url, params }) {
  return json(load({ url, params }).data);
}
