import type { APIRoute } from "astro";
import { DB } from "$src/server/db";
export const GET: APIRoute = async ({ params }) => {
  const prefix = params.prefix;
  if (prefix === undefined) {
    return new Response(null, {
      status: 400,
      statusText: "Incorrect request, prefix must be provided",
    });
  }

  const completion = await DB.getPrefixCompletion(decodeURIComponent(prefix));
  return new Response(JSON.stringify(completion), {
    headers: {
      "Content-Type": "application/json; charset=utf-8",
    },
  });
};
