import { fastify } from "fastify";
import { fastifyStatic } from "@fastify/static";
// cute name, did no one note how hard it is to read? That's a second "i".
import { fastifyMiddie } from "@fastify/middie";
import { handler } from "../dist/server/entry.mjs";
import { env } from "node:process";
import * as path from "node:path";
import { Option, pipe } from "effect";

const app = fastify();
await app
  .register(fastifyStatic, {
    root: path.resolve("./client"),
  })
  .register(fastifyMiddie);
await app.use(handler);
await app.listen({
  port: pipe(
    Option.fromNullishOr(env["PORT"]),
    Option.map(parseInt),
    Option.getOrElse(() => 3000),
  ),
});
