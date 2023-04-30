/* POST some stuff to a URL.
 * Usage: one of
 *   echo input | node postSync.mjs <url>
 *   node postSync.mjs <url> <input>
 */

const url = process.argv[2];

async function get(text) {
  const response = await fetch(url, {
    method: "POST",
    body: text,
  });
  console.log(await response.text());
}

let text = "";
if (process.argv.length > 3) {
  text = process.argv[3];
  get(text);
} else {
  process.stdin.on("readable", () => {
    let chunk;
    while (null !== (chunk = process.stdin.read())) {
      text += chunk;
    }
  });
  process.stdin.on("end", () => {
    get(text);
  });
}
