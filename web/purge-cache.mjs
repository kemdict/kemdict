const token = process.env["CLOUDFLARE_TOKEN"];
const id = process.env["CLOUDFLARE_ZONE_ID"];

if (!token || !id) {
  process.exit(0);
}

console.log("Telling Cloudflare to purge cache");

const req = new Request(
  `https://api.cloudflare.com/client/v4/zones/${id}/purge_cache`,
  {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
    body: JSON.stringify({
      purge_everything: true,
    }),
  }
);
fetch(req)
  .then((response) => response.json())
  .then((json) => {
    if (!json.success) throw json;
    console.log("Done");
  })
  .catch(console.error);
