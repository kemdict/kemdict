export async function toPOJ(text: string) {
  const response = await fetch("https://pojtl.kemdict.com/toPOJ", {
    method: "POST",
    body: text,
  });
  return await response.text();
}

export async function toTL(text: string) {
  const response = await fetch("https://pojtl.kemdict.com/toTL", {
    method: "POST",
    body: text,
  });
  return await response.text();
}
