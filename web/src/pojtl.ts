import { Ku } from "@kemdict/kesi";

// export async function toPOJ(text: string) {
//   const response = await fetch("https://pojtl.kemdict.com/toPOJ", {
//     method: "POST",
//     body: text,
//   });
//   return await response.text();
// }

export function toPOJ(text: string) {
  return new Ku(text).POJ().hanlo;
}

export function toTL(text: string) {
  return new Ku(text).KIP().hanlo;
}
