export async function clientGetCompletions(prefix: string) {
  if (prefix.trim() === "") return [];
  const res = await fetch(`/api/completion/${prefix}`);
  return res.json() as Promise<string[]>;
}
