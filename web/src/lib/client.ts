export async function clientGetCompletions(prefix: string) {
  if (prefix.trim() === "") return [];
  const res = await fetch(`/api/compl/prefix/${prefix}`);
  return res.json() as Promise<string[]>;
}
