export function lGet(key: string, def?: any): any {
  const s = window.localStorage.getItem(key);
  if (typeof s === "undefined") {
    return def;
  }
  return JSON.parse(s);
}

export function lSet(key: string, value: any) {
  window.localStorage.setItem(key, JSON.stringify(value));
}

export function lUpdate(key: string, updater: (key: string) => void) {
  lSet(key, updater(lGet(key)));
}
