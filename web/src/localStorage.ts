/**
 * Helpers for storing serializable values in local storage.
 */

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

export function lUpdate(key: string, updater: (value: any) => void) {
  lSet(key, updater(lGet(key)));
}

/** Push `value` to the array stored in `key`. */
export function lPush(key: string, value: any) {
  lUpdate(key, (arr) => {
    if (Array.isArray(arr)) {
      // Put it in "front" without duplicating
      const s = new Set(arr);
      s.delete(value);
      s.add(value);
      return [...s];
    } else {
      return [value];
    }
  });
}

export function saveHistory(type: string, obj: any) {
  if (typeof window !== "undefined" && obj) {
    lPush(`${type}History`, obj);
  }
}
