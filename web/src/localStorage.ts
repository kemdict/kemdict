/**
 * Helpers for storing serializable values in local storage.
 */

export function lGet<T>(key: string, def?: T): T | undefined {
  const s = window.localStorage.getItem(key);
  if (typeof s === "undefined") {
    return def;
  }
  return typeof s === "string" && JSON.parse(s);
}

export function lSet<T>(key: string, value: T) {
  window.localStorage.setItem(key, JSON.stringify(value));
}

export function lUpdate<T>(key: string, updater: (value: T | undefined) => T) {
  lSet(key, updater(lGet(key)));
}

/** Push `value` to the array stored in `key`. */
export function lPush<T>(key: string, value: T) {
  lUpdate<T[]>(key, (arr) => {
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

/**
 * Save OBJ into a history localStorage.
 * Given a type of, say, "word", this would save into an array under the
 * "wordHistory" key.
 * This pushes to the end such that the array is sorted old to new.
 */
export function saveHistory(type: string, obj: any) {
  if (typeof window !== "undefined" && obj) {
    lPush(`${type}History`, obj);
  }
}
