import { t } from "i18next";
import { template } from "lodash-es";

import { joinLast, parseQueryToTokens, format } from "common";
import type { Mtch } from "common";

export function getSearchTitle(
  mtch: Mtch,
  query: string,
  markup?: boolean
): string {
  const tokens = parseQueryToTokens(query);
  function wrap(s: string) {
    if (markup) {
      return format(
        t("searchTitle.wrap"),
        `<span class="font-bold">${s}</span>`
      );
    } else {
      return format(t("searchTitle.wrap"), s);
    }
  }
  if (markup) {
    query = `<span class="font-bold">${query}</span>`;
  }
  if (mtch === "contains") {
    return format(
      t("searchTitle.contains"),
      joinLast(tokens.map(wrap), t("、"), t("及"))
    );
  } else if (mtch === "prefix") {
    if (tokens.length === 1) {
      return format(t("searchTitle.prefix"), wrap(tokens[0]));
    } else {
      return template(t("searchTitle.prefixN"))({
        first: wrap(tokens[0]),
        rest: joinLast(tokens.slice(1).map(wrap), t("、"), t("及")),
      });
    }
  } else if (mtch === "suffix") {
    if (tokens.length === 1) {
      return format(t("searchTitle.suffix"), wrap(tokens[tokens.length - 1]));
    } else {
      return template(t("searchTitle.suffixN"))({
        last: wrap(tokens[tokens.length - 1]),
        rest: joinLast(tokens.slice(0, -1).map(wrap), t("、"), t("及")),
      });
    }
  } else {
    // if (mtch === "exact") {
    // }
    return format(t("searchTitle.exact", query));
  }
}
