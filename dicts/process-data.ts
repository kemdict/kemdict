// WIP: TypeScript port of process-data
// Why? It's a less insane language than Elisp, I guess

export interface Heteronym {
  title: string;
  from: string | undefined;
  lang: string;
  props: any;
  exact?: boolean;
}

type Opt<T> = T | undefined;
interface Link {
  to: string;
  from: string;
}
/** Used to mark where links are coming from to register to the links table. */
let links__from: Opt<string> = undefined;
/**
 * A list of targets that have already been linked.
 *
 *`links__linkify-keywords` uses this to avoid replacing links multiple times. */
let links__linked: unknown[] = [];
/** Used to override the language of a link. */
let links__lang: Opt<string> = undefined;
/** A list of Link objects. */
let links: Link[] = [];
let titles = new Set<string>();

/**
 * Create an HTML link with `desc` to `target` when appropriate.
 * Just return `desc` if `target` does not exist in `titlesLookUpTable`, or if
 * `desc` already looks like an HTML link.
 *
 * If `href` is a string, the HTML will link to `href` instead. This is useful for
 * creating links to "/word/word#heading" while still only checking for the
 * word's existence with "word". Use it like this:
 * links_linkToWord("word", undefined, "word#heading")
 */
function links_linkToWord(target: string, desc = target, href = target) {
  // HACK: some phrases contain a period. Treat would-be references to it
  // without a period as actual references.
  let period = false;
  if (
    !target ||
    target === null ||
    target === links__from ||
    !(titles.has(target) || (titles.has(target + "。") && (period = true))) ||
    desc.toLowerCase().includes("<a")
  ) {
    return desc;
  }
  if (period) {
    target = target + "。";
    href = href + "。";
  }
  if (links__from !== undefined) {
    links__linked.push(target);
    links.push({ from: links__from, to: target });
  }
  if (links__lang !== undefined) {
    // If the word doesn't exist in the language we ask for here, the word page
    // just removes the query param and does a redirect as if it's not passed
    // in. So this is fine.
    return `<a href="/word/${href}?lang=${links__lang}">${desc}</a>`;
  } else {
    return `<a href="/word/${href}">${desc}</a>`;
  }
}

/** Try to create a link for synonym arrows in `str`. */
function links_linkifyArrow(str: string) {
  return str.replace(
    /→(.+?)(。|$)/,
    (_$0, $1: string, $2: string) => `→${links_linkToWord($1)}${$2}`,
  );
}

/**
 * Normalize pronunciation PN.
 * If PN is a hash table, get the string from its value for \"zh-Hant\".
 * Return a list of normalized strings. This is because some
 * pronunciation strings include multiple pronunciations.
 */
function pnNormalize(pn: string | { zh_Hant: string }) {
  const actualPn = typeof pn === "string" ? pn : pn.zh_Hant;
  actualPn
    .normalize("NFD")
    .replaceAll("　", " ")
    // The replacement character, which appears in one entry in
    // itaigi.
    // https://itaigi.tw/k/%E5%8D%88%E5%AE%89/
    // I'm pretty sure it's not supposed to be there.
    .replaceAll("\uFFFD", "")
    .replaceAll("（變）", "/")
    .split(/[ \t\n\r]*\/[ \t\n\r]*/)
    .filter((it) => it !== "");
}

/** Collect pronunciations from `het`. */
function pnCollect(het: Heteronym) {
  let keys = [
    // kemdict-data-ministry-of-education
    "bopomofo",
    // "pinyin",

    // moedict-twblg
    "trs",
    // kisaragi-dict
    "pronunciation",

    // hakkadict
    "p_四縣",
    "p_海陸",
    "p_大埔",
    "p_饒平",
    "p_詔安",
    "p_南四縣",

    // chhoetaigi-itaigi (keys are defined in Makefile
    // in this repository)
    "poj",
    "kip",
    "pojInput",
    "kipInput",
    "pojInputOthers",
    "kipInputOthers",

    // unihan
    "kMandarin",
  ];

  const ret = new Map<string, string>();
  for (const key of keys) {
    if (key in het.props) {
      for (const p of pnNormalize(het.props[key] as string)) {
        ret.set(key, p);
      }
    }
  }
  return ret;
}

/**
 * Normalize `pn` such that it is searchable with an ASCII keyboard.
 */
function pnToInputForm(pn: string) {
  return [...pn.replaceAll("ⁿ", "nn").normalize("NFKD")]
    .filter((char) => {
      const codepoint = char.codePointAt(0);
      return codepoint && codepoint < 128;
    })
    .join("");
}
