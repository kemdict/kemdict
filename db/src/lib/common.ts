export interface Dict {
  id: string;
  name: string;
  url: string;
  lang: string;
}

export interface Heteronym {
  title: string;
  from?: string;
  pns?: any[];
  props: any;
}

export const signatures = {
  "/het": {
    signature: "/het/<query>?m=[mtch]",
    args: [
      { name: "query", desc: "Return heteronyms that match this query" },
      {
        name: "m",
        desc: "Match type. One of:",
        children: [
          '"prefix"',
          '"suffix"',
          '"contains"',
          "anything else: match exactly",
        ],
      },
    ],
  },
  "/back": {
    signature: "/back/<title>",
    desc: "Return words that link to <title>",
    args: [{ name: "title", desc: "The title to return backlinks for" }],
  },
  "/titles": {
    signature: "/titles/<dict>?limit=[limit]",
    args: [
      { name: "dict", desc: "Return titles from this dictionary ID" },
      { name: "limit", desc: "How many entries to return; empty means all" },
    ],
  },
  "/all-chars": {
    desc: "All characters with stroke count, along with another array for characters with unknown stroke counts.",
    signature: "/all-chars (big page)",
    args: [],
  },
  "/chars": {
    desc: "Return characters of a given radical",
    signature: "/chars/<radical>",
    args: [{ name: "radical", desc: 'The radical, like "火"' }],
  },
  "/radicals": {
    desc: "Return an array of all radicals.",
    signature: "/radicals",
    args: [],
  },
};
