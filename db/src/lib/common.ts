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
  "/titles": {
    signature: "/titles/[dict]?limit=[limit]",
    args: [
      { name: "dict", desc: "Return titles from this dictionary ID" },
      { name: "limit", desc: "How many entries to return; empty means all" },
    ],
  },
  "/het": {
    signature: "/het/[query]?m=[mtch]",
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
};
