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
    signature: "/het/[query]?m=[mtch]",
    args: [
      { name: "query", desc: "return heteronyms that match this query" },
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
