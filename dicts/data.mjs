export const langs = {
  zh_TW: "華語",
  nan_TW: "台語",
  hak_TW: "客語",
  han: "漢字",
  // ais: "撒奇萊雅語",
  // ami: "阿美語",
  // bnn: "布農語",
  // ckv: "噶瑪蘭語",
  // dru: "魯凱語",
  // pwn: "排灣語",
  // pyu: "卑南語",
  // ssf: "邵語",
  // sxr: "拉阿魯哇語",
  // tao: "雅美語",
  // tay: "泰雅語",
  // trv: "太魯閣語",
  // sdq: "賽德克語",
  // tsu: "鄒語",
  // xnb: "卡那卡那富語",
  // xsy: "賽夏語",
};

// const ilrdf_codes = [
// "ais",
// "ami",
// "bnn",
// "ckv",
// "dru",
// "pwn",
// "pyu",
// "ssf",
// "sxr",
// "tao",
// "tay",
// "trv",
// "sdq",
// "tsu",
// "xnb",
// "xsy",
// ];

// desc_long is Markdown.
export const dicts = [
  {
    id: "unihan",
    name: "Unihan資料庫",
    url: "https://www.unicode.org/cgi-bin/GetUnihanData.pl?codepoint=$1",
    lang: "han",
    meta: {
      author: "Unicode聯盟",
      desc_short: "Unicode聯盟所維護的漢字資料庫",
      desc_long: `Unicode聯盟所維護的漢字資料庫，包含筆畫數、部首、簡繁對應等資訊。`,
    },
  },
  // ...ilrdf_codes.map((code) => ({
  //   id: `ilrdf_${code}`,
  //   name: `原住民族語言線上辭典（${langs[code]}）`,
  //   url: `https://e-dictionary.ilrdf.org.tw/${code}/search.htm`,
  //   lang: code,
  // })),
  {
    id: "chhoetaigi_taioanpehoekichhoogiku",
    name: "台灣白話基礎語句",
    url: "https://chhoe.taigi.info/TaioanPehoeKichhooGiku/$1",
    lang: "nan_TW",
    meta: {
      author: "高積煥、陳邦鎮",
      year: 1956,
      desc_short: "A Basic Vocabulary For A Beginner In Taiwanese",
      desc_long: `利用ChhoeTaigi的數位化成果。

https://thak.taigi.info/1956TaioanPehoeKichhooGiku/`,
    },
  },
  {
    id: "kisaragi_dict",
    name: "如月的現代台灣華語補足典",
    url: "/dict-kisaragi",
    lang: "zh_TW",
    meta: {
      desc_short: "TODO",
      desc_long: ``,
    },
  },
  {
    id: "dict_concised",
    name: "國語辭典簡編本",
    url: "https://dict.concised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh_TW",
    meta: {
      author: "教育部",
      desc_short: "TODO",
      desc_long: `TODO`,
    },
  },
  {
    id: "dict_revised",
    name: "重編國語辭典修訂本",
    url: "https://dict.revised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh_TW",
    meta: {
      author: "教育部",
      desc_short: "TODO",
      desc_long: `TODO`,
    },
  },
  {
    id: "kisaragi_taigi",
    name: "如月的台語補足典",
    // TODO
    url: "/dict-kisaragi",
    lang: "nan_TW",
    meta: {
      desc_short: "TODO",
      desc_long: `TODO`,
    },
  },
  {
    id: "chhoetaigi_taijittoasutian",
    name: "台日大辭典台語譯本 (1932)",
    url: "https://taigi.fhl.net/dict/search.php?DETAIL=1&LIMIT=id=$1",
    lang: "nan_TW",
    meta: {
      author: "TODO",
      year: 1932,
      desc_short: "TODO",
      desc_long: `TODO`,
    },
  },
  {
    id: "moedict_twblg",
    name: "臺灣閩南語常用詞辭典",
    url: "https://sutian.moe.edu.tw/und-hani/tshiau/?lui=tai_su&tsha=$1",
    lang: "nan_TW",
    meta: {
      author: "教育部",
      desc_short: "TODO",
      desc_long: `TODO`,
    },
  },
  {
    id: "chhoetaigi_itaigi",
    name: "iTaigi 華台對照典",
    url: "https://itaigi.tw/k/$1",
    lang: "nan_TW",
    meta: {
      desc_short: "TODO",
      desc_long: `TODO`,
    },
  },
  {
    id: "hakkadict",
    name: "臺灣客家語常用詞辭典",
    url: 'https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi?o=dalldb&s=id="$1".&searchmode=basic',
    lang: "hak_TW",
    meta: {
      author: "教育部",
      desc_short: "TODO",
      desc_long: `TODO`,
    },
  },
  {
    id: "dict_idioms",
    name: "成語典",
    url: "https://dict.idioms.moe.edu.tw/idiomList.jsp?idiom=$1",
    lang: "zh_TW",
    meta: {
      author: "教育部",
      desc_short: "TODO",
      desc_long: `TODO`,
    },
  },
];
