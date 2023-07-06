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
      desc_long: `Unicode聯盟所維護的漢字資料庫，包含筆畫數、部首、簡繁對應等資訊。`,
      license: {
        name: "Unicode License",
        url: "https://www.unicode.org/license.txt",
      },
      source: "https://unihan-etl.git-pull.com/",
      original: "https://unicode.org/charts/unihan.html",
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
      author: "Ko Chek-hoàn（高積煥）、Tân Pang-tìn（陳邦鎮）",
      extra: {
        數位化及編修: "Lîm Bûn-cheng、Tēⁿ Tì-têng、Tân Kim-hoa、Chiúⁿ Ji̍t-êng",
      },
      year: 1956,
      desc_long: `1956年出版的，一套台語教科書的第四本。詳細請參照[ChhoeTaigi 冊tha̍k-á上的資訊](https://thak.taigi.info/1956TaioanPehoeKichhooGiku/)。`,
      license: {
        name: "CC BY-SA 4.0",
        url: "https://github.com/ChhoeTaigi/ChhoeTaigiDatabase/#8-1956-台灣白話基礎語句",
      },
      source: "https://github.com/ChhoeTaigi/ChhoeTaigiDatabase",
      original: "https://thak.taigi.info/1956TaioanPehoeKichhooGiku/",
    },
  },
  {
    id: "kisaragi_dict",
    name: "如月的現代台灣華語補足典",
    url: "/dicts/kisaragi_dict",
    lang: "zh_TW",
    meta: {
      desc_long: `Kemdict 的額外華語詞。

我記錄台灣華語有在使用但沒被收錄的詞的地方。

不是只有流行語。

來自台語的借詞也會列。

重編本或簡編本已經有的定義不會再重複，只寫它們沒提到的部分。`,
      license: {
        name: "CC0",
        url: "https://github.com/kemdict/kemdict/blob/main/dicts/kisaragi/LICENSE",
      },
      source:
        "https://github.com/kemdict/kemdict/blob/main/dicts/kisaragi/kisaragi-dict.org",
    },
  },
  {
    id: "kisaragi_taigi",
    name: "如月的台語補足典",
    url: "/dicts/kisaragi_taigi",
    lang: "nan_TW",
    meta: {
      desc_long: `我盡力補充台語沒被定義的詞的地方。`,
      license: {
        name: "CC0",
        url: "https://github.com/kemdict/kemdict/blob/main/dicts/kisaragi/LICENSE",
      },
      source:
        "https://github.com/kemdict/kemdict/blob/main/dicts/kisaragi/kisaragi-taigi.org",
    },
  },
  {
    id: "dict_concised",
    name: "國語辭典簡編本",
    url: "https://dict.concised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh_TW",
    meta: {
      author: "教育部",
      desc_long: `
> 主要適用對象：國中、小學生及學習華語人士。
>
> 特色：
>
> 1. 為一部有聲音及圖片之多媒體辭典，收詞以字詞頻統計結果為依據，釋義行文則採淺白語體編寫。
> 2. 本辭典所收的字音，參照教育部公布之「國語一字多音審訂表」，並經審音委員會審訂決議。`,
      license: {
        name: "CC BY-ND",
        url: "/l/conciseddict_10312.pdf",
      },
      source:
        "https://language.moe.gov.tw/001/Upload/Files/site_content/M0001/respub/",
      original: "https://dict.concised.moe.edu.tw/"
    },
  },
  {
    id: "dict_revised",
    name: "重編國語辭典修訂本",
    url: "https://dict.revised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh_TW",
    meta: {
      author: "教育部",
      // TODO: fill this out
      desc_long: ``,
      license: {
        name: "CC BY-ND 3.0 TW",
        url: "/l/reviseddict_10312.pdf",
      },
      source: "https://language.moe.gov.tw/001/Upload/Files/site_content/M0001/respub/",
      original: "https://dict.revised.moe.edu.tw/"
    },
  },
  {
    id: "chhoetaigi_taijittoasutian",
    name: "台日大辭典台語譯本",
    url: "https://taigi.fhl.net/dict/search.php?DETAIL=1&LIMIT=id=$1",
    lang: "nan_TW",
    meta: {
      author: "小川尚義",
      extra: {
        台文翻譯及編修: "Lîm Chùn-io̍k（林俊育）長老",
      },
      year: 1932,
      desc_long: `
《台日大辭典》由日本語言學家小川尚義主編、台灣總督府出版，以日語編寫，記錄了當時的台語。

其後[台語信望愛](https://toj.fhl.net/index.html)執行人 Lîm Chùn-io̍k 長老將其譯為台文，並整理資料、數位化，且資料以 [CC-BY-NC-SA 3.0 台灣](https://creativecommons.org/licenses/by-nc-sa/3.0/tw/) 授權[公開於 GitHub](https://github.com/fhl-net/Lim-Chun-iok_2008_Tai-jip-Tua-su-tian)。

可以參照[ChhoeTaigi 冊tha̍k-á上的介紹](https://thak.taigi.info/1931TaijitToaSutian1/)。`,
      license: {
        name: "CC BY-NC-SA 3.0 TW",
        url: "https://github.com/ChhoeTaigi/ChhoeTaigiDatabase/#2-1932-台日大辭典台譯版",
      },
      source: "https://github.com/ChhoeTaigi/ChhoeTaigiDatabase/",
      original: "https://taigi.fhl.net/dict/",
    },
  },
  {
    id: "moedict_twblg",
    name: "臺灣閩南語常用詞辭典",
    url: "https://sutian.moe.edu.tw/und-hani/tshiau/?lui=tai_su&tsha=$1",
    lang: "nan_TW",
    meta: {
      author: "教育部",
      // TODO: fill this out
      desc_long: ``,
      license: {
        name: "CC BY-ND 3.0 TW",
        url: "https://sutian.moe.edu.tw/zh-hant/piantsip/pankhuan-singbing/",
      },
      source: "https://github.com/g0v/moedict-data-twblg",
      original: "https://sutian.moe.edu.tw/"
    },
  },
  {
    id: "chhoetaigi_itaigi",
    name: "iTaigi 華台對照典",
    url: "https://itaigi.tw/k/$1",
    lang: "nan_TW",
    meta: {
      author: "g0v、iTaigi 貢獻者",
      desc_long: `
> iTaigi 是 g0v 零時政府專案「萌典」的延伸專案，想知道一個詞的台語怎麼說，來這裡查就對了！甚麼都可以查，但不一定查得到，查不到時可以發問，或者自己發明台語講法貢獻給大家，簡單說就是「自己的辭典自己編」。

參見[iTaigi的介紹](https://itaigi.tw/about)。`,
      license: {
        name: "CC0",
        url: "https://itaigi.tw/hokbu",
      },
      source: "https://github.com/ChhoeTaigi/ChhoeTaigiDatabase/",
      original: "https://itaigi.tw/",
    },
  },
  {
    id: "hakkadict",
    name: "臺灣客家語常用詞辭典",
    url: 'https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi?o=dalldb&s=id="$1".&searchmode=basic',
    lang: "hak_TW",
    meta: {
      author: "教育部",
      // TODO: fill this out
      desc_long: ``,
      license: {
        name: "CC BY-ND 3.0 TW",
        url: "https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi/ccd=KO93Fb/newsresult_format0?r1=1&searchall=1",
      },
      source:
        "https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi/ccd=KO93Fb/newsresult_format0?r1=1&searchall=1",
      original: "https://hakkadict.moe.edu.tw"
    },
  },
  {
    id: "dict_idioms",
    name: "成語典",
    url: "https://dict.idioms.moe.edu.tw/idiomList.jsp?idiom=$1",
    lang: "zh_TW",
    meta: {
      author: "教育部",
      // TODO: fill this out
      desc_long: ``,
      license: {
        name: "CC BY-ND",
        url: "/l/idiomsdict_10409.pdf",
      },
      source:
        "https://language.moe.gov.tw/001/Upload/Files/site_content/M0001/respub/",
      original: "https://dict.idioms.moe.edu.tw/"
    },
  },
];
