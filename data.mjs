export const langs = {
  zh_TW: "華語",
  nan_TW: "台語",
  hak_TW: "客語",
  han: "漢字",
};

export const dicts = [
  {
    id: "unihan",
    name: "Unihan 資料庫",
    url: "https://www.unicode.org/cgi-bin/GetUnihanData.pl?codepoint=$1",
    lang: "han",
  },
  {
    id: "chhoetaigi_taioanpehoekichhoogiku",
    name: "台灣白話基礎語句",
    url: "https://chhoe.taigi.info/TaioanPehoeKichhooGiku/$1",
    lang: "nan_TW",
  },
  {
    id: "kisaragi_dict",
    name: "如月的現代台灣華語補足典",
    url: "/dict-kisaragi",
    lang: "zh_TW",
  },
  {
    id: "dict_concised",
    name: "國語辭典簡編本",
    url: "https://dict.concised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh_TW",
  },
  {
    id: "dict_revised",
    name: "重編國語辭典修訂本",
    url: "https://dict.revised.moe.edu.tw/search.jsp?word=$1",
    lang: "zh_TW",
  },
  {
    id: "chhoetaigi_taijittoasutian",
    name: "台日大辭典台語譯本 (1932)",
    url: "https://taigi.fhl.net/dict/search.php?DETAIL=1&LIMIT=id=$1",
    lang: "nan_TW",
  },
  {
    id: "moedict_twblg",
    name: "臺灣閩南語常用詞辭典",
    url: "https://twblg.dict.edu.tw/holodict_new/result_main.jsp?radiobutton=1&limit=20&querytarget=1&sample=$1",
    lang: "nan_TW",
  },
  {
    id: "chhoetaigi_itaigi",
    name: "iTaigi 華台對照典",
    url: "https://itaigi.tw/k/$1",
    lang: "nan_TW",
  },
  {
    id: "hakkadict",
    name: "臺灣客家語常用詞辭典",
    url: 'https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi?o=dalldb&s=id="$1".&searchmode=basic',
    lang: "hak_TW",
  },
  {
    id: "dict_idioms",
    name: "成語典",
    url: "https://dict.idioms.moe.edu.tw/idiomList.jsp?idiom=$1",
    lang: "zh_TW",
  },
];
