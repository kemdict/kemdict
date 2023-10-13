import json
import os
import re
import sqlite3
from dataclasses import dataclass
from types import MappingProxyType
from typing import Any, Optional

import unicodedataplus

_links: list[str] = []
_linked_targets: list[str] = []
_titles_lookup_table: set[str] = set()
_db: Optional[sqlite3.Connection] = None

# MappingProxyType is an immutable proxy; use it to enforce an immutable dict.
# cf. https://adamj.eu/tech/2022/01/05/how-to-make-immutable-dict-in-python/
_abc_han_dict: MappingProxyType[str, str] = MappingProxyType(
    # This is mainly a hack to avoid this constant taking up an entire screen.
    json.loads(
        # adjacent string literals are concatenated
        '{"a": "日", "b": "月", "c": "金", "d": "木", "e": "水", "f": "火",'
        ' "g": "土", "h": "竹", "i": "戈", "j": "十", "k": "大", "l": "中",'
        ' "m": "一", "n": "弓", "o": "人", "p": "心", "q": "手", "r": "口",'
        ' "s": "尸", "t": "廿", "u": "山", "v": "女", "w": "田", "x": "難",'
        ' "y": "卜", "z": "重"}'
    )
)
_pn_keys = (
    # kemdict-data-ministry-of-education
    "bopomofo",  # "pinyin",
    # moedict-twblg
    "trs",
    # kisaragi-dict
    "pronunciation",
    # hakkadict
    "p_四縣",
    "p_海陸",
    "p_大埔",
    "p_饒平",
    "p_詔安",
    "p_南四縣",
    # chhoetaigi-itaigi (keys are defined in Makefile
    # in this repository)
    "poj",
    "kip",
    "kMandarin",
)


@dataclass
class Dictionary:
    """Represents a dictionary.
    A None ID means its entries are included but not shown."""

    id: Optional[str]
    lang: str
    files: tuple[str, ...]


def _dicts() -> tuple[Dictionary, ...]:
    """Return all available dictionaries."""
    raw_data: tuple[tuple[str, str, str | tuple[str, ...]], ...] = (
        ("unihan", "han", "unihan.json"),
        ("kisaragi_dict", "zh_TW", "kisaragi/kisaragi_dict.json"),
        ("dict_concised", "zh_TW", "ministry-of-education/dict_concised.json"),
        ("dict_revised", "zh_TW", "ministry-of-education/dict_revised.json"),
        ("kisaragi_taigi", "nan_TW", "kisaragi/kisaragi_taigi.json"),
        (
            "moedict_twblg",
            "nan_TW",
            (
                "moedict-data-twblg/dict-twblg.json",
                "moedict-data-twblg/dict-twblg-ext.json",
            ),
        ),
        (
            "chhoetaigi_taijittoasutian",
            "nan_TW",
            "chhoetaigi/ChhoeTaigi_TaijitToaSutian.json",
        ),
        (
            "chhoetaigi_itaigi",
            "nan_TW",
            "chhoetaigi/ChhoeTaigi_iTaigiHoataiTuichiautian.json",
        ),
        (
            "chhoetaigi_taioanpehoekichhoogiku",
            "nan_TW",
            "chhoetaigi/ChhoeTaigi_TaioanPehoeKichhooGiku.json",
        ),
        ("hakkadict", "hak_TW", "ministry-of-education/hakkadict.json"),
        ("ilrdf_ais", "ais", "ilrdf/ais.json"),
        ("ilrdf_ami", "ami", "ilrdf/ami.json"),
        ("ilrdf_bnn", "bnn", "ilrdf/bnn.json"),
        ("ilrdf_ckv", "ckv", "ilrdf/ckv.json"),
        ("ilrdf_dru", "dru", "ilrdf/dru.json"),
        ("ilrdf_pwn", "pwn", "ilrdf/pwn.json"),
        ("ilrdf_pyu", "pyu", "ilrdf/pyu.json"),
        ("ilrdf_ssf", "ssf", "ilrdf/ssf.json"),
        ("ilrdf_sxr", "sxr", "ilrdf/sxr.json"),
        ("ilrdf_tao", "tao", "ilrdf/tao.json"),
        ("ilrdf_tay", "tay", "ilrdf/tay.json"),
        ("ilrdf_trv", "trv", "ilrdf/trv.json"),
        ("ilrdf_sdq", "sdq", "ilrdf/sdq.json"),
        ("ilrdf_tsu", "tsu", "ilrdf/tsu.json"),
        ("ilrdf_xnb", "xnb", "ilrdf/xnb.json"),
        ("ilrdf_xsy", "xsy", "ilrdf/xsy.json"),
        ("dict_idioms", "zh_TW", "ministry-of-education/dict_idioms.json"),
    )
    tmp: list[Dictionary] = []
    for id, lang, files in raw_data:
        files = files if isinstance(files, tuple) else (files,)
        if all(map(lambda f: os.path.exists(f), files)):
            tmp.append(Dictionary(id, lang, files))
    return tuple(tmp)


def _langs() -> tuple[tuple[str, str], ...]:
    """Return all languages."""
    raw_data = (
        ("zh_TW", "華語"),
        ("nan_TW", "台語"),
        ("hak_TW", "客語"),
        ("han", "漢字"),
        ("ais", "撒奇萊雅語"),
        ("ami", "阿美語"),
        ("bnn", "布農語"),
        ("ckv", "噶瑪蘭語"),
        ("dru", "魯凱語"),
        ("pwn", "排灣語"),
        ("pyu", "卑南語"),
        ("ssf", "邵語"),
        ("sxr", "拉阿魯哇語"),
        ("tao", "雅美語"),
        ("tay", "泰雅語"),
        ("trv", "太魯閣語"),
        ("sdq", "賽德克語"),
        ("tsu", "鄒語"),
        ("xnb", "卡那卡那富語"),
        ("xsy", "賽夏語"),
    )
    available_codes = set(map(lambda d: d.lang, _dicts()))
    tmp: list[tuple[str, str]] = []
    for code, name in raw_data:
        if code in available_codes:
            tmp.append((code, name))
    return tuple(tmp)


def db_init() -> sqlite3.Connection:
    """Create and initialize a new entries.db including its tables.
    Return the connection."""
    try:
        os.remove("entries.db")
    except FileNotFoundError:
        pass
    db = sqlite3.connect("entries.db")
    db.executescript(
        """
BEGIN;
CREATE TABLE langs (
  "id" TEXT PRIMARY KEY,
  "name" TEXT NOT NULL
);
CREATE TABLE dicts (
  "id" TEXT PRIMARY KEY,
  "lang" TEXT REFERENCES langs("id")
);
CREATE TABLE heteronyms (
  "id" INTEGER PRIMARY KEY,
  "title" TEXT NOT NULL,
  "from" TEXT REFERENCES dicts("id"),
  "lang" TEXT REFERENCES langs("id"),
  "props" TEXT NOT NULL
);
CREATE TABLE aliases (
  "het_id" INTEGER REFERENCES heteronyms("id"),
  "alias" TEXT NOT NULL,
  "exact" INTEGER
);
CREATE TABLE links (
  "from" TEXT NOT NULL,
  "to" TEXT NOT NULL
);
-- New words, sorted by date/time added
CREATE TABLE newwords (
  "title" TEXT NOT NULL,
  "time" TEXT NOT NULL,
  "from" TEXT REFERENCES dicts("id")
);
COMMIT;
    """
    )
    return db


def db_insert(heteronyms, links):
    "Insert `heteronyms` and `links` into the database."
    print("Initializing database...")
    db = db_init()
    cursor = db.cursor()
    print("Preparing langs and dicts...")
    cursor.executemany("""INSERT INTO langs ("id", "name") VALUES (?, ?)""", _langs())


# 2023-10-13:
# Uncommenting this requires Python 3.12, which is not in Arch yet.
# def dict_prune[K, V](dictionary: dict[K, V], value: V) -> dict[K, V]:
def dict_prune(dictionary: dict[Any, Any], value: Any) -> dict[Any, Any]:
    """Remove all entries in `dictionary` that are associated with `value`.
    Modifies `dictionary`.
    """
    for k, v in list(dictionary.items()):
        if v == value:
            del dictionary[k]
    return dictionary


# def dict_rename[K, V](
#     dictionary: dict[K, V], key_from: K, key_to: K
# ) -> dict[K, V]:
def dict_rename(
    dictionary: dict[Any, Any], key_from: Any, key_to: Any
) -> dict[Any, Any]:
    """Rename the key `key_from` to `key_to` in `dictionary`.
    Modifies `dictionary`."""
    if key_from in dictionary.keys():
        dictionary[key_to] = dictionary[key_from]
        del dictionary[key_from]
    return dictionary


def cangjie_abc_to_han(abc: str) -> str:
    """Convert Cangjie code `abc` in latin form to Han characters."""
    return "".join([_abc_han_dict[c] for c in abc.lower()])


def is_latin_only(string: str) -> bool:
    """Check whether `string` contains mostly just latin characters."""
    scripts = {"Latin", "Inherited", "Common"}
    return all(map(lambda c: unicodedataplus.script(c) in scripts, string))


def radical_id_to_char(radical_id: int) -> str:
    """Return the normalized radical character for `radical-id`.
    For example, 1 is 一, 213 is 龜."""
    kanxi_radical = chr(0x2F00 + radical_id - 1)
    return unicodedataplus.normalize("NFKC", kanxi_radical)


def hakkadict_pn(pn: str, dialect: str) -> str:
    """Turn numeric tones in `pn` into Unicode.

    `dialect` is the dialect, because that matters: 31 is \"ˋ\" in 四縣
    and 南四縣, \"^\" in 大埔 and 詔安.

    According to 客家語拼音方案. See:
    https://language.moe.gov.tw/result.aspx?classify_sn=&subclassify_sn=447&content_sn=12
    """
    for from_, to in (
        ("113", "ˇ"),
        ("11", "ˇ"),
        ("21", "^"),
        ("24", "ˊ"),
        ("33", "+"),
        ("35", "ˊ"),
        ("43", "ˋ"),
        ("53", "ˋ"),
        ("54", "ˋ"),
        ("55", ""),
        ("2", "ˋ"),
        ("5", ""),
    ):
        pn = pn.replace(from_, to)
    # 四縣, 南四縣 is "ˋ", 大埔, 詔安 is "^"
    if dialect in ("四縣", "南四縣"):
        pn = pn.replace("31", "ˋ")
    else:
        pn = pn.replace("31", "^")
    return pn


# FIXME: there is one entry in TaijitToaSutian that uses a slash to indicate
# multiple different sets of Han characters.
def process_title(title: str) -> str | None:
    """Process `title` to remove problematic characters, and so on.
    Return None if the title is empty so the call site can decide what to do."""
    title = title.strip()
    if title == "":
        return
    title = title.replace("?", "？")
    # A "省" in dict_concised is CJK COMPATIBILITY
    # IDEOGRAPH-F96D. I've reported the error.
    title = title.replace("省", "省")
    # This has a few uses in itaigi.
    title = title.replace("⿸疒哥", "𰣻")
    # This is only used once in itaigi:
    # https://itaigi.tw/k/%EF%97%AA%E8%8A%B3%E6%B0%B4/
    #
    # The problem is that it's in the private use area, and so even though it's
    # covered by HanaMinA I still am not quite sure what it is actually
    # supposed to look like.
    #
    # Given its pronunciation / meaning has another matching Han character,
    # this seems more correct.
    title = title.replace(chr(62954), "𫝺")
    # Work around chhoetaigi_taijittoasutian entries like "(**裝)模做樣". I
    # don't think the title is supposed to be like that.
    title = re.sub(r"\(\*\*(.+)\)", r"\1", title)
    title = re.sub(r"[][]", "", title)
    return title
