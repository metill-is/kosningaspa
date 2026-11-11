"""Re-parse the pinned pages into the parsed tables beside this script.

Run from the project root, after fetching the pins with the commands in README.md:

    python3 -I harness/sources/older_polls/parse_pins.py <dir of fetched pins> [<out dir>]

Each pin of pins.csv that names a parsed table is read from <dir>/<file>. A Wikipedia
response must hold the pinned revision and its wikitext must have the pinned sha256; a
Wayback capture must have the pinned sha256 as a whole. The parsed tables and manifest.csv
(each table's rows and sha256) are written to <out dir>, by default this script's folder,
and only once every check has passed. Standard library only; nothing is fetched.
"""
import csv
import hashlib
import html
import io
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
DASHES = {"", "-", "–", "—", "n/a"}

# Internal codes of the lists the tables carry; the README lists them.
EN13_LETTERS = {"C": "SAM", "V": "V", "S": "S", "T": "T", "L": "L", "Þ": "P", "A": "A", "B": "B",
                "D": "D", "G": "G"}
EN13_COLS = ["D", "B", "S", "V", "P", "A", "T", "G", "L", "SAM", "other"]
EN16_LETTERS = {"V": "V", "S": "S", "P": "P", "A": "A", "B": "B", "D": "D", "C": "C"}
EN16_COLS = ["D", "B", "S", "V", "P", "A", "C", "other"]
IS13_CELLS = ["fieldwork", "house", "sample", "names_party", "B", "D", "S", "V", "A", "T", "G", "L",
              "Þ", "C", "other"]
IS13_LETTERS = {"B": "B", "D": "D", "S": "S", "V": "V", "A": "A", "T": "T", "G": "G", "L": "L",
                "Þ": "P", "C": "SAM", "other": "other"}
CHART_LISTS = {"Framsóknarflokkur": "B", "Sjálfstæðisflokkur": "D",
               "Frjálslyndi fl.": "FF", "Borgarahreyfing": "O", "Samfylking": "S",
               "Vinstri græn": "V", "Hreyfingin": "HR", "Björt framtíð": "A",
               "Dögun": "T", "Hægri grænir": "G", "Samstaða": "SAM", "Píratar": "P",
               "Lýðræðisvaktin": "L", "Aðrir": "other"}
CHART_COLS = ["D", "B", "S", "V", "P", "A", "T", "G", "L", "SAM", "FF", "O", "HR", "other"]
MBL_LISTS = {"Sjálfstæðisflokkur": "D", "Framsóknarflokkur": "B", "Samfylkingin": "S",
             "Vinstri græn": "V", "Píratar": "P", "Björt framtíð": "A", "Dögun": "T",
             "Hægri grænir": "G", "Lýðræðisvaktin": "L", "Samstaða": "SAM",
             "Flokkur heimilanna": "I", "Regnboginn": "RB", "Alþýðufylkingin": "R",
             "Húmanistaflokkurinn": "H", "Landsbyggðarflokkurinn": "LB",
             "Borgarahreyfingin": "O", "Hreyfingin": "HR", "Framfaraflokkurinn": "FRAM",
             "Lýðræðishreyfingin": "LH", "Annað": "other"}
MBL_COLS = ["D", "B", "S", "V", "P", "A", "T", "G", "L", "SAM", "I", "RB", "R", "H", "LB", "O", "HR",
            "FRAM", "LH", "other"]
MONTHS_IS = {"janúar": 1, "febrúar": 2, "mars": 3, "apríl": 4, "maí": 5, "júní": 6,
             "júlí": 7, "ágúst": 8, "september": 9, "október": 10,
             "nóvember": 11, "desember": 12}

TABLES = {
    "wikipedia_en_2013.csv": ["row", "house", "date", "date_note", "cited"] + EN13_COLS,
    "wikipedia_is_2013.csv": ["row", "fieldwork", "house", "fieldwork_start", "fieldwork_end", "published",
                              "sample_text", "sample", "response_rate", "n_answering", "names_party"]
                             + ["D", "B", "S", "V", "P", "A", "T", "G", "L", "SAM", "other", "cited"],
    "wikipedia_en_2016.csv": ["row", "house", "date", "cited"] + EN16_COLS,
    "mmr_chart.csv": ["label", "fieldwork_end", "note"] + CHART_COLS,
    "mbl_box.csv": ["konnun_id", "house", "date"] + MBL_COLS,
    "mmr_items.csv": ["item", "capture", "fieldwork_start", "fieldwork_end", "svarfjoldi", "took_position"],
}


def fail(msg):
    raise SystemExit("parse_pins: " + msg)


def sha256(b):
    return hashlib.sha256(b).hexdigest()


def iso(y, m, d):
    return "%04d-%02d-%02d" % (int(y), int(m), int(d))


def share_text(x, where):
    """A printed share as plain decimal text; blank for a dash or a blank cell."""
    t = x.replace("%", "").replace(" ", " ").strip().replace(",", ".")
    if t in DASHES:
        return ""
    if not re.fullmatch(r"[0-9]+(\.[0-9]+)?", t):
        fail("not a share in %s: %r" % (where, x))
    return t


# ---------------------------------------------------------------- wikitext

NESTED = re.compile(r"\{\{[^{}]*\}\}|\[\[[^\[\]]*\]\]|\[https?://[^\[\]]*\]")


def mask(x):
    store = []
    while True:
        m = NESTED.search(x)
        if not m:
            return x, store
        store.append(m.group(0))
        x = x[:m.start()] + "\x01%d\x02" % (len(store) - 1) + x[m.end():]


def unmask(x, store):
    while True:
        m = re.search(r"\x01(\d+)\x02", x)
        if not m:
            return x
        x = x[:m.start()] + store[int(m.group(1))] + x[m.end():]


def split_cells(line):
    # WHY: a cell's attribute separator "|" also appears inside templates and links
    x, store = mask(line[1:])
    out = []
    for p in x.split("||"):
        a, v = p.split("|", 1) if "|" in p else ("", p)
        out.append((unmask(a, store), unmask(v, store)))
    return out


def urls_of(x):
    u = re.findall(r"\[(https?://[^ \]]+)", x)
    u += re.findall(r"url\s*=\s*(https?://[^|}\s]+)", x)
    return u


def clean(x):
    """Cell text without references, templates, links and markup, and the date of a dts template."""
    x = re.sub(r"<ref[^>]*/>", "", x)
    x = re.sub(r"(?s)<ref[^>]*>.*?</ref>", "", x)
    date = None
    while True:
        m = re.search(r"\{\{[^{}]*\}\}", x)
        if not m:
            break
        t = m.group(0)
        name = t[2:-2].split("|")[0].strip().lower()
        if name == "dts":
            nums = [p.strip() for p in t[2:-2].split("|")[1:] if "=" not in p]
            date = rep = iso(nums[0], nums[1], nums[2])
        elif name == "deprecated archive":
            tm = re.search(r"title\s*=\s*([^|}]*)", t)
            rep = tm.group(1) if tm else ""
        elif name == "usurped":
            rep = re.sub(r"^1=", "", t[2:-2].split("|", 1)[1])
        elif name in ("dead link", "webarchive", "óvirkur hlekkur", "cbignore", "note", "party color",
                      "ref"):
            rep = ""
        else:
            fail("unknown template in a table cell: " + t)
        x = x[:m.start()] + rep + x[m.end():]
    x = re.sub(r"\[\[(?:[^|\]]*\|)?([^\]]*)\]\]", r"\1", x)
    x = re.sub(r"\[https?://[^ \]]+(?: ([^\]]*))?\]", r"\1", x)
    x = re.sub(r"<[^>]+>", "", x)
    x = re.sub(r"'{2,3}", "", x)
    x = x.replace("&nbsp;", " ").replace("&amp;", "&")
    return re.sub(r"\s+", " ", x).strip(), date


def section(wikitext, name):
    lines = wikitext.split("\n")
    h = [i for i, x in enumerate(lines) if re.match(r"^==\s*" + re.escape(name) + r"\s*==\s*$", x)]
    if len(h) != 1:
        fail("section %r found %d times" % (name, len(h)))
    end = [i for i, x in enumerate(lines) if re.match(r"^==[^=]", x) and i > h[0]]
    return lines[h[0]:(end[0] if end else len(lines))]


def table_rows(lines):
    """The header lines and data rows (lists of (attribute, value) cells) of the section's table."""
    start = [i for i, x in enumerate(lines) if x.startswith("{|") and "wikitable" in x.lower()]
    if len(start) != 1:
        fail("expected one wikitable, found %d" % len(start))
    i1 = next(i for i in range(start[0], len(lines)) if lines[i].startswith("|}"))
    header, rows, cells = [], [], None
    for x in lines[start[0] + 1:i1]:
        if x.startswith("|-"):
            if cells:
                rows.append(cells)
            cells = []
        elif x.startswith("!"):
            if cells:
                rows.append(cells)
                cells = []
            if not rows:
                header.append(x)
        elif x.startswith("|"):
            if cells is None:
                cells = []
            cells.extend(split_cells(x))
        elif x.strip() and cells:
            a, v = cells[-1]
            cells[-1] = (a, v + "\n" + x)
    if cells:
        rows.append(cells)
    return header, rows


def wikitext_of(data, oldid):
    p = json.loads(data.decode("utf-8"))["parse"]
    if p["revid"] != oldid:
        fail("the response holds revision %s, not %s" % (p["revid"], oldid))
    return p["wikitext"]


def parse_wiki_en(wikitext, letters_map, cols, oldest_first):
    header, rows = table_rows(section(wikitext, "Opinion polls"))
    letters = []
    for h in header:
        m = re.search(r"\[\[[^\]|]*\|(?:<span[^>]*>)?([A-ZÞ])(?:</span>)?\]\]", h)
        if m:
            letters.append(m.group(1))
    if sorted(letters) != sorted(letters_map):
        fail("the header's list columns %s differ from the expected %s" % (letters, sorted(letters_map)))
    out, prev = [], None
    for i, cells in enumerate(rows, start=1):
        raw = [v for _, v in cells]
        if len(raw) < 3 + len(letters):
            fail("row %d has %d cells" % (i, len(raw)))
        house = clean(raw[0])[0]
        if "result" in house.lower():
            continue
        date = clean(raw[1])[1]
        if not date:
            fail("row %d has no dts date" % i)
        note = ""
        # WHY: the 2013 table runs oldest first, and one MMR row prints the previous year
        if oldest_first and prev and date < prev and int(prev[:4]) - int(date[:4]) == 1 and date[4:] >= prev[4:]:
            note = "printed %s, after %s in an oldest-first table: year read as %d" % (date, prev, int(date[:4]) + 1)
            date = str(int(date[:4]) + 1) + date[4:]
        prev = date
        rec = {"row": i, "house": house, "date": date, "date_note": note, "cited": " ".join(urls_of(raw[0]))}
        for L, v in zip(letters + ["other"], raw[2:3 + len(letters)]):
            rec[letters_map.get(L, "other")] = share_text(clean(v)[0], "row %d, %s" % (i, L))
        out.append(rec)
    return out


def parse_wiki_is13(wikitext):
    lines = section(wikitext, "Kannanir")
    i0 = next(i for i, x in enumerate(lines) if "wikitable" in x)
    i1 = next(i for i in range(i0, len(lines)) if lines[i].startswith("|}"))
    rows, cells = [], None
    for x in lines[i0 + 1:i1]:
        if x.startswith("|-"):
            if cells:
                rows.append(cells)
            cells = []
        elif x.startswith("|") and cells is not None:
            cells.extend(v for _, v in split_cells(x))
    if cells:
        rows.append(cells)
    out = []
    year, prev_month = 2013, None
    for i, c in enumerate(rows, start=1):
        if len(c) != len(IS13_CELLS):
            fail("is 2013 row %d has %d cells" % (i, len(c)))
        rec = {k: clean(v)[0] for k, v in zip(IS13_CELLS, c)}
        cited = " ".join(u for v in c for u in urls_of(v))
        if "kosningar" in rec["house"].lower():
            continue
        fw = rec["fieldwork"]
        m = re.match(r"^(\d+)\.\s*(\d+)\s*[–-]\s*(\d+)\.\s*(\d+)$", fw)
        b = re.match(r"^Birt\s+(\d+)\.(\d+)$", fw)
        if not (m or b):
            fail("is 2013 row %d: unparsed fieldwork %r" % (i, fw))
        end_m = int(m.group(4)) if m else int(b.group(2))
        # WHY: the table runs newest first but not strictly (a 19.2-4.3 row sits below a 28.2 row),
        # so only a jump from January back to December turns the year
        if prev_month is not None and end_m > prev_month + 6:
            year -= 1
        prev_month = end_m
        fs = fe = pub = ""
        if m:
            fe = iso(year, m.group(4), m.group(3))
            fs = iso(year - 1 if int(m.group(2)) > int(m.group(4)) else year, m.group(2), m.group(1))
            if fs > fe:
                fail("is 2013 row %d: fieldwork start after end" % i)
        else:
            pub = iso(year, b.group(2), b.group(1))
        s = rec["sample"].replace(".", "")
        sample = rr = ans = ""
        g = re.match(r"^(\d+)\s*\((\d+(?:,\d+)?)%\)$", s)
        f = re.match(r"^(\d+)\s*\((\d+)\)$", s)
        if g:
            sample, rr = g.group(1), g.group(2).replace(",", ".")
        elif f:
            sample, ans = f.group(1), f.group(2)
        elif re.fullmatch(r"\d+", s):
            ans = s
        took = rec["names_party"].replace("%", "").replace(",", ".").strip() if "%" in rec["names_party"] else ""
        out_rec = {"row": i, "fieldwork": fw, "house": rec["house"], "fieldwork_start": fs, "fieldwork_end": fe,
                   "published": pub, "sample_text": rec["sample"], "sample": sample, "response_rate": rr,
                   "n_answering": ans, "names_party": took, "cited": cited}
        for L, code in IS13_LETTERS.items():
            out_rec[code] = share_text(rec[L], "is 2013 row %d, %s" % (i, L))
        out.append(out_rec)
    return out


# ---------------------------------------------------------------- Wayback captures

def parse_chart(data):
    raw = data.decode("utf-8")
    m = re.search(r"var data_votechart\s*=\s*\{", raw)
    if not m:
        fail("the MMR capture has no data_votechart")
    i, depth = m.end() - 1, 0
    for j in range(i, len(raw)):
        if raw[j] == "{":
            depth += 1
        elif raw[j] == "}":
            depth -= 1
            if depth == 0:
                break
    txt = raw[i:j + 1]
    keep = dict(parse_float=str, parse_int=str)
    try:
        ch = json.loads(txt, **keep)
    except ValueError:
        ch = json.loads(re.sub(r",\s*([}\]])", r"\1", txt), **keep)
    labs = ch["x_axis"]["labels"]
    labels = labs["labels"] if isinstance(labs, dict) else labs
    out = []
    for k, lab in enumerate(labels):
        if lab.startswith("Kosn"):
            continue
        d, mo, y = lab.split(".")
        note = ""
        if lab == "29.1.2014":
            y = "2015"
            note = "label 29.1.2014 sits between 14.1.2015 and 19.2.2015: read as 2015-01-29"
        rec = {"label": lab, "fieldwork_end": iso(y, mo, d), "note": note}
        for c in CHART_COLS:
            rec[c] = ""
        for e in ch["elements"]:
            if e["text"] not in CHART_LISTS:
                fail("unknown list in the MMR chart: " + e["text"])
            v = e["values"][k] if k < len(e["values"]) else None
            if v is not None:
                rec[CHART_LISTS[e["text"]]] = v
        out.append(rec)
    return out


def parse_mbl(data):
    s = data.decode("utf-8")
    d = json.loads(re.search(r"flot_data\s*=\s*(\[.*?\]);", s, re.S).group(1), parse_float=str, parse_int=str)
    out = []
    for p in d:
        rec = {"konnun_id": p["konnun_id"], "house": p["konnun_name"], "date": p["konnun_isodate"][:10]}
        for c in MBL_COLS:
            rec[c] = ""
        for x in p["konnun_data"]:
            if x["label"] not in MBL_LISTS:
                fail("unknown list in the mbl.is box: " + x["label"])
            rec[MBL_LISTS[x["label"]]] = x["data"]
        out.append(rec)
    return sorted(out, key=lambda r: int(r["konnun_id"]))


def parse_mmr_item(data, item, capture):
    s = data.decode("utf-8", "replace")
    t = re.sub(r"(?is)<(script|style).*?</\1>", " ", s)
    t = re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", " ", t)))
    sv = re.search(r"Svarfj[öo]ldi:?\s*([\d.]+)", t)
    fw = re.search(r"Dagsetning framkv[æa]mdar:?\s*(\d+)\.\s*(?:([a-záéíóúýþæö]+)\s+)?"
                   r"til\s+(\d+)\.\s*([a-záéíóúýþæö]+)\s+(\d{4})", t)
    pos = re.search(r"Samtals voru ([\d,]+)% sem gáfu upp afstöðu", t)
    if not (sv and fw):
        fail("MMR item %s: no Svarfjöldi or fieldwork line" % item)
    y = int(fw.group(5))
    me = MONTHS_IS[fw.group(4)]
    ms = MONTHS_IS[fw.group(2)] if fw.group(2) else me
    return [{"item": item, "capture": capture,
             "fieldwork_start": iso(y if ms <= me else y - 1, ms, fw.group(1)), "fieldwork_end": iso(y, me, fw.group(3)),
             "svarfjoldi": sv.group(1).replace(".", ""),
             "took_position": pos.group(1).replace(",", ".") if pos else ""}]


# ---------------------------------------------------------------- main

def to_csv(cols, rows):
    buf = io.StringIO()
    w = csv.DictWriter(buf, fieldnames=cols, lineterminator="\n")
    w.writeheader()
    for r in rows:
        w.writerow({c: r.get(c, "") for c in cols})
    return buf.getvalue().encode("utf-8")


def main():
    if len(sys.argv) not in (2, 3):
        fail("usage: parse_pins.py <dir of fetched pins> [<out dir>]")
    src = sys.argv[1]
    out_dir = sys.argv[2] if len(sys.argv) == 3 else HERE
    pins = list(csv.DictReader(open(os.path.join(HERE, "pins.csv"), encoding="utf-8")))
    rows = {t: [] for t in TABLES}
    for p in pins:
        if p["table"] not in TABLES:
            continue
        path = os.path.join(src, p["file"])
        if not os.path.isfile(path):
            fail("missing fetched pin %s: %s" % (p["pin"], p["file"]))
        data = open(path, "rb").read()
        if p["kind"] == "wikipedia":
            oldid = int(p["identifier"].split()[-1])
            text = wikitext_of(data, oldid)
            if sha256(text.encode("utf-8")) != p["check_sha256"]:
                fail("%s: the wikitext is not the pinned text" % p["pin"])
            if p["table"] == "wikipedia_en_2013.csv":
                rows[p["table"]] += parse_wiki_en(text, EN13_LETTERS, EN13_COLS, oldest_first=True)
            elif p["table"] == "wikipedia_en_2016.csv":
                rows[p["table"]] += parse_wiki_en(text, EN16_LETTERS, EN16_COLS, oldest_first=False)
            else:
                rows[p["table"]] += parse_wiki_is13(text)
        else:
            if sha256(data) != p["check_sha256"]:
                fail("%s: the capture is not the pinned file" % p["pin"])
            if p["table"] == "mmr_chart.csv":
                rows[p["table"]] += parse_chart(data)
            elif p["table"] == "mbl_box.csv":
                rows[p["table"]] += parse_mbl(data)
            else:
                rows[p["table"]] += parse_mmr_item(data, p["pin"].split("_")[-1], p["identifier"].split()[-1])
    blobs = {t: to_csv(TABLES[t], rows[t]) for t in TABLES}
    for t, r in rows.items():
        if not r:
            fail("no rows for " + t)
    manifest = [{"table": t, "rows": len(rows[t]), "sha256": sha256(blobs[t])} for t in TABLES]
    blobs["manifest.csv"] = to_csv(["table", "rows", "sha256"], manifest)
    os.makedirs(out_dir, exist_ok=True)
    for name, b in blobs.items():
        with open(os.path.join(out_dir, name), "wb") as f:
            f.write(b)
    for m in manifest:
        print("%-24s %4d rows  %s" % (m["table"], m["rows"], m["sha256"]))


if __name__ == "__main__":
    main()
