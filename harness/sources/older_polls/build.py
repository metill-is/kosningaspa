"""Build harness/older_polls.csv from the committed files beside this script.

Run from the project root:

    python3 -I harness/sources/older_polls/build.py [<out csv>]

It reads polls.csv (the polls and the source rows each rests on), judgements.csv, the parsed
tables, print_reports.csv and snapshot_sizes.csv, and writes the table (by default
harness/older_polls.csv). It reads no other file and stops on any inconsistency.

    python3 -I harness/sources/older_polls/build.py --snapshot-sizes [<out csv>]

rewrites snapshot_sizes.csv from harness/snapshot/polling_data.csv, after checking that file
against harness/snapshot_manifest.csv. Standard library only.
"""
import collections
import csv
import datetime as dt
import hashlib
import io
import math
import os
import statistics
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
TABLE = os.path.join(ROOT, "harness", "older_polls.csv")

GALLUP, MMR, FEL = "Gallup", "MMR", "Félagsvísindastofnun"
FBL = "Fréttablaðið, Stöð 2 og Vísir"
HOUSES = (GALLUP, MMR, FBL, FEL)
PERIOD = ("2012-07-01", "2015-12-31")
POOL_END = "2016-10-29"
POOL_DAYS = 730
OFFSET_DAYS = 400
CODES = ["D", "B", "S", "V", "P"]
PARTIES = ["Sjálfstæðisflokkurinn", "Framsóknarflokkurinn", "Samfylkingin", "Vinstri Græn",
           "Píratar", "Viðreisn", "Flokkur Fólksins", "Miðflokkurinn",
           "Sósíalistaflokkurinn", "Annað"]
CODE_PARTY = dict(zip(CODES, PARTIES))
ANNAD = PARTIES[-1]
STATUSES = ("added", "replacement", "in_snapshot", "pool", "excluded")
SHARE_PRIORITY = {MMR: ["chart", "en2013", "is2013", "en2016", "mbl", "print"],
                  "other": ["en2013", "is2013", "en2016", "mbl", "print"]}
FIELDWORK_PRIORITY = ["mmr_item", "print", "is2013", "chart"]
MISPRINT_RATE = 20.0
EPS = 1e-9
OUT_COLS = ["date", "fyrirtaeki", "flokkur", "n", "share", "n_total", "n_source", "fieldwork_start",
            "fieldwork_end", "published", "poll_id", "pins", "flags", "replaces"]


def fail(msg):
    raise SystemExit("build: " + msg)


def read(name):
    with open(os.path.join(HERE, name), encoding="utf-8", newline="") as f:
        return list(csv.DictReader(f))


def day(x):
    return dt.date.fromisoformat(x)


def days(a, b):
    return (day(a) - day(b)).days


def plus(x, k):
    return (day(x) + dt.timedelta(days=k)).isoformat()


def midpoint(start, end):
    # WHY: R's `+.Date` rounds a difftime with round(), half to even, and so does Python's round()
    return plus(start, round(days(end, start) / 2))


def num(x, k=9):
    return ("%.*f" % (k, x)).rstrip("0").rstrip(".")


def fnum(x):
    return float(x) if x not in ("", None) else None


def part_values(j):
    """{poll id: part} from a replace judgement's value: the part of the snapshot key each poll is."""
    if j["kind"] != "replace" or not j["value"]:
        return {}
    return dict(x.split("=") for x in j["value"].split(";"))


# ---------------------------------------------------------------- sources

def obs(src, ref, **kw):
    o = dict(src=src, ref=ref, shares={}, qualified={}, fw_start="", fw_end="", published="", release="",
             sample=None, rr=None, ans=None, method_only=False, cited="")
    o.update(kw)
    return o


def load_sources():
    out = {}
    for r in read("wikipedia_en_2013.csv"):
        out["en2013:" + r["row"]] = obs("en2013", "en2013:" + r["row"], release=r["date"], cited=r["cited"],
                                        shares={c: float(r[c]) for c in CODES if r[c]})
    for r in read("wikipedia_en_2016.csv"):
        if r["C"] and r["date"] < "2016-01-01":
            fail("en2016 row %s gives Viðreisn before 2016" % r["row"])
        out["en2016:" + r["row"]] = obs("en2016", "en2016:" + r["row"], release=r["date"], cited=r["cited"],
                                        shares={c: float(r[c]) for c in CODES if r[c]})
    for r in read("wikipedia_is_2013.csv"):
        rr = fnum(r["response_rate"])
        out["is2013:" + r["row"]] = obs("is2013", "is2013:" + r["row"], fw_start=r["fieldwork_start"],
                                        fw_end=r["fieldwork_end"], published=r["published"], cited=r["cited"],
                                        shares={c: float(r[c]) for c in CODES if r[c]}, sample=fnum(r["sample"]),
                                        rr=rr, ans=fnum(r["n_answering"]),
                                        misprint=rr is not None and rr < MISPRINT_RATE)
    for r in read("mmr_chart.csv"):
        out["chart:" + r["label"]] = obs("chart", "chart:" + r["label"], fw_end=r["fieldwork_end"],
                                         shares={c: float(r[c]) for c in CODES if r[c]})
    for r in read("mbl_box.csv"):
        out["mbl:" + r["konnun_id"]] = obs("mbl", "mbl:" + r["konnun_id"], release=r["date"],
                                           shares={c: float(r[c]) for c in CODES if r[c]})
    for r in read("mmr_items.csv"):
        out["mmr_item:" + r["item"]] = obs("mmr_item", "mmr_item:" + r["item"], fw_start=r["fieldwork_start"],
                                           fw_end=r["fieldwork_end"], ans=float(r["svarfjoldi"]), method_only=True)
    reports = collections.OrderedDict()
    for r in read("print_reports.csv"):
        o = reports.setdefault(r["report"], obs("print", "print:" + r["report"], house=r["house"], pages=[]))
        if r["page_id"] not in o["pages"]:
            o["pages"].append(r["page_id"])
        f, v, q = r["figure"], r["value"], r["qualifier"]
        if f in CODES:
            if f in o["shares"] or f in o["qualified"]:
                fail("print %s gives %s twice" % (r["report"], f))
            (o["qualified"] if q else o["shares"])[f] = float(v)
        elif f in ("fieldwork_start", "fieldwork_end"):
            o["fw_start" if f == "fieldwork_start" else "fw_end"] = v
        elif f == "first_print":
            o["published"] = v
        elif f in ("sample", "response_rate", "n_answering"):
            if not q:
                o[{"sample": "sample", "response_rate": "rr", "n_answering": "ans"}[f]] = float(v)
        else:
            fail("print %s: unknown figure %s" % (r["report"], f))
    for o in reports.values():
        o["method_only"] = not (o["shares"] or o["qualified"])
        out[o["ref"]] = o
    return out


# ---------------------------------------------------------------- per poll

def poll_shares(p):
    pr = SHARE_PRIORITY.get(p["house"], SHARE_PRIORITY["other"])
    ms = sorted((m for m in p["members"] if m["shares"] or m["qualified"]), key=lambda m: pr.index(m["src"]))
    vals, qual = collections.defaultdict(list), {}
    for m in ms:
        for c, v in m["shares"].items():
            vals[c].append((v, m["src"], m["ref"]))
        for c, v in m["qualified"].items():
            qual.setdefault(c, v)
    primary = {}
    for c, lst in vals.items():
        chart = [v for v, s, _ in lst if s == "chart"]
        if chart:
            primary[c] = chart[0]
            continue
        xs = sorted(lst, key=lambda t: t[0])
        k = len(xs)
        if k % 2:
            primary[c] = xs[k // 2][0]
        else:
            a, b = xs[k // 2 - 1], xs[k // 2]
            primary[c] = a[0] if pr.index(a[1]) <= pr.index(b[1]) else b[0]
    qual_only = {c: v for c, v in qual.items() if c not in primary}
    spread = (0.0, "")
    for c in CODES:
        refs = {r for _, _, r in vals.get(c, [])}
        if len(refs) >= 2:
            d = max(v for v, _, _ in vals[c]) - min(v for v, _, _ in vals[c])
            if d > spread[0] + EPS:
                spread = (round(d, 3), c)
    return primary, qual_only, spread


def fieldwork(p):
    fws = sorted((m for m in p["members"] if m["fw_end"] and m["src"] in FIELDWORK_PRIORITY),
                 key=lambda m: FIELDWORK_PRIORITY.index(m["src"]))
    with_start = [m for m in fws if m["fw_start"]]
    chosen = with_start[0] if with_start else (fws[0] if fws else None)
    notes = []
    for m in fws:
        if chosen is None or m is chosen:
            continue
        if m["src"] == "chart":
            if not 0 <= days(m["fw_end"], chosen["fw_end"]) <= 1:
                notes.append("chart label %s against fieldwork end %s" % (m["fw_end"], chosen["fw_end"]))
            continue
        ds = abs(days(m["fw_start"], chosen["fw_start"])) if (m["fw_start"] and chosen["fw_start"]) else 0
        if ds > 1 or abs(days(m["fw_end"], chosen["fw_end"])) > 1:
            notes.append("%s gives %s to %s" % (m["ref"], m["fw_start"] or "?", m["fw_end"]))
    return chosen, notes


def published_size(p):
    """(n, how) from a pinned source, in the snapshot's meaning of n, or None."""
    h, ms = p["house"], p["members"]

    def first(srcs, test):
        for s in srcs:
            for m in ms:
                if m["src"] == s and test(m):
                    return m
        return None

    if h in (GALLUP, FEL):
        m = first(["print", "is2013"], lambda m: m["sample"] and m["rr"] and not m.get("misprint"))
        if m:
            return m["sample"] * m["rr"] / 100, "%s: sample %s x response rate %s%%" % (m["ref"], num(m["sample"]), num(m["rr"]))
        if h == FEL:
            m = first(["print", "is2013"], lambda m: m["ans"])
            if m:
                return m["ans"], "%s: %s answering, no sample printed" % (m["ref"], num(m["ans"]))
        return None
    if h == MMR:
        m = first(["mmr_item"], lambda m: m["ans"])
        if m:
            return m["ans"], "%s: Svarfjöldi %s" % (m["ref"], num(m["ans"]))
        m = first(["print", "is2013"], lambda m: m["ans"] and not m["method_only"])
        if m:
            return m["ans"], "%s: %s respondents" % (m["ref"], num(m["ans"]))
        m = first(["print"], lambda m: m["ans"] and m["method_only"])
        if m:
            return m["ans"], "%s: %s respondents to the same survey wave" % (m["ref"], num(m["ans"]))
        return None
    m = first(["print", "is2013"], lambda m: m["ans"])
    if m:
        return m["ans"], "%s: %s reached" % (m["ref"], num(m["ans"]))
    return None


def printed_rate(p):
    for s in ("print", "is2013"):
        for m in p["members"]:
            if m["src"] == s and m["rr"] and not m.get("misprint"):
                return m["rr"]
    return None


def printed_sample(p):
    for s in ("print", "is2013"):
        for m in p["members"]:
            if m["src"] == s and m["sample"]:
                return m["sample"], m["ref"]
    return None, None


# ---------------------------------------------------------------- build

def build():
    sources = load_sources()
    judgements = collections.OrderedDict((j["id"], j) for j in read("judgements.csv"))
    register = read("polls.csv")
    sizes = read("snapshot_sizes.csv")
    seen = set()
    polls = []
    for r in register:
        if r["poll_id"] in seen:
            fail("poll %s listed twice" % r["poll_id"])
        seen.add(r["poll_id"])
        if r["status"] not in STATUSES:
            fail("%s: unknown status %s" % (r["poll_id"], r["status"]))
        if r["fyrirtaeki"] not in HOUSES and r["status"] != "excluded":
            fail("%s: house %s is not one of the four" % (r["poll_id"], r["fyrirtaeki"]))
        refs = [x for x in r["sources"].split(";") if x]
        missing = [x for x in refs if x not in sources]
        if missing:
            fail("%s names source rows that no committed table holds: %s" % (r["poll_id"], ", ".join(missing)))
        js = [x for x in r["judgements"].split(";") if x]
        if any(j not in judgements for j in js):
            fail("%s names an unknown judgement" % r["poll_id"])
        polls.append(dict(id=r["poll_id"], house=r["fyrirtaeki"], status=r["status"], key=r["snapshot_key"],
                          match=r["match"], refs=refs, members=[sources[x] for x in refs], judgements=js))
    for j in judgements.values():
        named = {p["id"] for p in polls if j["id"] in p["judgements"]}
        if j["scope"] == "poll" and named != set(j["applies_to"].split(";")):
            fail("judgement %s applies to %s, but polls.csv names it for %s" % (j["id"], j["applies_to"], ";".join(sorted(named))))
        if j["scope"] != "poll" and named:
            fail("judgement %s is not a poll's, but polls.csv names it" % j["id"])
        parts = part_values(j)
        if parts and (set(parts) != set(j["applies_to"].split(";")) or len(set(parts.values())) != len(parts)):
            fail("judgement %s must give each of its polls a part of its own" % j["id"])

    def estimate_of(p):
        for j in p["judgements"]:
            if judgements[j]["kind"] == "estimate":
                v = dict(x.split("=") for x in judgements[j]["value"].split(";"))
                return int(v["gap_days"]), int(v["length_days"])
        return None

    def held_by(p):
        held = [s for s in sizes if s["date"] == p["key"] and s["fyrirtaeki"] == p["house"]]
        if not held:
            fail("%s replaces %s, which snapshot_sizes.csv does not hold" % (p["id"], p["key"]))
        if len(held) > 2:
            fail("%s replaces a key of %d polls; only a key of two half samples is provided for" % (p["id"], len(held)))
        return held

    def snapshot_size(p):
        held = held_by(p)
        if len(held) == 1:
            return float(held[0]["n_total"]), "the snapshot row it replaces has n_total %s" % held[0]["n_total"]
        part = next((v[p["id"]] for v in map(part_values, (judgements[j] for j in p["judgements"])) if p["id"] in v), None)
        hit = [s for s in held if s["part"] == part]
        if len(hit) != 1:
            fail("%s replaces one of the two polls under %s, and no judgement names its part" % (p["id"], p["key"]))
        return 2 * float(hit[0]["n_total"]), (
            "twice its half: part %s of the snapshot key, which holds 2 polls each at about half its sample, has n_total %s"
            % (part, hit[0]["n_total"]))

    # chart-label offsets: the snapshot's date against the chart label of each strictly matched poll
    offsets = []
    for p in polls:
        chart = [m for m in p["members"] if m["src"] == "chart"]
        if p["key"] and p["match"] == "strict" and chart:
            offsets.append((chart[0]["fw_end"], days(p["key"], chart[0]["fw_end"])))

    active = [p for p in polls if p["status"] in ("added", "replacement", "pool")]
    for p in active:
        chosen, notes = fieldwork(p)
        p["fw_notes"], p["flags"] = notes, []
        p["fieldwork_start"] = p["fieldwork_end"] = ""
        if chosen and chosen["fw_start"]:
            p["fieldwork_start"], p["fieldwork_end"] = chosen["fw_start"], chosen["fw_end"]
            p["date"] = midpoint(chosen["fw_start"], chosen["fw_end"])
        elif chosen:
            p["fieldwork_end"] = chosen["fw_end"]
            if p["status"] == "replacement" and p["match"] in ("strict", "loose"):
                p["date"] = p["key"]
                p["flags"].append("date is the snapshot key's")
            elif p["status"] == "pool":
                p["date"] = chosen["fw_end"]
            else:
                near = [o for lab, o in offsets if abs(days(lab, chosen["fw_end"])) <= OFFSET_DAYS]
                if len(near) < 3:
                    fail("%s: fewer than three chart offsets within %d days" % (p["id"], OFFSET_DAYS))
                k = math.floor(statistics.median(near))
                p["date"] = plus(chosen["fw_end"], k)
                p["flags"].append("fieldwork end is MMR's chart label; date estimated as the label %+d days" % k)
        else:
            est = estimate_of(p)
            dates = sorted(m["published"] or m["release"] for m in p["members"] if m["published"] or m["release"])
            if p["status"] == "pool" and dates:
                p["date"] = dates[0]
            elif est and dates:
                end = plus(dates[0], -est[0])
                p["date"] = midpoint(plus(end, -est[1]), end)
                p["flags"].append("date estimated from the release date %s" % dates[0])
            else:
                fail("%s: no fieldwork, and no estimate" % p["id"])
        pubs = [m["published"] for m in p["members"] if m["src"] in ("print", "is2013") and m["published"]]
        p["published"] = min(pubs) if pubs else ""
        r = published_size(p)
        p["n"], p["n_source"], p["n_how"] = (r[0], "published", r[1]) if r else (None, "", "")

    # Pirates count as a poll party of a house from its first share for them
    p_first = {}
    for p in sorted(active, key=lambda p: p["date"]):
        if any("P" in m["shares"] or "P" in m["qualified"] for m in p["members"]):
            p_first.setdefault(p["house"], p["date"])

    # the size pool: one published size per distinct poll of the house
    pool = collections.defaultdict(list)
    for s in sizes:
        if s["date"] >= POOL_END:
            fail("snapshot_sizes.csv holds a poll dated from %s" % POOL_END)
        if s["parts"] != "1":
            continue
        pool[s["fyrirtaeki"]].append((s["date"], float(s["n_total"]), "snapshot %s" % s["date"]))
    for p in active:
        if p["status"] in ("added", "pool") and p["n_source"] == "published":
            pool[p["house"]].append((p["date"], p["n"], p["id"]))
    rates = collections.defaultdict(list)
    for p in active:
        if p["status"] in ("added", "pool"):
            rr = printed_rate(p)
            if rr:
                rates[p["house"]].append((p["date"], rr))

    rows = []
    for p in active:
        if p["status"] == "pool":
            continue
        if not PERIOD[0] <= p["date"] <= PERIOD[1]:
            fail("%s is dated %s, outside the period" % (p["id"], p["date"]))
        if p["id"].split("-", 1)[1][:10] != p["date"]:
            fail("%s is dated %s: its id must carry its date" % (p["id"], p["date"]))
        if p["n_source"] != "published" and p["status"] == "replacement":
            p["n"], p["n_how"] = snapshot_size(p)
            p["n_source"] = "snapshot"
        elif p["n_source"] != "published":
            near = [n for d, n, _ in pool[p["house"]] if abs(days(d, p["date"])) <= POOL_DAYS]
            sample, sref = printed_sample(p)
            if p["house"] in (GALLUP, FEL) and sample:
                rs = [x for d, x in rates[p["house"]] if abs(days(d, p["date"])) <= POOL_DAYS]
                if not rs:
                    fail("%s: no response rate within %d days" % (p["id"], POOL_DAYS))
                m = statistics.median(rs)
                p["n"] = sample * m / 100
                p["n_how"] = "sample %s (%s) x the median response rate %s%% of %d polls within %d days" % (
                    num(sample), sref, num(m), len(rs), POOL_DAYS)
            else:
                if not near:
                    fail("%s: no published size within %d days" % (p["id"], POOL_DAYS))
                p["n"] = statistics.median(near)
                p["n_how"] = "the median of %d published sizes within %d days" % (len(near), POOL_DAYS)
            p["n_source"] = "assumed"
        primary, qual_only, spread = poll_shares(p)
        given = dict(primary)
        given.update(qual_only)
        existing = ["D", "B", "S", "V"] + (["P"] if p["house"] in p_first and p["date"] >= p_first[p["house"]] else [])
        if any(c not in existing for c in given):
            fail("%s gives a share for a party the house had not yet reported" % p["id"])
        missing = [c for c in existing if c not in given]
        flags = []
        if "P" not in existing:
            flags.append("P not yet reported by this house: inside Annað")
        if missing:
            flags.append("not reported: %s, Annað" % ", ".join(missing))
        if qual_only:
            flags.append("rounded or in words in print: %s" % ", ".join(c for c in CODES if c in qual_only))
        if spread[0] > 0.5 + EPS:
            flags.append("sources differ by %s points on %s" % (num(spread[0], 3), spread[1]))
        if p["fw_notes"]:
            flags.append("fieldwork differs: " + "; ".join(p["fw_notes"]))
        flags += p["flags"]
        flags.append("n %s: %s" % (p["n_source"], p["n_how"]))
        if p["status"] == "replacement" and p["n_source"] == "published":
            held = held_by(p)
            if len(held) == 1:
                flags.append("the snapshot row it replaces has n_total %s" % held[0]["n_total"])
            else:
                flags.append("the snapshot key it replaces holds %d polls, each at about half its sample" % len(held))
        flags += [judgements[j]["flag"] for j in p["judgements"] if judgements[j]["flag"]]
        annad = None if missing else 100 - sum(given.values())
        if annad is not None and annad < 0:
            fail("%s: negative Annað" % p["id"])
        for party in PARTIES:
            code = next((c for c in CODES if CODE_PARTY[c] == party), None)
            share = annad if party == ANNAD else given.get(code)
            rows.append({
                "date": p["date"], "fyrirtaeki": p["house"], "flokkur": party,
                "n": num(share / 100 * p["n"], 6) if share is not None else "0",
                "share": num(round(share, 9)) if share is not None else "",
                "n_total": num(p["n"], 6), "n_source": p["n_source"], "fieldwork_start": p["fieldwork_start"],
                "fieldwork_end": p["fieldwork_end"], "published": p["published"], "poll_id": p["id"],
                "pins": ";".join(p["refs"]), "flags": "; ".join(flags),
                "replaces": p["key"] if p["status"] == "replacement" else "",
            })
    order = {party: i for i, party in enumerate(PARTIES)}
    rows.sort(key=lambda r: (r["date"], r["fyrirtaeki"], r["poll_id"], order[r["flokkur"]]))
    return rows


def to_csv(cols, rows):
    buf = io.StringIO()
    w = csv.DictWriter(buf, fieldnames=cols, lineterminator="\n")
    w.writeheader()
    w.writerows(rows)
    return buf.getvalue().encode("utf-8")


def snapshot_sizes(out):
    path = os.path.join(ROOT, "harness", "snapshot", "polling_data.csv")
    manifest = os.path.join(ROOT, "harness", "snapshot_manifest.csv")
    with open(manifest, encoding="utf-8", newline="") as f:
        want = next(r["sha256"] for r in csv.DictReader(f) if r["name"] == "polling_data")
    data = open(path, "rb").read()
    if hashlib.sha256(data).hexdigest() != want:
        fail("harness/snapshot/polling_data.csv is not the file the manifest names")
    groups = collections.OrderedDict()
    for r in csv.DictReader(io.StringIO(data.decode("utf-8"))):
        if r["fyrirtaeki"] in HOUSES and r["date"] < POOL_END:
            groups.setdefault((r["date"], r["fyrirtaeki"]), []).append(r)
    rows = []
    for (date, house), rs in sorted(groups.items()):
        parts, seen = [], collections.Counter()
        # WHY: a key holding two polls lists each party twice (MMR 2013-05-16); split by occurrence
        for r in rs:
            k = seen[r["flokkur"]]
            seen[r["flokkur"]] += 1
            while len(parts) <= k:
                parts.append(0.0)
            parts[k] += float(r["n"])
        for j, n in enumerate(parts, start=1):
            rows.append({"date": date, "fyrirtaeki": house, "part": j, "parts": len(parts), "n_total": num(n, 6)})
    with open(out, "wb") as f:
        f.write(to_csv(["date", "fyrirtaeki", "part", "parts", "n_total"], rows))
    print("%d snapshot polls written to %s" % (len(rows), os.path.relpath(out, ROOT)))


def main():
    args = sys.argv[1:]
    if args and args[0] == "--snapshot-sizes":
        snapshot_sizes(args[1] if len(args) > 1 else os.path.join(HERE, "snapshot_sizes.csv"))
        return
    out = args[0] if args else TABLE
    rows = build()
    with open(out, "wb") as f:
        f.write(to_csv(OUT_COLS, rows))
    polls = collections.Counter((r["fyrirtaeki"], r["date"][:4]) for r in rows if r["flokkur"] == ANNAD)
    print("%d rows, %d polls written to %s" % (len(rows), sum(polls.values()), os.path.relpath(out, ROOT)))


if __name__ == "__main__":
    main()
