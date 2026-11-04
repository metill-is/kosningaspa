#!/usr/bin/env python3
"""Build the kosningaspá methodology notes: chapter pages and the contents page.

Adapted from the build script of the author's PhD method notes (29 Oct 2026); same page model.

The table of contents lives in site.json. Each chapter page keeps its body between the
markers <!-- content:start --> and <!-- content:end -->; this tool rewrites everything
around the body (head, breadcrumb, title, review stamp, the contents sidebar built from
the body's <section id><h2> pairs, previous/next links, footer) and leaves the body as
it is. Edit a chapter's body in the page itself and re-run the tool to refresh the chrome.

Usage (from the notes root, docs/method/):
  python3 tools/build.py                      # refresh every page and index.html
  python3 tools/build.py --insert 3.1 FILE    # put FILE's fragment in as chapter 3.1's body
  python3 tools/build.py --check              # report pages whose chrome is out of date; writes nothing

A fragment starts with <!--meta {json} --> (id, title, sub, kicker, review, sources, and an
optional snapshot date that overrides the default) followed by <section> elements. The
meta comment stays inside the body.
"""
import argparse
import html
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
START, END = "<!-- content:start -->", "<!-- content:end -->"
SNAPSHOT = "29 Oct 2026"
KATEX = "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist"
HLJS = "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/highlight.min.js"


def load_site():
    return json.loads((ROOT / "site.json").read_text())


def parse_meta(body):
    m = re.search(r"<!--meta\s*(\{.*?\})\s*-->", body, re.S)
    if not m:
        return {}
    try:
        return json.loads(m.group(1))
    except json.JSONDecodeError as e:
        sys.exit(f"meta JSON does not parse: {e}")


def sections(body):
    out = []
    for m in re.finditer(r'<section\s+id="([^"]+)"[^>]*>\s*<h2[^>]*>(.*?)</h2>', body, re.S):
        label = re.sub(r"<[^>]+>", "", m.group(2)).strip()
        out.append((m.group(1), re.sub(r"\s+", " ", label)))
    return out


def rel(from_path, to_path):
    """Relative href from one chapter page to another (both relative to the notes root)."""
    up = len(Path(from_path).parent.parts)
    return "../" * up + str(Path(to_path))


def esc(s):
    return html.escape(s, quote=True)


def page(site, ch, body):
    # Link only to written chapters, so the pager never points at a page that does not exist yet.
    chs = [c for c in site["chapters"] if c["id"] == ch["id"] or (ROOT / c["path"]).exists()]
    i = next(k for k, c in enumerate(chs) if c["id"] == ch["id"])
    prev_c = chs[i - 1] if i > 0 else None
    next_c = chs[i + 1] if i + 1 < len(chs) else None
    meta = parse_meta(body)
    title = meta.get("title", ch["title"])
    sub = meta.get("sub", "")
    kicker = meta.get("kicker") or ch["part"]
    review = meta.get("review", "Not reviewed by the author.")
    snapshot = meta.get("snapshot", SNAPSHOT)
    up = "../" * len(Path(ch["path"]).parent.parts)
    toc = "\n".join(f'      <li><a href="#{esc(sid)}">{label}</a></li>' for sid, label in sections(body))
    pager = []
    if prev_c:
        pager.append(f'<a href="{rel(ch["path"], prev_c["path"])}"><span class="lbl">Previous</span>{esc(prev_c["id"])} {esc(prev_c["title"])}</a>')
    else:
        pager.append("<span></span>")
    if next_c:
        pager.append(f'<a class="next" href="{rel(ch["path"], next_c["path"])}"><span class="lbl">Next</span>{esc(next_c["id"])} {esc(next_c["title"])}</a>')
    return f"""<!doctype html>
<html lang="en-GB">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{esc(ch["id"])} {esc(title)} · Kosningaspá method</title>
<meta name="description" content="{esc(sub)}">
<link rel="stylesheet" href="{KATEX}/katex.min.css">
<link rel="stylesheet" href="{up}assets/method.css">
</head>
<body>
<div class="wrap">

<nav class="breadcrumb"><a href="{up}index.html">Kosningaspá method</a> <span class="sep">/</span> {esc(ch["part"])} <span class="sep">/</span> {esc(ch["id"])}</nav>

<header class="top">
  <div class="kicker">Kosningaspá · Method · {esc(kicker)}</div>
  <h1><span class="chnum">{esc(ch["id"])}</span> {esc(title)}</h1>
  <p class="sub">{esc(sub)}</p>
  <div class="stamp"><b>Review state.</b> {esc(review)} <b>Snapshot.</b> Describes the code and records as they stood on {esc(snapshot)}: kosningaspa <code>master</code> unless a box names a branch. Live state: kosningaspa <code>master</code>.</div>
</header>

<div class="layout">
  <nav class="toc">
    <div class="t">On this page</div>
    <ol>
{toc}
    </ol>
    <div class="book"><a href="{up}index.html">Contents</a></div>
  </nav>

  <main>
{START}
{body.strip()}
{END}
    <nav class="pager">{"".join(pager)}</nav>
  </main>
</div>

<footer>
  Methodology notes · chapter {esc(ch["id"])} · drafted with Claude from kosningaspá's code and records ·
  page chrome built by <code>tools/build.py</code> from <code>site.json</code> (edit the body between the content markers, then re-run it) ·
  maths by KaTeX 0.16.9, code by highlight.js 11.9 (CDN; the page reads fine offline).
</footer>
</div>
<script defer src="{KATEX}/katex.min.js"></script>
<script defer src="{KATEX}/contrib/auto-render.min.js"></script>
<script defer src="{HLJS}"></script>
<script defer src="{up}assets/method.js"></script>
</body>
</html>
"""


def body_of(path):
    if not path.exists():
        return None
    t = path.read_text()
    if START not in t or END not in t:
        sys.exit(f"{path}: content markers missing")
    return t.split(START, 1)[1].split(END, 1)[0]


def index_page(site, bodies):
    parts_html = []
    for part in site["parts"]:
        rows = []
        for ch in [c for c in site["chapters"] if c["part_dir"] == part["dir"]]:
            b = bodies.get(ch["id"])
            meta = parse_meta(b) if b else {}
            written = b is not None
            dot = '<span class="dot done">●</span>' if written else '<span class="dot todo">○</span>'
            title = meta.get("title", ch["title"])
            link = f'<a href="{ch["path"]}">{esc(title)}</a>' if written else esc(title)
            sub = esc(meta.get("sub", "")) if written else "planned"
            review = esc(meta.get("review", "")) if written else ""
            rows.append(f'''      <li><span class="snum">{esc(ch["id"])}</span><div>{link}<div class="cdesc">{sub}</div>{f'<div class="crev">{review}</div>' if review else ''}</div><span class="st">{dot}</span></li>''')
        parts_html.append(f'''  <section class="part">
    <h2>{esc(part["title"])}</h2>
    <ul class="chapters">
{chr(10).join(rows)}
    </ul>
  </section>''')
    written = sum(1 for c in site["chapters"] if c["id"] in bodies)
    tmpl = (ROOT / "_index_template.html").read_text()
    return (tmpl.replace("{{PARTS}}", "\n".join(parts_html))
                .replace("{{N_WRITTEN}}", str(written))
                .replace("{{N_TOTAL}}", str(len(site["chapters"]))))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--insert", nargs=2, metavar=("ID", "FRAGMENT"), action="append", default=[])
    ap.add_argument("--check", action="store_true")
    a = ap.parse_args()
    site = load_site()
    by_id = {c["id"]: c for c in site["chapters"]}
    inserts = {}
    for cid, frag in a.insert:
        if cid not in by_id:
            sys.exit(f"unknown chapter id {cid}")
        inserts[cid] = Path(frag).read_text()
        meta = parse_meta(inserts[cid])
        if meta.get("id") and meta["id"] != cid:
            sys.exit(f"{frag}: meta id {meta['id']} does not match {cid}")
    bodies, stale = {}, []
    for ch in site["chapters"]:
        p = ROOT / ch["path"]
        b = inserts.get(ch["id"]) or body_of(p)
        if b is None:
            continue
        bodies[ch["id"]] = b
        new = page(site, ch, b)
        if a.check:
            if not p.exists() or p.read_text() != new:
                stale.append(ch["path"])
            continue
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(new)
    idx = index_page(site, bodies)
    if a.check:
        if not (ROOT / "index.html").exists() or (ROOT / "index.html").read_text() != idx:
            stale.append("index.html")
        print("stale: " + (", ".join(stale) if stale else "none"))
        sys.exit(1 if stale else 0)
    (ROOT / "index.html").write_text(idx)
    print(f"built {len(bodies)} chapter pages and index.html")


if __name__ == "__main__":
    main()
