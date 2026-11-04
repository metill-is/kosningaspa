# Kosningaspá methodology notes

Living HTML notes on how the kosningaspá models work: the forecast model, the Fylgisvakt polling smoother and the v2 work in progress. They exist so the author can stay current with the modelling as it changes. Plain, self-contained HTML; mathematics typeset with KaTeX from CDN; one build script refreshes the page chrome.

**Public.** These pages are tracked in the public kosningaspa repository. A chapter cites only what a reader of the repository could check: code and commits on `master` (or a named branch), data the repository reads, and dated fits in the git-ignored `data/<date>/` folders. Unmerged work may be described in a branch box that names the branch and says whether it is pushed. The author's private project notes (the Obsidian vault) may be named as a source, labelled as private, but not quoted. Keep out local-only reports in git-excluded folders, absolute or home-relative paths, and survey-derived figures whose licence forbids redistribution (the 1983–2017 ÍSKOS waves).

## Reading

Open `docs/method/index.html` in a browser (double-click it in Finder, or `open docs/method/index.html`). Links between pages resolve as files, so no server is needed. KaTeX and highlight.js load from CDN; offline, the pages still read, with raw TeX.

## Layout

```
index.html              ← contents (built; edit _index_template.html)
site.json               ← table of contents: parts, chapters, page paths
_index_template.html    ← template of index.html
assets/                 ← method.css, method.js (shared by every chapter)
tools/build.py          ← refreshes every chapter's chrome and index.html
analysis/<chapter>/     ← scripts that computed a chapter's own numbers, with a README
start/ forecast/ fylgisvakt/ bias/ v2/ history/   ← chapters, one folder per part
```

## Editing

A chapter's body sits between `<!-- content:start -->` and `<!-- content:end -->`; edit it in place. Everything around it (head, breadcrumb, title, review stamp, the "On this page" sidebar built from the body's `<section id><h2>` pairs, previous/next links, footer) and `index.html` are generated. Run from `docs/method/`:

```bash
python3 tools/build.py            # refresh every chapter's chrome and the contents page
python3 tools/build.py --check    # report pages whose chrome is out of date; writes nothing
python3 tools/build.py --insert 3.1 fragment.html   # put a new fragment in as chapter 3.1's body
python3 tools/leak_check.py       # look for local-only folder names, absolute paths and out-of-scope terms
```

Run the leak check before every commit. It reads the local-only folder names from this clone's git exclude file at run time, so the list itself is never committed. It cannot see figures derived from licensed survey waves, so a reviewer checks those.

To add or reorder chapters, edit `site.json`, then run the build. A chapter listed there with no page yet shows as "planned" on the contents page.

**Conventions:**

- Each body opens with a `<!--meta {json} -->` comment: `id`, `title`, `sub`, `kicker`, `review`, `snapshot` (the date the page describes) and `sources`. Change `review` when the author has reviewed a chapter.
- Code is cited by repository path and function or Stan block, not by pinned line numbers, except where a line is the point. Unmerged work goes in a purple `.branch` box that names the branch.
- Recorded numbers carry their source; a number measured for the page says how it was measured and when.
- Boxes: `.picture` (the plain picture at the top), `.def` / `.eg` / `.algo`, `.measured`, `.decision`, `.branch`, `.open`, `.review`, `.xref`.
- When a model changes, update the chapters it touches in the same change and bump their `snapshot`.
