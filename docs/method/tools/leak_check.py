#!/usr/bin/env python3
"""Check the methodology notes for text the public repository must not carry.

Run from docs/method/ (or anywhere inside the repository):

    python3 tools/leak_check.py            # every chapter, analysis script and README under docs/method/
    python3 tools/leak_check.py FILE ...   # only these files

It reports each hit as file:line and exits 1 if there is any. It looks for:

- names of local-only folders, read at run time from this clone's git exclude file
  (`git rev-parse --git-common-dir`, then info/exclude), so the list itself is never committed.
  A name the tracked .gitignore already covers is public and skipped. A name that is a plain
  word is matched only as a path component (`name/` not preceded by a path character, or
  preceded by the main checkout's own folder name), since plain words collide with ordinary
  text; a name with `/`, `.`, `_` or `-` is matched as written;
- absolute and home-relative paths;
- a few terms for a topic the notes leave out entirely.

It cannot see figures derived from licensed survey waves; the leak review covers those.
"""
import re
import subprocess
import sys
from pathlib import Path

FIXED = [
    (re.compile(r"/Users/"), "absolute path"),
    (re.compile(r"(?<![\w.])~/"), "home-relative path"),
    (re.compile(r"\$HOME\b"), "home-relative path"),
    (re.compile(r"(?<![\w./-])usa/"), "out-of-scope topic"),
    (re.compile(r"midterm", re.I), "out-of-scope topic"),
    (re.compile(r"\bSenate\b"), "out-of-scope topic"),
]
SUFFIXES = {".html", ".md", ".R", ".r", ".py", ".json", ".txt", ".csv", ".qmd"}


def git(*args, cwd):
    return subprocess.run(["git", *args], cwd=cwd, capture_output=True, text=True, check=True).stdout.strip()


def norm(entry):
    return entry.strip().strip("/").replace("*", "").strip("/")


def local_only_patterns(root):
    common = Path(git("rev-parse", "--git-common-dir", cwd=root))
    if not common.is_absolute():
        common = (root / common).resolve()
    exclude = common / "info" / "exclude"
    tracked = root / ".gitignore"
    public = set()
    if tracked.exists():
        public = {norm(l) for l in tracked.read_text(encoding="utf-8").splitlines() if l.strip() and not l.startswith("#")}
    repo = re.escape(common.parent.name + "/")
    pats = []
    if not exclude.exists():
        return pats
    for i, line in enumerate(exclude.read_text(encoding="utf-8").splitlines()):
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        name = norm(line)
        if not name or any(name == p or name.startswith(p + "/") for p in public if p):
            continue
        if re.search(r"[/._-]", name):
            rx = re.compile(re.escape(name))
        else:
            rx = re.compile(r"(?:(?<![\w./-])|(?<=" + repo + r"))" + re.escape(name) + r"/")
        pats.append((rx, f"local-only folder (exclude line {i + 1})"))
    return pats


def targets(root, args):
    if args:
        return [Path(a).resolve() for a in args]
    base = root / "docs" / "method"
    me = Path(__file__).resolve()
    return sorted(p for p in base.rglob("*") if p.is_file() and p.suffix in SUFFIXES and p.resolve() != me)


def main(argv):
    root = Path(git("rev-parse", "--show-toplevel", cwd=Path.cwd()))
    pats = FIXED + local_only_patterns(root)
    hits = 0
    for path in targets(root, argv[1:]):
        text = path.read_text(encoding="utf-8", errors="replace")
        for n, line in enumerate(text.splitlines(), 1):
            for rx, label in pats:
                m = rx.search(line)
                if m:
                    hits += 1
                    rel = path.relative_to(root) if path.is_relative_to(root) else path
                    print(f"{rel}:{n}: {label}: {m.group(0)!r}")
    n_local = len(pats) - len(FIXED)
    print(f"leak_check: {hits} hit(s); {len(FIXED)} fixed patterns, {n_local} local-only names", file=sys.stderr)
    return 1 if hits else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
