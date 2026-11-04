"""Fold new 2026 poll files into the working poll tables before a refit.

    python3 -I usa/merge_new_polls.py <tables dir> <new_polls dir>

New files are <yyyymmdd-hhmm>_senate.csv, _house.csv or _generic.csv in the columns of
senate_polls_2026.csv or house_district_polls_2026.csv. They are appended to
senate_polls_2026_v2.csv and house_district_polls_2026_v2.csv (each created
from its original on first use; the originals, which the forecast recorded on
2 November read, are never changed). A row is kept only if it ended by
2026-11-02 and is not already there: Senate rows are matched on state, race,
pollster and end date, House rows on state, district, pollster and end date.
merged_files.txt in the tables dir lists the files already folded in, so a
file is read once. Rows listed in excluded_polls.csv (kind plus the key columns)
are skipped like duplicates: the owner's rule of 3 November keeps out pollsters
with no track record, whose house effect would be all prior. Prints what was
added and skipped.
"""

import csv
import pathlib
import shutil
import sys

LAST_END = "2026-11-02"
KINDS = {
    "senate": ("senate_polls_2026.csv", "senate_polls_2026_v2.csv", ("state", "race", "pollster", "end_date")),
    "house": ("house_district_polls_2026.csv", "house_district_polls_2026_v2.csv", ("state", "district", "pollster", "end_date")),
    "generic": ("generic_ballot_2026.csv", "generic_ballot_2026_v2.csv", ("pollster", "end_date", "population")),
}


def main(tables: pathlib.Path, drops: pathlib.Path) -> None:
    log = tables / "merged_files.txt"
    done = set(log.read_text().split()) if log.exists() else set()
    for path in sorted(drops.glob("*.csv")):
        if path.name in done:
            continue
        kind = path.stem.rsplit("_", 1)[-1]
        if kind not in KINDS:
            print(f"skip {path.name}: name does not end in _senate, _house or _generic")
            continue
        orig, work, key = KINDS[kind]
        work_path = tables / work
        if not work_path.exists():
            shutil.copyfile(tables / orig, work_path)
        with work_path.open(encoding="utf-8") as f:
            have = list(csv.DictReader(f))
        fields = list(have[0].keys())
        with path.open(encoding="utf-8") as f:
            reader = csv.DictReader(f)
            if list(reader.fieldnames or []) != fields:
                print(f"skip {path.name}: columns differ from {orig}")
                continue
            new = list(reader)
        seen = {tuple(r[k].strip().lower() for k in key) for r in have}
        excl_path = tables / "excluded_polls.csv"
        if excl_path.exists():
            with excl_path.open(encoding="utf-8") as f:
                seen |= {tuple(r[k].strip().lower() for k in key) for r in csv.DictReader(f) if r.get("kind") == kind}
        added, late, dup = [], 0, 0
        for r in new:
            k = tuple(r[c].strip().lower() for c in key)
            if r["end_date"] > LAST_END:
                late += 1
            elif k in seen:
                dup += 1
            else:
                seen.add(k)
                added.append(r)
        with work_path.open("a", newline="", encoding="utf-8") as f:
            csv.DictWriter(f, fieldnames=fields).writerows(added)
        with log.open("a") as f:
            f.write(path.name + "\n")
        print(f"{path.name}: {len(added)} added to {work}, {dup} duplicates, {late} ending after {LAST_END}")


if __name__ == "__main__":
    main(pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2]))
