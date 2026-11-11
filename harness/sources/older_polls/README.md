# Older polls

`harness/older_polls.csv` holds national polls of July 2012 to December 2015 that the harness snapshot lacks, for the 2016 cells, whose window opens at the 2013 election. The snapshot holds no poll from before that election and, before 2016, only Gallup's and MMR's. Nothing reads the table yet: a later stage decides how `info_set()` takes it, so it changes no existing cell. Every source was retrieved on 11 November 2026.

## Commands

From the project root:

```sh
python3 -I harness/sources/older_polls/build.py
```

rewrites `harness/older_polls.csv` from the committed files in this folder and reads nothing else. It stops on any inconsistency: a source row no table holds, a poll dated outside the period, a judgement named for polls other than those it lists, a negative Annað.

```sh
dir=$(mktemp -d)
python3 -I -c 'import csv; [print(r["file"], r["url"]) for r in csv.DictReader(open("harness/sources/older_polls/pins.csv", encoding="utf-8")) if r["file"]]' |
  while read -r file url; do
    curl -sS --fail -A 'kosningaspa-harness/0.1' -o "$dir/$file" "$url"
    sleep 2
  done
python3 -I harness/sources/older_polls/parse_pins.py "$dir"
```

fetches the 16 pinned pages again and re-parses them into the six parsed tables and `manifest.csv`, byte for byte. It checks each page first: a Wikipedia response must hold the pinned revision and its wikitext the pinned sha256, and a Wayback capture must have the pinned sha256. A second argument writes the tables elsewhere instead.

```sh
python3 -I harness/sources/older_polls/build.py --snapshot-sizes
```

rewrites `snapshot_sizes.csv` from `harness/snapshot/polling_data.csv`, after checking it against `harness/snapshot_manifest.csv`. All three scripts use Python's standard library only. `tests/harness/test-older-polls.R` checks the table and runs the first and third commands.

## The table

One row per poll and poll party, ten rows a poll, ordered by date, house, poll and party. 77 polls: 71 ordinary ones and 6 replacements.

- `date`, `fyrirtaeki`, `flokkur`, `n`: as in the snapshot. `n` is the party's share of `n_total`, and 0 for a party the poll did not report.
- `share`: the party's share in percent, blank where not reported. `n_total`: the poll's size. `n_source`: `published` when a pinned source prints it, `assumed` when the size rule gives it, `snapshot` when a replacement poll takes the snapshot's count (J37).
- `fieldwork_start`, `fieldwork_end`: as printed. For an MMR poll known only from its chart, `fieldwork_end` is the chart label and the start is blank. Both are blank for the two polls whose fieldwork is estimated (J14).
- `published`: the first print day found on timarit.is, or the Icelandic 2013 table's "Birt" (published) date (J35).
- `poll_id`: the poll's key in `polls.csv`, a house code and the date. `pins`: the source rows that hold the poll, as `<source>:<row>` (below). `flags`: what a reader should know about the poll, how its size was found, and the judgements it rests on.
- `replaces`: blank for an ordinary poll. A replacement poll names the snapshot key (an MMR date) it stands in for. The owner ruled on 11 November 2026 that the 2016 cells take the replacements, so a cell that reads this table drops the snapshot rows of those keys.

`R/harness/older_polls.R` reads the table strictly.

## Sources and pins

`pins.csv` lists every source with its kind, identifier, URL, retrieval time, bytes and sha256; `check_sha256` is what `parse_pins.py` checks (the wikitext's sha256 for a Wikipedia response) and `file` the name it reads.

| Source | Pin | Parsed table | Rows | `pins` prefix |
|---|---|---|---|---|
| English Wikipedia, 2013 Icelandic parliamentary election, "Opinion polls" | [oldid 1380684716](https://en.wikipedia.org/w/index.php?oldid=1380684716) | `wikipedia_en_2013.csv` | 56 | `en2013` (row) |
| Icelandic Wikipedia, Alþingiskosningar 2013, "Kannanir", a superseded revision with fieldwork and sample sizes | [oldid 1889408](https://is.wikipedia.org/w/index.php?oldid=1889408) | `wikipedia_is_2013.csv` | 46 | `is2013` (row) |
| English Wikipedia, 2016 Icelandic parliamentary election, "Opinion polls" | [oldid 1380684635](https://en.wikipedia.org/w/index.php?oldid=1380684635) | `wikipedia_en_2016.csv` | 97 | `en2016` (row) |
| MMR's chart of its series, "Fylgi flokka og ríkisstjórnar" | [Wayback capture 20151012222031](https://web.archive.org/web/20151012222031id_/http://www.mmr.is:80/fylgi-flokka-og-rikisstjornar) | `mmr_chart.csv` | 96 | `chart` (label) |
| mbl.is's 2013 poll box | [Wayback capture 20130430075308](https://web.archive.org/web/20130430075308id_/http://www.mbl.is:80/frettir/kosning/2013/04/26/sjalfstaedisflokkur_med_mest_fylgi/) | `mbl_box.csv` | 55 | `mbl` (konnun_id) |
| MMR's releases, items 292 to 316, 355, 408 and 485: fieldwork and Svarfjöldi | Wayback captures, one per item | `mmr_items.csv` | 11 | `mmr_item` (item) |
| Newspaper pages on timarit.is | 126 pages | `print_reports.csv` | 608 | `print` (report) |

- **Parsed tables.** The raw pages are not committed: the Wikipedia tables are CC BY-SA 4.0 and are cited by revision, the others by capture. A parsed table keeps each row's numbers as printed, with list columns under the codes D, B, S, V, P (Píratar, Þ on the 2013 pages), C (Viðreisn), A (Björt framtíð), T (Dögun), G (Hægri grænir), L (Lýðræðisvaktin), SAM (Samstaða, C on the 2013 pages), I, RB, R, H, LB, O, HR, FF, FRAM, LH and `other`. The Wikipedia tables add the house as printed, the row's date and the URLs it cites; the Icelandic one its fieldwork, sample text and the parsed sample, response rate and number answering. `manifest.csv` records each table's rows and sha256.
- **Print reports.** `print_reports.csv` holds the figures read from newspaper pages: poll-party shares, fieldwork, sample, response rate, number answering and the first print day, one row each, with the paper, issue date, page number and timarit page id. It holds no newspaper text. It was entered by hand from the pages on 11 November 2026, and every figure was confirmed by a second, independent reading. A `qualifier` marks a share printed in words or as "just under", "about" and the like, or read from a chart, and a method figure printed as approximate; the build uses a qualified share only where no exact figure exists, and never a qualified method figure. To check a row, open `https://timarit.is/page/<page_id>`. `pins.csv` lists the 126 pages with the sha256 of the copy read; `curl -sS --fail -A 'kosningaspa-harness/0.1' https://timarit.is/page/<page_id>` fetches one again, though its markup may have changed since.
- **The register.** `polls.csv` lists every poll the sources hold in the period for the four houses, and the earlier polls the size rule uses, with the source rows each rests on and the judgements it names. `status` is `added` (in the table), `replacement` (in the table, with `snapshot_key`), `in_snapshot` (the snapshot holds it, at `snapshot_key`, matched `strict`, `loose` or `misdated`), `pool` (a poll before the period, or a printed size with no shares, whose size or response rate enters the size rule) or `excluded`. The rows were joined on 11 November 2026: a source row joined a poll of the same house only when the dates fit (fieldwork ends within 2 days, or a release from 1 day before to 16 days after the end) and the shares agreed (a mean difference of 0.35 points or less and at most 1.3 over three or more parties, or all but one party within 0.11).
- **Snapshot sizes.** `snapshot_sizes.csv` is generated from the snapshot: each national poll of the four houses dated before 2016-10-29, with its size. The size rule (J08) and the replacement polls' sizes (J37) read it; the build reads no other snapshot file.

## Conventions

The table follows the snapshot's own conventions, found from its rows and `R/data.R`.

- **Date** (J01): the fieldwork midpoint, start + round((end − start) / 2), rounding half to even as R's `+.Date` does. An MMR poll known only by its chart label is dated at the label plus the floor of the median offset of the snapshot's MMR dates from the labels of the strictly matched polls within 400 days, −1 to −3 days (J26).
- **Size** (J02): the number who answered. Gallup and Félagsvísindastofnun: sample × response rate (Félagsvísindastofnun's number answering where no sample is printed). MMR: its Svarfjöldi, from its release, else print, else a print report of the same survey wave. Fréttablaðið: the number reached. No poll of the period carries Fréttablaðið's "800 called" figure (J31).
- **Assumed sizes** (J08): MMR, the median of the house's published sizes within 730 days, one per distinct poll: the snapshot's polls at their own dates, the published sizes of the polls here and those of the pool polls. Gallup, where the sample drawn is printed but not the response rate, that sample × the median printed response rate of the house's polls within 730 days, else the median size. Félagsvísindastofnun follows the Gallup rule (J38). Left out of the pool: the two half-sample sets under the snapshot's MMR key 2013-05-16 (J28) and four print reports that repeat a survey wave already counted (J27).
- **Replacement sizes** (J34, J37): a replacement poll takes the count the snapshot holds for the row it replaces (`n_source` `snapshot`), since what was wrong in those rows is the date or the shares, not the count of respondents; under the key 2013-05-16, which holds two polls each at half its sample, a poll takes twice its own half. Where a pinned source prints MMR's own count, as item 408 does for 2014-06-20, that count wins.
- **Annað and parties not reported** (J03, J04): Annað is 100 minus the poll parties given, so it holds Björt framtíð and every list outside the ten. Píratar count inside Annað until a house first reports them: Gallup from 2012-12-13, MMR from 2013-02-03, Fréttablaðið from 2013-01-16, Félagsvísindastofnun from 2013-02-25. A party a poll did not report has `n` 0; where a poll party that existed is missing, Annað is left unreported too. One poll is so: Fréttablaðið 2013-06-26, which gives D, B, S and V only.
- **Shares** (J30): an MMR poll takes its chart's unrounded shares; another house's poll the median of the exact figures across its sources, of two the higher-priority source's (en2013, is2013, en2016, mbl.is box, print).
- **Houses** (J07, J29): the sources' names map to the data's. Every Gallup poll rests on a Wikipedia revision, the mbl.is box or a newspaper page, never on Gallup's own series.

## What the table adds

| House | 2012 | 2013 | 2014 | 2015 | All |
|---|---|---|---|---|---|
| Gallup | 6 | 7 | 0 | 0 | 13 |
| MMR | 6 | 17 | 4 | 7 | 34 |
| Fréttablaðið, Stöð 2 og Vísir | 0 | 9 | 4 | 4 | 17 |
| Félagsvísindastofnun | 0 | 7 | 0 | 0 | 7 |
| All | 12 | 40 | 8 | 11 | 71 |

- 40 of the 71 are from before the 2013 election (2012-07-12 to 2013-04-24): Gallup 13, MMR 15, Fréttablaðið 7, Félagsvísindastofnun 5.
- 29 have an assumed size: MMR 23, Gallup 5 (the three readings of April 2013, and November and December 2012 by the printed sample), Félagsvísindastofnun 1 (October 2013).
- Every Gallup row in the sources after the 2013 election is already in the snapshot, so Gallup adds nothing after April 2013.

The six replacement polls, all MMR's, from its chart (J15 to J17):

| Poll | Replaces | What the snapshot row holds |
|---|---|---|
| M-2013-07-10 | 2013-01-09 | this poll, MMR's of 11 July 2013, dated six months early |
| M-2013-05-16 | 2013-05-16 | the poll of 17 May 2013, at half its sample, beside the next |
| M-2013-06-17 | 2013-05-16 | the poll of 18 June 2013, at half its sample, a month early |
| M-2014-03-30 | 2014-03-30 | the named parties 1.6% too large |
| M-2014-05-08 | 2014-05-08 | the named parties 2.6% too large |
| M-2014-06-20 | 2014-06-20 | the named parties 2.0% too large (MMR's release item 408 agrees with the chart) |

Five take the snapshot's own count for the row they replace (J37); M-2014-06-20 takes MMR's published 943.

## Judgements

`judgements.csv` holds each judgement once, with its reason, who made it and the polls it applies to; `polls.csv` names the poll judgements, and the build checks that the two agree. `decided_by` is `owner` for Brynjólfur's rulings, `review` for the judgements of the reviews that planned this table and checked it, `reconciliation` for those of the pass that joined the sources, and `build` for those made in building it, all on 11 November 2026. Beside the conventions above:

- **Relabelled.** Wikipedia's 2016 row 74, "MMR 2015-03-21", is Fréttablaðið's poll of 18 and 19 March 2015, entered with that row's shares, flagged: MMR's chart has no such poll, Fréttablaðið prints the one share P 29.1, and its printed seat split is D'Hondt on the row's shares (J09). Wikipedia's 2016 row 90, "MMR 2014-05-02", is Gallup's April 2014 reading, which the snapshot holds (J10).
- **Merged.** Fréttablaðið's 1,000 reached on 22 and 23 April 2013 are part of its 2,000 of 22 to 24 April: only the larger poll is entered (J11).
- **Entered, flagged.** Three Fréttablaðið polls with one share printed only rounded or in words: 2014-02-26 (S), 2014-04-14 (B), 2015-11-10 (P) (J12). Two MMR polls of late 2015 that only one Wikipedia row holds, citing an aggregator, with fieldwork estimated to end a day before the row's date and to last 7 days (J13, J14).
- **Read.** The English 2013 table's MMR row printed 2012-01-20 is 2013-01-20 (J22). The Icelandic 2013 table's years follow the row order (J23). MMR's chart label 29.1.2014 is 2015-01-29 (J24). The Icelandic table's Gallup response rate of 6% for November and December 2012 is a misprint (J25). Wikipedia's 2016 row 88 repeats V's figure for B (J36).
- **Pool.** The 879 respondents printed on 4 December 2012 count once in the pool, and MMR's chart poll of 13 November 2012, to which they may belong, keeps an assumed size (J32). A pool poll is dated by its printed fieldwork, else its chart label, else its first print or release day (J33).

## Left out

- **Within the period** (`polls.csv`, status `excluded`): Fréttablaðið 2013-04-22 (merged, J11); Fréttablaðið 2014-08-29, whose shares are only rounded or worded in print (J20); Fréttablaðið 2014-10-21, which prints only D, B and S (J21); Plúsinn's two online polls for a radio programme (J18); Kjarninn's poll average of March 2015 (J19).
- **Houses outside the four.** Miðlun's one poll (June 2010); Maskína, whose first party poll was in April 2016.
- **Polls from 2016-01-01**, which the snapshot holds for every cell.
- **Polls before July 2012.** What the sources would support from May 2009 to June 2012 (polls; with four or more exact shares; with a published size):

| House | 2009 (May–Dec) | 2010 | 2011 | 2012 (Jan–Jun) |
|---|---|---|---|---|
| Gallup | 8; 4; 3 | 9; 5; 2 | 7; 2; 0 | 6; 6; 6 |
| MMR | 3; 3; 2 | 12; 12; 8 | 12; 12; 9 | 6; 6; 3 |
| Fréttablaðið, Stöð 2 og Vísir | 2; 2; 0 | 3; 3; 1 | 5; 5; 0 | 3; 3; 1 |

MMR's chart gives every MMR poll. Gallup's readings of 2009 to 2011 come only from print, often rounded, and the sources hold 24 of the 32 monthly readings. Fréttablaðið's polls before May 2012 print only that 800 were called, and would need an assumed number answering. A 2013 test election would also need its own anchors, fundamentals and economy rows.

## What the sources show about the snapshot

Beyond the five rows the replacements address: 16 snapshot polls dated May 2013 to December 2015 are in no source held (Gallup: 7 of 2013 and 8 of 2014; MMR 2015-12-04), and MMR 2014-09-22 has an `n` of 1,436 where every other MMR poll of 2013 to 2015 has 900 to 1,100, which no source confirms. The table changes none of them.

The 2016 polls the snapshot lacks, which the table does not add; each is in the English 2016 table:

| House | Fieldwork | First print | Size | Sources |
|---|---|---|---|---|
| Fréttablaðið | 26–27 Jan | 28 Jan | 801 | en2016 row 51; print |
| Fréttablaðið | 8–9 Mar | 11 Mar | 794 | en2016 row 46; print |
| MMR | not printed (released 18 Mar) | | | en2016 row 45 |
| Félagsvísindastofnun | not printed | | | en2016 row 43 (citing an aggregator) |
| Fréttablaðið | 4–5 Apr | 6 Apr | 800 | en2016 row 42; print |
| Félagsvísindastofnun | 7–8 Apr | 9 Apr | 826 | en2016 row 38; print |
| Maskína | 7–8 Apr | 9 Apr | 2,438 respondents | en2016 row 39; print; Maskína's post |
| MMR | not printed (released 3 May) | | | en2016 row 35 (citing an aggregator) |
| Fréttablaðið | 2–3 May | 6 May | 797 | en2016 row 34; print |
| Fréttablaðið | 9 May | 12 May | 799 | en2016 row 33; print |
| MMR | not printed (released 13 May) | | | en2016 row 32 (citing an aggregator) |
| Félagsvísindastofnun | not printed | 17 May | 2,003 × 47% | en2016 row 31; print |
| Fréttablaðið | 23–24 May | 27 May | 800 | en2016 row 30; print |
| Félagsvísindastofnun | 1–2 Jun | 4 Jun | 920 | en2016 row 28; print |
| Félagsvísindastofnun | 8–12 Jun | 14 Jun | 1,080 | en2016 row 27; print |
| Félagsvísindastofnun | 19–22 Jun | 24 Jun | 1,470 | en2016 row 26; print |
