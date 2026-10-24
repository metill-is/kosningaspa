# v2 backtest harness

The harness builds frozen, leak-checked inputs for the v2 forecast backtests: official
results, poll availability, fundamentals target rows and seat layouts, for the forecast
cells of the 2021 and 2024 elections and the held-out 2017 confirmatory cells. Code lives
in `R/harness/` (box modules), tests in `tests/harness/`.

Spec: the note "2026-10-23 — v2 forecast model and two-election harness (design)" in the
Metill vault (Kosningar/Forecast model).

## The snapshot

The harness never reads the live `data/`. `snapshot_inputs(src_root)` copies the files in
`SNAPSHOT_FILES` (`R/harness/inputs.R`) from the main checkout into `harness/snapshot/`
(gitignored) and writes `harness/snapshot_manifest.csv` (name, file, source, bytes, sha256;
committed). Every read goes through `snapshot_file(name)`, which checks that file's sha256
against the manifest; `verify_snapshot()` checks them all. `snapshot_inputs()` refuses an
existing `harness/snapshot/`: to re-snapshot, delete it by hand, re-run, and commit the new
manifest.

## Tests

From the project root: `LC_ALL=en_US.UTF-8 Rscript tests/run.R [filter]`, which runs the
suite under testthat edition 3. Tests that need the snapshot call `skip_without_snapshot()`
and skip without it. `tests/harness/test-generated.R` fails if a generated committed file
that builds offline is not a fresh build.

The R code in `R/harness/` and `R/election_utils.R` writes every non-ASCII character as a
`\uXXXX` escape and gives a non-ASCII name with `stats::setNames()`, never as a name tag;
comments keep their Icelandic. R parses a module in the locale active when it is loaded.
Under C it translates every name tag to a native symbol, escaped or not, so
`c("Framsókn" = …)` became `Frams<U+00F3>kn` and Framsóknarflokkurinn silently dropped
out of the fundamentals' party set. A literal survives such a load only because box tells
the parser the file is UTF-8; `source()` or a bare `parse()` reads it natively.
`tests/harness/test-escapes.R` enforces both rules and loads the harness under `LC_ALL=C`
in a subprocess to check that it still reads the snapshot right. `read_inputs()` and
`build_phase0.R` still stop under a locale that is not UTF-8, but that check cannot stand
in for the escapes: it runs when they are called, after the modules were parsed, so a
session that loads the harness under C and then switches to UTF-8 passes it.

## Rebuilding the generated files

From the project root: `LC_ALL=en_US.UTF-8 Rscript R/harness/build_phase0.R`. It verifies
the snapshot, then rewrites, each through its own writer,
`harness/sources/seats_2021_official.csv` (from the committed Alþingi page),
`harness/truth/elections_official.csv`, `harness/availability.csv`,
`harness/availability_report.md` and `harness/entry_table.csv`.
It prints the polls per availability rule and each forecast cell's admitted polls
(national and kjördæmi). The output is deterministic, so on unchanged inputs
`git status --porcelain harness/` stays empty. It does not rebuild the Wikipedia fieldwork
tables, whose parser needs page revisions fetched from the web (`harness/sources/README.md`),
or `snapshot_manifest.csv`.

## Committed versus generated

- Committed: this README, `snapshot_manifest.csv`, the generated tables that
  `build_phase0.R` writes, and `harness/sources/`, which holds source files fetched once or
  entered by hand, with their provenance in a README: `harness/truth/README.md` for the
  results sources, `harness/sources/README.md` for the poll-fieldwork tables and the house
  map, the fieldwork corrections, the publication dates, the two correction tables
  (`poll_n_corrections.csv`, `fundamentals_corrections.csv`), the economy release dates,
  the house table and the coalition events.
- Generated and gitignored: `harness/snapshot/`.

## Target rows: the economy and the carried columns

`fundamentals_inputs()` (`R/harness/fundamentals.R`) builds E's target rows. Their `vnv`
and `growth` are the snapshot's `economy_data.csv` row at E, as `R/prepare_economy_data.R`
writes it: `vnv` is the 12-month change of the CPI (Hagstofa VIS01002) at July of E − 1,
and `growth` is the volume change of GDP per capita on the previous year (THJ01000, "Verg
landsframleiðsla á mann, magnbreyting frá fyrra ári") for E − 1. `info_set()` admits only
an F on or after the day Hagstofa first published both figures, from
`harness/sources/economy_release.csv` (sources and the checks: `harness/sources/README.md`).
The data was copied into the snapshot on 23 Oct 2026, so the values are that vintage, not
the first releases: growth for 2016 was first published as 5.5 and is 4.7 in the
snapshot, and growth for 2020 as −8.2 against −7.8. That is the revision leak spec §5.3
rule 5 accepts: the release dates bound when a figure was first public, not which value
it had then.

The carried columns, `year`, `new_party`, `party_split`, `voteshare_prior` and
`incumbent_years`, come from the party's row at E: known before E, not necessarily at F.
Framsókn's `party_split` = 1 in 2017 records the split of September 2017 (Miðflokkurinn
was founded on 24 September; English Wikipedia, "Centre Party (Iceland)", retrieved
24 Oct 2026). Both 2017 cells' F postdate it, but an F before it would see a split not
yet made. It is latent: the spec's stage-1 regression (§6.5) and the production model do
not use `party_split`.

## Notes for the freeze review

Brynjólfur decides on these before Phase 3. The Brier event of spec §5.6 is a majority for
the coalition in office at F: `coalition_event()` in `R/harness/tables.R`, from
`harness/sources/coalition_events.csv`.

- **2017-10-14 and 2017-10-27: the event is D + C, with A in Annað.** Björt framtíð left
  the coalition on 15 September 2017. The Prime Minister resigned on 16 September, and the
  cabinet sat on as a caretaker government, with Björt framtíð's ministers in office until
  30 November. So the composition in office at both dates is D + C + A, and the literal
  §5.6 event, with A in Annað, is D + C. The incumbents (`incumbents_at()`) are also
  D + C.
- **2024-11-16 and 2024-11-29: the event is D + B.** Vinstri græn left the government on
  17 October 2024, and Sjálfstæðisflokkurinn and Framsóknarflokkurinn sat as a caretaker
  government until 21 December. So the literal §5.6 rule gives D + B at both dates, while
  D + B + V, the cabinet's opening composition, still counts as the incumbents
  (`incumbents_at()`, as `fundamentals_data.csv` codes them).
- **2024-10-14: the event is D + B + V.** F is one day after Bjarni Benediktsson announced
  the end of the coalition on 13 October 2024. The government had not yet resigned (the
  President accepted the resignation on 15 October), and Vinstri græn's ministers held
  office until 17 October. So the composition in office, and the literal §5.6 event, is
  D + B + V. Spec §5.2 asks whether the 30 November election date had been announced by
  F. It had been announced, not yet fixed. At his press conference of 13 October the
  Prime Minister said elections would be held on 30 November, pending the President's
  consent to dissolve the Alþingi ([Reykjavík Grapevine, 13 Oct
  2024](https://grapevine.is/?p=10079846)). The President's statement of 14 October
  records his proposal of a dissolution and elections "í lok nóvember" and puts off her
  decision to later in the week ([Wayback capture](https://web.archive.org/web/20250805091546/https://www.forseti.is/media/14866/20241014-yfirl%C3%BDsing-forseta-vegna-tilloegu-um-%C3%BEingrof.pdf)).
  Her statement of 15 October accepts it: the dissolution would be announced at the
  Alþingi's sitting of 17 October, and the election held on 30 November ([Wayback
  capture](https://web.archive.org/web/20250802184631/https://www.forseti.is/media/14843/yfirl%C3%BDsing-forseta-vegna-%C3%BEingrofs-og-lausnarbei%C3%B0nar.pdf)).
  The two statements' forseti.is addresses returned 404 on 24 Oct 2026, when these were
  read, and althingi.is refuses scripted requests.
- **`fundamentals_data.csv` codes incumbency by three conventions.** `incumbent` and
  `incumbent_prime_minister` follow the cabinet formed after the previous election (the
  rule `incumbent` follows from 2003 on, `harness/sources/README.md`; before 2003, 1991 is
  an exception), the cabinet in office on election day, or, in 1983, the parties rather
  than the prime minister. The first two readings differ in four years, and 1983 follows
  neither:
  - The cabinet formed after the previous election, in 1979 and 2009. In 1979 the
    incumbents are Framsókn, Alþýðuflokkur and Alþýðubandalag and the prime minister is
    Framsókn's: Ólafur Jóhannesson's cabinet, appointed on 1 September 1978. Benedikt
    Gröndal (Alþýðuflokkur) was prime minister on election day; his term began on
    15 October 1979. In 2009 the prime minister is D's, of the coalition formed after the
    2007 election, although S held the premiership on election day.
  - The cabinet in office on election day, in 1991 for both columns and in 2024 for the
    prime minister only. In 1991 the incumbents are Framsókn, Alþýðuflokkur,
    Alþýðubandalag and Borgaraflokkur and the prime minister is Framsókn's: Steingrímur
    Hermannsson, prime minister from 28 September 1988. Þorsteinn Pálsson (D) headed the
    coalition formed after the 1987 election. In 2024 the prime minister is D's, as from
    9 April 2024, not V's, whose leader headed the coalition formed after the 2021
    election; the incumbents (B, D and V) are that coalition's.
  - The parties, not the prime minister, in 1983. Gunnar Thoroddsen, a D member, was
    prime minister from 8 February 1980 to 26 May 1983, so both readings give his
    cabinet. He formed it with Framsókn and Alþýðubandalag against his party's will
    (English Wikipedia, "Gunnar Thoroddsen", retrieved 24 Oct 2026). The data codes
    Framsókn and Alþýðubandalag as incumbents, D as not incumbent, and no party as the
    prime minister's, which neither reading gives.

  In every other year from 1963 to 2021 both readings give the same prime minister's
  party: D in 1963, 1967 and 1971, B in 1974, D in 1978, B in 1987, D from 1995 to 2007,
  S in 2013, B in 2016, D in 2017 and V in 2021. So 1979, 1991 and 2009 are the training
  rows on which the first two readings differ, and 1983 is the one that follows neither;
  2024 is a target row only. The harness codes the target row's prime minister as at F
  (`pm_at()`), which in every cell equals the data's coding at E.
  Prime ministers' terms: Icelandic Wikipedia, "Listi yfir forsætisráðherra Íslands"
  (retrieved 24 Oct 2026).
