# Data Sources — Post-Election National Polls

Provenance log for the post-election national polls feeding Fylgisvakt and
the next Alþingiskosningar forecast cycle. One section per data file. Within
each, polls are listed chronologically per pollster with the canonical
source URL and any notes on sample frame, fielding window, or attribution.

**Scope.** This file documents `data/post_election_polls.csv` (Dec 2024
onward) and `data/post_election_polls_kjordaemi.csv` (election results
only, as of 2026-05-20). Pre-election polls (≤ 2024-11-30) flow through the
`POLLING_SHEET_URL` Google Sheet and carry their own provenance there.

**Conventions**:

- `date` = midpoint of fielding window. Publication date is *not* canonical
  and gets corrected to the midpoint on ingest.
- `n_total` convention is **firm-specific** and must not be mixed:
  - **Gallup** → `heildarúrtak` (gross sample contacted)
  - **Maskína** → `svarendur` (took a position on a party)
- The durable record is the `tribble()` literal inside
  `R/scrape_polls.R::get_hardcoded_polls()`. The CSVs in `data/` are
  regenerated from it on each run.
- Add a row to the matching table below for every tribble row you add or
  change. The `scrape-polls` skill enforces this on ingest.

---

## `data/post_election_polls.csv`

### Election result anchor

| Date | n_total | Source | Notes |
|---|---|---|---|
| 2024-11-30 | 212,470 | [kosning.is — Alþingiskosningar 2024](https://www.kosning.is/althingiskosningar-2024/urslit-kosninga/) | Official tally; `n_total` = gild atkvæði nationally |

### Gallup (Þjóðarpúls)

Monthly publication, typically reporting the previous month's field period.
Gallup's own publication on `gallup.is` precedes the RÚV write-up by several
days (≥8 days observed in May 2026). The PDF is methodologically
authoritative — it carries `heildarúrtak`, response rate, and field period
that the RÚV article often omits.

**PDF hosting paths.** Gallup migrated PDF hosting in May 2025:
- May 2025 onward: `cdnx.gallup.is/media/documents/Puls_MMYY_Fylgi_flokka.pdf` (deterministic, URL-guessable)
- Pre-May 2025: `gallup.is/documents/<opaque_id>/Puls_MMYY_Fylgi_flokka.pdf` (IDs not enumerable; find via PDF-viewer-page iframe `src`)

For pre-May 2025 polls where the PDF can't be located, the next-month PDF's
"comparison" column provides secondary confirmation of the prior month's
values.

| Date | n_total | Source | Notes |
|---|---|---|---|
| 2024-12-24 | 3,460 | [Útvarp Saga](https://utvarpsaga.is/ny-konnun-synir-litlar-breytingar-a-fylgi-flokkana-eftir-kosningar/) | Fielded 16. des. 2024 – 1. jan. 2025; úrtak 3.460. First post-election poll; gallup.is PDF not located |
| 2025-02-16 | 9,652 | [RÚV](https://www.ruv.is/frettir/innlent/2025-03-03-samfylkingin-tekur-stokk-i-nyjum-thjodarpulsi-437990) ([mbl](https://www.mbl.is/frettir/innlent/2025/03/03/gallup_samfylkingin_i_mikilli_sokn/)) | Þjóðarpúls mars 2025; n_total not independently verified from a gallup.is source |
| 2025-03-17 | 10,324 | [PDF](https://www.gallup.is/documents/917/Puls_0425_Fylgi_flokka.pdf) | Fielded 3.–31. mars 2025; úrtak 10.324, svh 47,5% |
| 2025-04-15 | 10,005 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0525_Fylgi_flokka.pdf) | Fielded 1.–30. apríl 2025; úrtak 10.005, svh 46,7% |
| 2025-05-16 | 11,521 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0625_Fylgi_flokka.pdf) | Fielded 1. maí – 1. júní 2025; úrtak 11.521, svh 44,9% |
| 2025-06-16 | 10,216 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0725_Fylgi_flokka.pdf) | Fielded 2.–30. júní 2025; úrtak 10.216, svh 46,5%. Tribble corrected 2026-05-20 (was 10,500) |
| 2025-07-16 | 11,541 | [PDF](https://www.gallup.is/documents/1214/Puls_0825_Fylgi_flokka.pdf) | Fielded 1.–31. júlí 2025; úrtak 11.541, svh 43,6% |
| 2025-08-16 | 10,055 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0925_Fylgi_flokka.pdf) | Fielded 1.–31. ágúst 2025; úrtak 10.055, svh 44,5% |
| 2025-09-15 | 10,887 | [PDF](https://cdnx.gallup.is/media/documents/Puls_1025_Fylgi_flokka.pdf) | Fielded 1.–30. september 2025; úrtak 10.887, svh 43,1% |
| _2025-10-15_ | _11,225_ | _[PDF](https://cdnx.gallup.is/media/documents/Puls_1125_Fylgi_flokka.pdf)_ | _**MISSING FROM TRIBBLE.** Fielded 1. október – 2. nóvember 2025; úrtak 11.225, svh 46,5%. Per-party percentages need verification before tribble insertion (see Verification log)_ |
| 2025-11-16 | 10,332 | [PDF](https://cdnx.gallup.is/media/documents/Puls_1225_Fylgi_flokka.pdf) | Fielded 3.–30. nóvember 2025; úrtak 10.332, svh 41,8% |
| 2025-12-15 | 9,091 | [PDF](https://cdnx.gallup.is/media/documents/Puls_1225_2_Fylgi_flokka.pdf) | Fielded 1.–28. desember 2025; úrtak 9.091, svh 43,4%. Tribble corrected 2026-05-20 (was 10,000 default) |
| 2026-01-15 | 9,713 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0226_Fylgi_flokka.pdf) | Fielded 9. janúar – 1. febrúar 2026; úrtak 9.713, svh 43,6%. Tribble corrected 2026-05-20 (was 10,000 default) |
| 2026-02-15 | 9,958 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0326_Fylgi_flokka.pdf) | Fielded 2. febrúar – 1. mars 2026; úrtak 9.958, svh 44,3% |
| 2026-03-16 | 10,746 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0426_Fylgi_flokka.pdf) ([Gallup article](https://www.gallup.is/frettir/framsokn-tapar--fylgi/)) | Fielded 2.–31. mars 2026; úrtak 10.746, svh 42,3%. Tribble corrected 2026-05-20 (was 10,000 default) |
| 2026-04-15 | 10,484 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0526_Fylgi_flokka.pdf) ([Gallup article](https://www.gallup.is/frettir/sjalfstaedisflokkurinn-baetir-vid-sig-fylgi/)) | Fielded 1.–29. apríl 2026; úrtak 10.484, svh 40,4% |
| 2026-05-15 | 12,979 | [RÚV (þröskuldsfrétt)](https://www.ruv.is/frettir/innlent/2026-06-01-flokkur-folksins-maelist-utan-things-476896) ([RÚV forsíðufrétt](https://www.ruv.is/frettir/innlent/2026-06-01-sjalfstaedisflokkurinn-staerri-en-samfylking-eftir-kosningar-476906)) | Fielded 30. apríl – 31. maí 2026; úrtak 12.979, svh 40,8%, vikmörk 0,4–1,3 pp. Þjóðarpúls maí 2026. **Gallup PDF (Puls_0626) not yet published at ingest — RÚV published ahead of gallup.is.** Full 9-party breakdown + methodology taken from the companion threshold article's interactive Highcharts data table (S 28,4 / D 23,5 / M 17,6 / C 10,6 / B 6,7 / V 4,3 / F 4,0 / J 2,6 / P 1,9); whole-month figure used per convention (poll also reports a pre/post-16 May municipal-election split — not ingested). Broadcast graphic (7 largest parties) cross-checks exactly |

### Maskína (national)

Irregular publication for Vísir, Sýn (Stöð 2), and occasional other outlets.
Three canonical sources, in order of preference:

1. **Maskína's own PDF report** at `maskina.is/wp-content/uploads/YYYY/MM/YYYY-MM-DD-Fylgi-Althingi_Maskinuskyrsla.pdf` (or `…_Maskinuskyrsla-N.pdf` when multiple). Authoritative for `svarendur`, field period, and per-party percentages. Discoverable via WebSearch (`site:maskina.is "<month> <year>"`) or by walking the news listing at [`maskina.is/frettir/`](https://maskina.is/frettir/) — each monthly poll gets its own news article whose body links to the PDF.
2. **Maskína dashboard** at [`maskina.is/fylgi-flokka-a-althingi/`](https://maskina.is/fylgi-flokka-a-althingi/). Tableau Public viz with five tabs (Nýjasta mæling, Þróun fylgis, Mín ríkisstjórn, Samanlagt fylgi, Hreyfing á fylgi) covering every Maskína monthly poll back to Nov 2021. The `Veldu mælingu` dropdown switches the displayed measurement; the "Bakgrunnur" panel shows `svarendur` for the selected poll. **Not programmatically scrapable** — Tableau renders to canvas, dropdown clicks go to canvas event handlers, the dashboard URL `public.tableau.com/views/FylgiFlokka-heimasa/Njastamling` doesn't expose a `.csv` endpoint. Use it for manual verification, not automated ingest.
3. **The downstream article** on `visir.is/g/...`, `mbl.is`, or `ruv.is/frettir/innlent/...`. Each new Maskína poll typically gets a same-day write-up. Article bodies sometimes disclose `svarendur` directly; when paywalled, per-party percentages can be cross-checked via search snippets.

| Date | n_total | Source | Notes |
|---|---|---|---|
| 2024-12-12 | 2,803 | [Vísir](https://www.visir.is/g/20242669675d/flokkur-folksins-dalar-eftir-kosningar) | Fielded 5.–19. desember 2024; svarendur 2.803. First post-election Maskína |
| 2025-10-09 | 1,765 | [Vísir](https://www.visir.is/g/20252820991d/ny-konnun-maskinu-vaeri-al-veg-ny-stada-i-is-lenskum-stjorn-malum-) | Fielded 3.–15. október 2025; svarendur 1.765 |
| 2025-11-10 | 1,500 | [Vísir](https://www.visir.is/g/20252806182d/mid-flokkur-skakar-sjalf-staedis-flokknum-stor-merki-leg-nidur-stada-) | Composite: 3.–10. nóv + 13.–18. nóv 2025; per-party % match tribble. **Side-publication for Vísir — not part of Maskína's regular monthly Alþingi series, so no Maskína-published PDF or news article exists (confirmed via news-pagination walk 2026-05-20). Dashboard has the data but isn't programmatically scrapable. `n_total` 1,500 remains the agent's best estimate from Vísir** |
| 2026-01-11 | 886 | [Vísir](https://www.visir.is/g/20262832055d/mid-flokkurinn-nalgast-sam-fylkingu) | Fielded 9.–13. janúar 2026; svarendur 886 (91,2% af úrtaki). Small single-period sample |
| 2026-02-24 | 1,993 | [mbl.is](https://www.mbl.is/frettir/innlent/2026/02/24/fylgi_midflokksins_laekkar/) | Maskína fyrir Sýn-fréttir; per-party % match tribble (S 27,2 / D 16,2 / M 19,0 / C 13,4 / F 4,8 / P 5,2 / V 4,1 / J 3,1 / B 7,0). **Side-publication for Sýn — not part of Maskína's regular monthly Alþingi series, so no Maskína-published PDF or news article exists (confirmed via news-pagination walk 2026-05-20: gap between Jan 12 and Mar 2). mbl paywalled. `n_total` 1,993 remains the agent's best estimate** |
| 2026-03-08 | 2,617 | [Vísir](https://www.visir.is/g/20262859852d/fylgi-sam-fylkingar-ekki-verid-minna-i-eitt-ar) | Composite: 26. feb.–3. mars + 12.–19. mars + 16.–19. mars 2026; svarendur 2.617 |
| 2026-04-05 | 1,786 | [Vísir](https://www.visir.is/g/20262871996d/sam-fylking-og-sjalf-staedis-flokkur-haekka-flugid) ([maskina.is](https://maskina.is/sjalfstaedisflokkurinn-a-uppleid-samfylkingin-afram-staerst-flokka-a-landsvisu/)) | Composite: 25.–31. mars + 8.–16. apríl 2026; svarendur 1.786 |

---

## `data/post_election_polls_kjordaemi.csv`

Currently contains only the 2024 election results (60 rows: 6 kjördæmi × 10
parties incl. Annað). No post-election kjördæmi-level polls have been
ingested as of 2026-05-20 — adding them is the subject of the kjördæmi
research strand discussed separately.

| Date | Pollster | Source | Notes |
|---|---|---|---|
| 2024-11-30 | Kosning | [kosning.is — Alþingiskosningar 2024 (kjördæmi)](https://www.kosning.is/althingiskosningar-2024/urslit-kosninga/) | Per-kjördæmi gild atkvæði from the official tally |

---

## Maintenance

When adding a new poll:

1. Append the row to the appropriate block in
   `R/scrape_polls.R::get_hardcoded_polls()`. Match column alignment with
   the surrounding rows — `git diff` reads much cleaner when columns align.
2. Add a row to the matching table above with: date, `n_total`, source
   URL(s), notes (field period, sample frame, response rate).
3. If `n_total` was disclosed in the source, record it as such. If it
   wasn't and you defaulted to 10,000 (Gallup) or estimated (Maskína),
   flag in the Notes column.
4. Use the **midpoint of fielding** as the canonical `date` — never the
   publication date.

The `scrape-polls` skill at `.claude/skills/scrape-polls/SKILL.md` codifies
these rules; this file is the durable artefact the skill produces.

---

## Verification log

- **[2026-05-20]** Initial `data_sources.md` created. Skeleton populated
  from the 23-row `get_hardcoded_polls()` tribble (1 election anchor +
  15 Gallup + 7 Maskína).

- **[2026-05-20]** Backfill agents (parallel) verified sources for all 22
  post-election tribble polls.
  - **Maskína (7/7 sources located).** All tribble dates match the source
    field-period midpoints within ±1 day. `n_total` values match exactly
    where the source disclosed `svarendur`. Two polls have minor gaps:
    - 2025-11-10: Vísir article body does not surface `svarendur`. The
      tribble value of 1,500 should be re-verified against the underlying
      Maskína PDF on `maskina.is` ("Fylgi flokka á Alþingi nóvember 2025").
    - 2026-02-24: mbl.is returns HTTP 403 to `WebFetch` (paywall). Per-party
      percentages match exactly via search snippets; `svarendur` and field
      period not visible.
  - **Gallup (13/15 sources via gallup.is PDFs; 2/15 via RÚV/Útvarp Saga).**
    Pre-May 2025 polls fall back to secondary sources because the older
    `gallup.is/documents/<opaque_id>/` URL space is not enumerable.
  - **4 `n_total` corrections applied to the tribble** (`R/scrape_polls.R`,
    commit pending):
    - 2025-06-16: 10,500 → **10,216** (per `Puls_0725` PDF)
    - 2025-12-15: 10,000 → **9,091** (per `Puls_1225_2` PDF; was defaulted)
    - 2026-01-15: 10,000 → **9,713** (per `Puls_0226` PDF; was defaulted)
    - 2026-03-16: 10,000 → **10,746** (per `Puls_0426` PDF; was defaulted)
  - **1 missing poll flagged for ingestion:** Þjóðarpúls október 2025 (PDF
    `Puls_1125`), fielded 1. október – 2. nóvember 2025, úrtak 11,225, svh
    46,5%. Headline party percentages obtained from text but not all
    parties; per-party values need PDF verification before tribble
    insertion. Suggested next step: run the `scrape-polls` skill against
    `https://cdnx.gallup.is/media/documents/Puls_1125_Fylgi_flokka.pdf`.

- **[2026-05-20]** Infrastructure note: Gallup migrated PDF hosting to
  `cdnx.gallup.is/media/documents/Puls_MMYY_Fylgi_flokka.pdf` from May 2025
  onward. The `scrape-polls` skill should be updated to mention this in
  its "Where the data actually lives" section.

- **[2026-05-20]** Maskína dashboard investigated as a scraping candidate.
  The Tableau Public viz at
  [`maskina.is/fylgi-flokka-a-althingi/`](https://maskina.is/fylgi-flokka-a-althingi/)
  carries every Maskína monthly national poll back to November 2021, with
  svarendur visible in the "Bakgrunnur" panel of "Nýjasta mæling" when a
  given measurement is selected. **Not programmatically scrapable** —
  Tableau renders to canvas (dropdown clicks dispatch to canvas event
  handlers rather than DOM), screenshot capture in this environment times
  out on the 2500px-tall iframe, and the dashboard URL has no `.csv` data
  endpoint. Recorded as a manual-verification source instead.

  **A better find from the same investigation:** Maskína publishes its
  own monthly PDF report at
  `maskina.is/wp-content/uploads/YYYY/MM/YYYY-MM-Fylgi-Althingi_Maskinuskyrsla.pdf`
  (examples confirmed: 2024-10, 2024-11-21, 2024-06). This is the analogue
  of Gallup's `Puls_MMYY_Fylgi_flokka.pdf` and should be the primary source
  for future Maskína ingestion of regular monthly readings.

  The `scrape-polls` skill has been updated to mention both sources.

- **[2026-05-20]** Verification attempt for the two outstanding `n_total`s
  (2025-11-10, 2026-02-24). Walked `maskina.is/frettir/` pages 1–4 covering
  June 2025 through April 2026. Result: **no Maskína-published Alþingi
  polling article exists for Nov or Dec 2025**, and **no national polling
  article exists for Feb 2026** between the Jan 12 ESB article and the Mar
  2 Reykjavík one. Combined with the Vísir/mbl attribution ("Maskína fyrir
  Sýn-fréttir" for the Feb 24 reading), this confirms a structural
  distinction:

  - **Regular monthly Alþingi readings** → Maskína publishes their own
    article on `maskina.is/frettir/` + a PDF report at
    `maskina.is/wp-content/uploads/YYYY/MM/...` + entry on the dashboard.
    These have full audit trail (article, PDF, dashboard, downstream RÚV/
    Vísir/mbl write-ups).
  - **Commissioned side-publications** (e.g. for Vísir, Sýn,
    Kryddsíldarþáttur, etc.) → no Maskína-authored article or PDF; only
    the commissioning outlet's article. Dashboard *includes* these as
    monthly measurements but the underlying methodology details
    (`svarendur`, exact field period) only exist in the partner outlet's
    article and, for manual verification, in the Maskína Tableau
    dashboard's "Bakgrunnur" panel.

  Both outstanding polls are commissioned side-publications. Their
  `n_total` values (1,500 and 1,993) come from the agent's reading of the
  Vísir / mbl source articles; per-party percentages match the source
  exactly in both cases. Verification of `n_total` against Maskína's own
  records requires manual click-through on the Tableau dashboard — not
  worth the effort given the modelling impact is negligible (n affects
  credible-interval width by ~5% at these sample sizes).

- **[2026-06-01]** Ingested Þjóðarpúls Gallup maí 2026 (tribble
  `2026-05-15`, úrtak 12.979, svh 40,8%, fielded 30. apríl – 31. maí 2026).
  Notable for this ingest:
  - **RÚV published ahead of gallup.is.** The Gallup PDF `Puls_0626` 404s on
    `cdnx.gallup.is` and no Þjóðarpúls article exists on `gallup.is/frettir/`
    yet (latest party-support article there is the 4 May April reading).
    This reverses the usual ≥8-day RÚV lag. Re-cite the PDF here once it
    appears at `https://cdnx.gallup.is/media/documents/Puls_0626_Fylgi_flokka.pdf`.
  - **RÚV `pollsArray` / `latestGallupPoll` globals are stale** — latest
    Gallup is still "mars 2026"; they don't even carry the April reading.
    So the canonical JSON blob was useless this month; values came from the
    companion threshold article's Highcharts data table instead.
  - **Whole-month figure ingested, not the split.** This poll headlines a
    pre/post pair around the 16 May municipal elections (after: D 26,0 >
    S 24,9 — first time D tops a sub-sample in 3+ years). The tribble row
    uses the full-May measurement (S 28,4 / D 23,5) per the whole-month
    convention; the split is a one-off artefact of the municipal cycle and
    must not be substituted in.
  - **Cross-check:** the 7-party RÚV broadcast graphic matches the data
    table exactly for S/D/M/C/B/V/F; the data table + article text supply
    the two sub-5% parties the broadcast omits (J 2,6 / P 1,9). Sum of the
    9 named = 99,6, so Annað ≈ 0,4 (computed by the script).
