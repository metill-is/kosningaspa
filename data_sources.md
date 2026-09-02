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
| 2025-01-17 | 10,908 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0225_Fylgi_flokka.pdf) ([Gallup dashboard](https://www.gallup.is/nidurstodur/thjodarpuls/fylgi-flokka-til-althingis/)) | Fielded 2. janúar – 2. febrúar 2025; úrtak 10.908, svh 48,6%, vikmörk 0,4–1,2 pp. Þjóðarpúls janúar 2025 (published 3 Feb 2025 as `Puls_0225`). **BACKFILLED 2026-09-02 — this reading had never been ingested**; the tribble jumped 2024-12-24 → 2025-02-16. 8 parties from the PDF's "Jan. '25" row (S 21,7 / D 20,5 / C 16,2 / M 12,7 / F 10,6 / B 6,7 / J 5,2 / P 3,5); **Vinstri græn omitted from the PDF table** (prose: "rúmlega 2% Vinstri græn"), exact **V 2,20 → 2,2** from the Looker series. Annað 0,7%, matching the prose "næstum 1% aðra flokka". Note `Puls_0225` is a **rasterised** Publisher export — `pdftotext` returns almost nothing, so it must be rendered (`pdftoppm -png`) and read as an image. Also note the deterministic `cdnx.gallup.is` path works here, contrary to the skill's old "pre-May-2025 needs an opaque id" note. Date is the linear midpoint of the 2 Jan – 2 Feb span (exactly 17) |
| 2025-02-16 | 9,652 | [RÚV](https://www.ruv.is/frettir/innlent/2025-03-03-samfylkingin-tekur-stokk-i-nyjum-thjodarpulsi-437990) ([mbl](https://www.mbl.is/frettir/innlent/2025/03/03/gallup_samfylkingin_i_mikilli_sokn/)) | Þjóðarpúls mars 2025; n_total not independently verified from a gallup.is source |
| 2025-03-17 | 10,324 | [PDF](https://www.gallup.is/documents/917/Puls_0425_Fylgi_flokka.pdf) | Fielded 3.–31. mars 2025; úrtak 10.324, svh 47,5% |
| 2025-04-15 | 10,005 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0525_Fylgi_flokka.pdf) | Fielded 1.–30. apríl 2025; úrtak 10.005, svh 46,7% |
| 2025-05-16 | 11,521 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0625_Fylgi_flokka.pdf) | Fielded 1. maí – 1. júní 2025; úrtak 11.521, svh 44,9% |
| 2025-06-16 | 10,216 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0725_Fylgi_flokka.pdf) | Fielded 2.–30. júní 2025; úrtak 10.216, svh 46,5%. Tribble corrected 2026-05-20 (was 10,500) |
| 2025-07-16 | 11,541 | [PDF](https://www.gallup.is/documents/1214/Puls_0825_Fylgi_flokka.pdf) | Fielded 1.–31. júlí 2025; úrtak 11.541, svh 43,6% |
| 2025-08-16 | 10,055 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0925_Fylgi_flokka.pdf) | Fielded 1.–31. ágúst 2025; úrtak 10.055, svh 44,5% |
| 2025-09-15 | 10,887 | [PDF](https://cdnx.gallup.is/media/documents/Puls_1025_Fylgi_flokka.pdf) | Fielded 1.–30. september 2025; úrtak 10.887, svh 43,1% |
| 2025-10-17 | 11,225 | [PDF](https://cdnx.gallup.is/media/documents/Puls_1125_Fylgi_flokka.pdf) ([Gallup dashboard](https://www.gallup.is/nidurstodur/thjodarpuls/fylgi-flokka-til-althingis/)) | Fielded 1. október – 2. nóvember 2025; úrtak 11.225, svh 46,5%, vikmörk 0,5–1,4 pp. Þjóðarpúls október 2025. **BACKFILLED 2026-09-02 — this reading had never been ingested**; the tribble jumped 2025-09-15 → 2025-11-16. 8 parties from the PDF's "Okt. 2025" row (S 31,9 / D 17,6 / M 16,3 / C 13,5 / F 5,9 / B 5,5 / P 3,9 / V 2,6); **Sósíalistaflokkurinn omitted from the PDF table**, exact **J 2,30 → 2,3** from the Looker series. Annað 0,5% (in the recent norm). Note the PDF drops whichever party is smallest — here J, not P. Date is the linear midpoint of the 1 Oct – 2 Nov span (exactly 17) |
| _2025-10-15_ | _11,225_ | _[PDF](https://cdnx.gallup.is/media/documents/Puls_1125_Fylgi_flokka.pdf)_ | _**MISSING FROM TRIBBLE.** Fielded 1. október – 2. nóvember 2025; úrtak 11.225, svh 46,5%. Per-party percentages need verification before tribble insertion (see Verification log)_ |
| 2025-11-16 | 10,332 | [PDF](https://cdnx.gallup.is/media/documents/Puls_1225_Fylgi_flokka.pdf) | Fielded 3.–30. nóvember 2025; úrtak 10.332, svh 41,8% |
| 2025-12-15 | 9,091 | [PDF](https://cdnx.gallup.is/media/documents/Puls_1225_2_Fylgi_flokka.pdf) ([Gallup dashboard](https://www.gallup.is/nidurstodur/thjodarpuls/fylgi-flokka-til-althingis/)) | Fielded 1.–28. desember 2025; úrtak 9.091, svh 43,4%. Tribble corrected 2026-05-20 (was 10,000 default). **CORRECTION 2026-09-02: D was 16,8, sources say 16,9** — Gallup's Looker series gives D 16,87 (→ 16,9 half-up) and the `Puls_0226` "Des. 2025" comparison row prints 16,9%. Transcription slip; the only real value error found in 21 rows × 9 parties. **Date note: floor(midpoint) of 1.–28. des is the 14th, not the stored 15th** — the one Gallup row not following the floor convention; left unchanged |
| 2026-01-20 | 9,713 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0226_Fylgi_flokka.pdf) | Fielded 9. janúar – 1. febrúar 2026; úrtak 9.713, svh 43,6%. Tribble corrected 2026-05-20 (was 10,000 default). **CORRECTION 2026-09-02: date was 2026-01-15, source gives floor(midpoint) = 2026-01-20** (9 Jan – 1 Feb is a 24-day span; midpoint 20,5 → floored to the 20th). Values were already correct |
| 2026-02-15 | 9,958 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0326_Fylgi_flokka.pdf) | Fielded 2. febrúar – 1. mars 2026; úrtak 9.958, svh 44,3% |
| 2026-03-16 | 10,746 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0426_Fylgi_flokka.pdf) ([Gallup article](https://www.gallup.is/frettir/framsokn-tapar--fylgi/)) | Fielded 2.–31. mars 2026; úrtak 10.746, svh 42,3%. Tribble corrected 2026-05-20 (was 10,000 default) |
| 2026-04-15 | 10,484 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0526_Fylgi_flokka.pdf) ([Gallup article](https://www.gallup.is/frettir/sjalfstaedisflokkurinn-baetir-vid-sig-fylgi/)) | Fielded 1.–29. apríl 2026; úrtak 10.484, svh 40,4% |
| 2026-05-15 | 12,979 | [RÚV (þröskuldsfrétt)](https://www.ruv.is/frettir/innlent/2026-06-01-flokkur-folksins-maelist-utan-things-476896) ([RÚV forsíðufrétt](https://www.ruv.is/frettir/innlent/2026-06-01-sjalfstaedisflokkurinn-staerri-en-samfylking-eftir-kosningar-476906)) | Fielded 30. apríl – 31. maí 2026; úrtak 12.979, svh 40,8%, vikmörk 0,4–1,3 pp. Þjóðarpúls maí 2026. **Gallup PDF (Puls_0626) not yet published at ingest — RÚV published ahead of gallup.is.** Full 9-party breakdown + methodology taken from the companion threshold article's interactive Highcharts data table (S 28,4 / D 23,5 / M 17,6 / C 10,6 / B 6,7 / V 4,3 / F 4,0 / J 2,6 / P 1,9); whole-month figure used per convention (poll also reports a pre/post-16 May municipal-election split — not ingested). Broadcast graphic (7 largest parties) cross-checks exactly |
| 2026-06-15 | 12,102 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0726_Fylgi_flokka.pdf) ([RÚV](https://www.ruv.is/frettir/innlent/2026-07-01-bilid-minnkar-milli-samfylkingarinnar-og-sjalfstaedisflokksins-480049), [Gallup dashboard](https://www.gallup.is/data/geytenbr/sso/)) | Fielded 1.–30. júní 2026; úrtak 12.102, svh 38,5%, vikmörk 0,5–1,4 pp. Þjóðarpúls júní 2026. **Puls_0726 PDF has since appeared and is now the primary citation (re-cited 2026-08-19); its "Júní 2026" row confirms S 26,2 / D 24,9 / M 15,1 / C 11,4 / B 5,3 / V 5,0 / F 4,6 / J 4,3 exactly.** 8 parties + methodology from the RÚV article's embedded Highcharts `pollData` (S 26,2 / D 24,9 / M 15,1 / C 11,4 / B 5,3 / V 5,0 / F 4,6 / J 4,3). **Píratar omitted from the RÚV article/chart** (first Gallup reading where RÚV drops P) but present in Gallup's own Looker dashboard at **P 2,7%** — that value used, giving Annað 0,5% (in line with the 0,4–0,6% recent norm). |
| 2026-07-17 | 13,167 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0826_Fylgi_flokka.pdf) ([Gallup article](https://www.gallup.is/frettir/litlar-breytingar--a-fylgi/), [Vísir](https://www.visir.is/g/20262917133d/mark-taekur-munur-a-fylgi-sam-fylkingar-og-sjalf-staedis-flokks)) | Fielded 1. júlí – 3. ágúst 2026; úrtak 13.167, svh 37,2%, vikmörk 0,5–1,4 pp. Þjóðarpúls júlí 2026 (published 5 Aug as `Puls_0826`). 8 parties from the PDF's "Júlí 2026" row (S 27,9 / D 25,4 / M 14,6 / C 11,4 / B 5,8 / F 5,2 / V 3,9 / J 3,2). **Píratar omitted from the PDF table** — prose says only "rúmlega 2% Pírata"; exact **P 2,2%** taken from Vísir ("Sósíalistar njóta nú stuðnings 3,2 prósenta kjósenda miðað við könnunina og Píratar 2,2 prósenta"), giving Annað 0,4% (in the 0,4–0,6% recent norm). Date is the linear midpoint of the 1 Jul – 3 Aug span (17,5 → floored to the 17th, per the dominant tribble convention) |
| 2026-08-17 | 13,836 | [PDF](https://cdnx.gallup.is/media/documents/Puls_0926_Fylgi_flokka.pdf) ([Gallup dashboard](https://www.gallup.is/nidurstodur/thjodarpuls/fylgi-flokka-til-althingis/), [RÚV](https://www.ruv.is/frettir/innlent/2026-09-01-samfylkingin-staekkar-en-vidreisn-minnkar-485813), [Vísir](https://www.visir.is/g/20262929192d/vid-reisn-tapar-fylgi)) | Fielded 4.–31. ágúst 2026; úrtak 13.836, svh 40,2%, vikmörk 0,4–1,3 pp. Þjóðarpúls ágúst 2026 (published as `Puls_0926` ~1 Sept; net- og símakönnun). 8 parties from the PDF's "Ágúst 2026" row (S 29,6 / D 26,8 / M 15,6 / C 9,7 / B 5,5 / F 4,8 / V 3,6 / J 2,4), confirmed verbatim by the RÚV article's embedded Highcharts `pollData`. **Píratar omitted from the PDF table, the RÚV chart and *all* press coverage** (0 hits for "Pírat" across RÚV/Vísir/Viðskiptablaðið/Heimildin); PDF prose says only "um 2% Pírata". Exact **P 1,93% → 1,9** recovered from Gallup's own Looker series (`gallup_dw::fylgi_til_althingis`, view `gallup_althingisfylgi`, row `response_date = 2026-08-31`), which also reproduces all eight tabulated parties to 2 dp (29,56 / 26,75 / 15,57 / 9,66 / 5,52 / 4,75 / 3,59 / 2,42). Annað 0,1% — **below the 0,4–0,6% recent norm**, because the series shows Lýðræðisflokkurinn and Ábyrg framtíð both null this month. Date is the linear midpoint of the 4–31 Aug span (exactly 17, no rounding needed) |

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
| 2026-06-06 | 1,705 | [PDF](https://maskina.is/wp-content/uploads/2026/07/2026-06-Fylgi-Althingi_Maskinuskyrsla.pdf) ([maskina.is](https://maskina.is/sjalfstaedisflokkurinn-baetir-i/), [Vísir](https://www.visir.is/g/20262901108d/sjalf-staedis-flokkur-a-flugi-og-rikis-stjornin-aldrei-maelst-med-minna-fylgi), [RÚV](https://www.ruv.is/frettir/innlent/2026-06-24-samfylkingin-staerst-en-sjalfstaedisflokkur-vinnur-a-479261)) | Fielded 2.–11. júní 2026; svarendur 1.705. **CORRECTION: n_total was 1,700 (estimate from the Vísir phrase "rúmlega 1700 tóku afstöðu"); the Maskína PDF's `Gild svör` row gives exactly 1.705.** **Also corrects the prior "side-publication for Sýn — no Maskína-published PDF" note: this IS a regular monthly Alþingi reading** — maskina.is published "Sjálfstæðisflokkurinn bætir í" on 22 June 2026 with a linked PDF report, hosted (unusually) under the `/2026/07/` upload path, which is why the earlier pagination walk missed it. PDF confirms per-party exactly: S 25,2 / D 22,7 / M 14,2 / C 12,4 / B 8,5 / V 5,4 / F 4,1 / J 3,9 / P 3,5 |
| 2026-07-02 | 963 | [PDF](https://maskina.is/wp-content/uploads/2026/07/2026-07-Fylgi-Althingi_Maskinuskyrsla.pdf) ([maskina.is](https://maskina.is/sjalfstaedisflokkurinn-a-flugi/), [Vísir](https://www.visir.is/g/20262912056d/sjalfstaedisflokkur-maelist-staerstur)) | Fielded 26. júní – 8. júlí 2026; svarendur 963 (91% gáfu upp afstöðu). Regular monthly reading, published 21–22 July. Per-party direct from the PDF's `Niðurstöður` table with raw counts: D 25,0 (240) / S 24,4 (235) / M 13,4 (129) / C 12,7 (122) / B 6,6 (64) / F 5,9 (57) / V 4,9 (47) / J 4,0 (39) / P 3,1 (30); `Gild svör` 963 = 100,0%, so Annað 0,0. First Maskína reading since the 2024 election with **D ahead of S**. **Field-period discrepancy: the maskina.is article says "frá 28. júní til 8. júlí" but the PDF methodology page says "26. júní til 8. júlí". PDF used (skill convention), and independently corroborated by [Vísir 30 July](https://www.visir.is/g/20262915223d/dregur-ur-anaegju-med-frammistodu-rikisstjornarflokkanna) on the same field wave ("framkvæmd dagana 26. júní til 8. júlí"). Midpoint 26 Jun–8 Jul = 2 July; the article's dates would give 3 July** |
| 2026-08-01 | 3,172 | [RÚV](https://www.ruv.is/frettir/innlent/2026-08-20-samfylkingin-maelist-staerst-a-althingi-a-ny-484481) ([Vísir](https://www.visir.is/g/20262923931d/sam-fylkingin-aftur-staerst-en-rikis-stjornin-stendur-i-stad)) | Fielded in two waves, 22.–29. júlí and 7.–11. ágúst 2026; svarendur 3.172 ("Alls tóku 3.172 svarendur afstöðu til flokka"). **Media-only release — no maskina.is article and no `2026-08` Maskínuskýrsla PDF exists**, so the usual PDF citation is unavailable; per-party values taken from the RÚV article's embedded Highcharts `pollData` (S 26,2 / D 25,1 / M 12,7 / C 11,5 / B 7,8 / F 5,2 / V 4,2 / J 3,9 / P 3,5). The nine named parties sum to **100,1%**, so `pmax(0, …)` clamps Annað to 0,0. Composite poll: date is the linear midpoint of the full 22 Jul – 11 Aug span (= 1 Aug), per the skill's composite rule |

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

- **[2026-09-02b]** Backfilled the 2 missing Gallup readings and applied 2 corrections
  (user-approved follow-up to the 2026-09-02 ingest above). The Gallup block is now
  **complete and exact**: all 21 post-election Þjóðarpúls readings present, each
  matching Gallup's own series on all 9 parties under round-half-up.
  - **BACKFILL — Þjóðarpúls janúar 2025** → tribble `2025-01-17`, úrtak 10.908,
    fielded 2. jan – 2. feb 2025. `Puls_0225` is a **rasterised** Publisher export
    (`pdftotext` yields ~24 bytes); rendered with `pdftoppm -png` and read as an
    image. V recovered from the Looker series (dropped from the PDF table).
  - **BACKFILL — Þjóðarpúls október 2025** → tribble `2025-10-17`, úrtak 11.225,
    fielded 1. okt – 2. nóv 2025. J recovered from the Looker series (dropped from
    the PDF table — note Gallup drops whichever party is *smallest*, not
    specifically Píratar).
  - **CORRECTION — Gallup date `2026-01-15` → `2026-01-20`.** `Puls_0226` states
    fielding "9. janúar - 1. febrúar 2026"; floored midpoint is the 20th. Identity
    certain (stored úrtak 9.713 matches the PDF exactly).
  - **Date convention re-verified against every Gallup PDF.** Field periods were
    extracted from all 19 reachable Þjóðarpúls PDFs and the midpoint recomputed.
    **18 of 19 match floor-of-midpoint exactly**, including both backfills and the
    corrected January 2026 row. One row still does not:
    - `2025-12-15` — fielded 1.–28. desember 2025, so floor(midpoint) = **2025-12-14**;
      the stored date is the 15th (i.e. half-up). **Left as-is, 1 day, flagged only** —
      it was not part of the requested change set and the effect is negligible, but
      the tribble is therefore *not* 100 % consistent on this convention.
    - Two rows could not be checked at all: `2024-12-24` and `2025-02-16`, whose PDFs
      (`Puls_0125`, `Puls_0325`) 404 on the CDN path. Their *values* are confirmed by
      the Looker series; only their field periods are unverified.
  - **CORRECTION — Gallup `2025-12-15` Sjálfstæðisflokkurinn 16,8 → 16,9.** Found by
    the full reconciliation, not by inspection. Gallup's series gives D 16,87 (→ 16,9
    half-up) and the `Puls_0226` "Des. 2025" comparison row independently prints
    16,9%. The stored 16,8 was a transcription slip. This was the **only** real value
    error in 21 rows x 9 parties = 189 values.
  - **Method note for future audits.** Six other rows initially flagged as 0,1 pp off
    were **false positives from float rounding** — every one was an exact `.x5`
    half-way value (7,35 / 30,65 / 3,25 / 3,55 / 2,65 / 9,95) where Python's
    `round()` goes to even but Gallup rounds half-up. Compare with
    `Decimal(...).quantize(Decimal('0.1'), rounding=ROUND_HALF_UP)`, or you will
    "find" six discrepancies that do not exist.
  - **`cdnx.gallup.is` reaches further back than documented.** `Puls_0225`
    (Feb 2025) resolves fine on the deterministic CDN path, so the skill's
    "pre-May-2025 needs an unguessable opaque id" caveat is at least partly wrong.
    Try the CDN path first regardless of date.

- **[2026-09-02]** Ingested 2 new polls. Also ran a **full reconciliation of every
  Gallup row against Gallup's own back-end series**, which surfaced two
  pre-existing gaps (see below).
  - **NEW — Þjóðarpúls Gallup ágúst 2026** (tribble `2026-08-17`, úrtak 13.836,
    svh 40,2%, fielded 4.–31. ágúst 2026). Discovery again ran off the guessable
    CDN path (`Puls_0926`), not RÚV: **RÚV's `pollsArray`/muninn API still had not
    published the August reading** at ingest time (newest entry `2026-08-04` = the
    July reading), even though RÚV's *article* went up 1 Sept. gallup.is/frettir/
    had no ágúst fylgi article either (newest fylgi item was 5 Aug = July reading).
  - **Gallup's Looker back-end is now programmatically reachable — use it first.**
    The skill previously recorded the dashboard as gated/cross-origin. It is not.
    `https://www.gallup.is/nidurstodur/thjodarpuls/fylgi-flokka-til-althingis/`
    embeds a *signed* Looker SSO URL; following it with a cookie jar authenticates
    an anonymous embed session, after which
    `GET https://gogn.gallup.is/api/internal/core/4.0/dashboards/gallup_dw::fylgi_til_althingis`
    yields the element's query id and
    `GET …/api/internal/core/4.0/queries/<id>/run/json` returns the **complete
    party-support series to 2 dp**, all parties, every month back to 2021.
    This is strictly better than the PDF (which drops sub-3% parties) and than RÚV
    (which lags and also drops them). **Two independent derivations agreed on
    P 1,93.**
  - **P recovery, third month running.** August is the third consecutive Gallup
    reading where Píratar is dropped from the published table. This month it was
    dropped from *every* public source — PDF table, RÚV `pollData`, and all four
    press write-ups checked (RÚV, Vísir, Viðskiptablaðið, Heimildin: 0 hits for
    "Pírat"). Only the Looker series carried it. Calibration note: Gallup's prose
    adverbs are reliable one-decimal hints — "hartnær 3%"=2,8, "liðlega 2%"=2,1,
    "næstum 2%"=1,9, "rúmlega 2%"=2,2, and this month's "um 2%"=1,93.
  - **NEW — Maskína ágúst 2026** (tribble `2026-08-01`, svarendur 3.172, fielded
    22.–29. júlí + 7.–11. ágúst). **Media-only release**: published 20 Aug via RÚV
    and Vísir with no maskina.is article and no Maskínuskýrsla PDF, so every
    `maskina.is/wp-content/uploads/2026/0{8,9}/…` probe 404s. This is the
    commissioned/side-publication pattern the skill warns about, and it is why the
    `maskina.is/frettir/` discovery channel alone would have missed it — its newest
    party-support article is still "Sjálfstæðisflokkurinn á flugi" (22 July).
  - **PRE-EXISTING GAP — Þjóðarpúls janúar 2025 is missing from the tribble.**
    Gallup's series carries a `2025-01-31` reading (S 21,66 / D 20,48 / C 16,17 /
    M 12,67 / F 10,64 / B 6,71 / J 5,17 / P 3,49 / V 2,20) that has no tribble row;
    the tribble jumps 2024-12-24 → 2025-02-16. **Not ingested in this session** —
    flagged for the user, since backfilling it changes historical model input.
  - **PRE-EXISTING GAP — Þjóðarpúls október 2025 is missing from the tribble.**
    Gallup's series carries a `2025-10-31` reading (S 31,93 / D 17,64 / M 16,31 /
    C 13,46 / F 5,87 / B 5,52 / P 3,85 / V 2,58 / J 2,30); the tribble jumps
    2025-09-15 → 2025-11-16. `Puls_1125` gives fielding 1. okt – 2. nóv 2025,
    úrtak 11.225, which would date it `2025-10-17`. **Not ingested in this
    session** — flagged for the user.
  - **POSSIBLE DATE ERROR — Gallup `2026-01-15`.** `Puls_0226` states fielding
    "9. janúar - 1. febrúar 2026", whose floored midpoint is `2026-01-20`, not the
    stored 15th. Identity is certain (stored n_total 9.713 matches the PDF úrtak
    exactly). This is the only one of 18 Gallup rows that breaks the
    floor-of-midpoint convention. **Not changed** — flagged for the user.
  - **Cross-check:** the reconciliation matched all 18 pre-existing Gallup tribble
    rows 1:1 to a Gallup series reading with max per-row absolute deviation 0,3 pp
    (pure 1-dp rounding), so apart from the three items above the Gallup block is
    clean. August's 9 named parties sum to 99,9 (Annað 0,1); Maskína's to 100,1
    (Annað clamped to 0,0).

- **[2026-08-19]** Ingested 2 new polls; corrected 1 existing row; re-cited 1.
  - **NEW — Þjóðarpúls Gallup júlí 2026** (tribble `2026-07-17`, úrtak 13.167,
    svh 37,2%, fielded 1. júlí – 3. ágúst 2026). Primary source is the
    `Puls_0826` PDF, guessed straight off the deterministic
    `cdnx.gallup.is/media/documents/Puls_MMYY_Fylgi_flokka.pdf` pattern — no
    browser needed. **Píratar again absent from the source table** (third
    month running that P goes missing somewhere in the chain); the PDF prose
    says only "rúmlega 2%". Recovered as **P 2,2%** from the Vísir write-up,
    verified against the raw article HTML rather than a summariser. sumNamed
    = 99,6 → Annað 0,4, inside the 0,4–0,6 norm.
  - **NEW — Maskína júlí 2026** (tribble `2026-07-02`, svarendur 963, fielded
    26. júní – 8. júlí 2026). Regular monthly reading with a full PDF report.
    The PDF is a Tableau export whose text layer is per-character, so
    `pdftotext` output is unreadable as prose; reconstructing rows by
    y-coordinate from `pdftotext -bbox` recovered the `Niðurstöður` table
    cleanly, with raw counts alongside percentages. Article-vs-PDF field-period
    disagreement (28 vs 26 June) resolved in favour of the PDF, corroborated
    by a second Vísir article on the same wave.
  - **CORRECTION — Maskína `2026-06-06`: n_total 1,700 → 1,705.** The
    June PDF exists after all, at
    `maskina.is/wp-content/uploads/**2026/07**/2026-06-Fylgi-Althingi_Maskinuskyrsla.pdf`
    — note the upload path month (07) differs from the report month (06),
    which is why the 2026-05-20 pagination walk concluded no PDF existed and
    filed the poll as a Sýn side-publication. That note is now corrected: it
    is a regular monthly reading. Per-party values were already exact.
  - **RE-CITED — Gallup `2026-06-15`:** `Puls_0726` has since been published
    and replaces RÚV as the primary source. Its "Júní 2026" row confirms all
    eight parties exactly, retroactively validating last month's ingest
    (including the P 2,7% dashboard recovery, which the PDF still omits).
  - **RÚV's `pollsArray` is badly stale** — newest entry `2026-04-20`, roughly
    four months behind the tribble, so step 2 of the skill (RÚV as primary
    discovery channel) produced nothing. Discovery ran off `gallup.is/frettir/`
    + the guessable PDF path and `maskina.is/frettir/` instead. If RÚV stays
    stale, the skill's source ordering should be flipped.
  - **Confirmed no third poll in the gap.** Vísir's Skoðanakannanir tag was
    walked for everything after 4 Aug: all five newer items are EU-referendum
    polls ahead of the late-August þjóðaratkvæðagreiðsla, not party support.
    `Puls_0926` 404s and no `2026-08` Maskína report exists yet.

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

- **[2026-07-01]** Ingested Þjóðarpúls Gallup júní 2026 (tribble
  `2026-06-15`, úrtak 12.102, svh 38,5%, fielded 1.–30. júní 2026).
  Notable for this ingest:
  - **RÚV published ahead of gallup.is again.** The Gallup PDF `Puls_0726`
    404s on `cdnx.gallup.is` and no Þjóðarpúls júní article exists on
    `gallup.is/frettir/` yet (latest party-support article there is the
    2 June reading). Second month running that RÚV beats the PDF. Re-cite
    once it appears at
    `https://cdnx.gallup.is/media/documents/Puls_0726_Fylgi_flokka.pdf`.
  - **Values from the article's embedded `__NEXT_DATA__`, not Chrome.** The
    RÚV article is a Next.js page; the full per-party breakdown ships in the
    Highcharts `pollData` array and the methodology line in a `raw_code_block`
    inside the server-rendered `__NEXT_DATA__` payload. `curl` + JSON parse
    gave exact values without needing the Chrome extension (which was
    disconnected this session).
  - **Píratar omitted from the RÚV article/chart, recovered from Gallup's
    dashboard.** Neither the article body nor the chart `pollData` carried a
    Píratar value (first Gallup reading where RÚV drops P; previous month P
    was 1,9%). Initially entered as `P = 0.0` (source-fidelity), which pushed
    `sumNamed` to 96,8 vs the recent 99,4–99,6 norm — a red flag. Gallup's own
    Looker dashboard (`gallup.is/data/geytenbr/sso/`, "Þróun á fylgi flokka
    til Alþingis") shows **Píratar 2,7%** for June. Tribble corrected to
    `P = 2.7`, which restores Annað to 0,5% (normal band) and `sumNamed` to
    99,5. The RÚV chart simply omits sub-3% Píratar; the underlying Gallup
    measurement carries it.
  - **Cross-check:** the 9 named parties now sum to 99,5 (S 26,2 / D 24,9 /
    M 15,1 / C 11,4 / B 5,3 / V 5,0 / F 4,6 / J 4,3 / P 2,7); seats in the RÚV
    chart (S 19 / D 18 / M 11 / C 8 / B 4 / V 3, rest 0) total 63.
