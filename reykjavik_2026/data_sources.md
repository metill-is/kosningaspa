# Data Sources

Provenance log for all polls and election results used in the Reykjavík 2026 forecast.
One section per data file. Within each, polls are listed chronologically with the
canonical source URL and any notes about sample size, fielding window, or attribution.

**Convention**: `date` columns are the **end-of-fielding** date where known, otherwise
the publication date. `n` is the count of respondents who named a party (not total
úrtak or total respondents).

---

## `polls.csv` — 2026 cycle (current)

Wide format, 11 parties (D, S, A, M, C, B, J, F, P, G, O).

| Date | Pollster | n | Source | Notes |
|---|---|---|---|---|
| 2025-09-30 | Maskína Borgarviti | 900 (est) | comparison chart in [Feb 2026 PDF](https://maskina.is/wp-content/uploads/2026/03/2026-02-Borgarstjorn-Borgarviti-Maskinu_v2.pdf) | n estimated |
| 2025-10-31 | Maskína Borgarviti | 900 (est) | same Feb 2026 PDF chart | n estimated |
| 2025-11-30 | Maskína Borgarviti | 900 (est) | same Feb 2026 PDF chart | n estimated |
| 2025-12-31 | Maskína Borgarviti | 900 (est) | same Feb 2026 PDF chart | n estimated |
| 2026-01-31 | Maskína Borgarviti | 900 (est) | same Feb 2026 PDF chart | n estimated; Vor til vinstri joined V_or_A slot from 1 Jan 2026 |
| 2026-02-27 | Maskína Borgarviti | 1015 | [PDF](https://maskina.is/wp-content/uploads/2026/03/2026-02-Borgarstjorn-Borgarviti-Maskinu_v2.pdf) ([post](https://maskina.is/fylgid-i-borginni/)) | Fielded 1–27 Feb 2026; Vinstrið slot from 25 Feb 2026 |
| 2026-03-19 | Maskína Borgarviti | 1442 | [DV article](https://www.dv.is/frettir/2026/03/28/fylgid-flugi-nyrri-konnun-borginni-einn-flokkur-lykilstodu/) | Fielded 12–19 Mar 2026; Okkar borg 2.3% + other 2.1% collapsed to O=4.4; added 2026-05-16 after audit identified 7-week observation gap |
| 2026-04-15 | Gallup Þjóðarpúls | 634 | [PDF](https://www.gallup.is/documents/2204/Puls_0426_Fylgi_flokka_Reykjavik.pdf) ([page](https://www.gallup.is/fylgi-flokka-til-borgarstjornar/)) | Fielded 1–15 Apr 2026; úrtak 2138, 40.1% þátttaka, 74% nefna flokk → n=634 |
| 2026-05-06 | Maskína (for Vísir) | 700 (est) | [kosningasaga](https://kosningasaga.wordpress.com/2026/05/06/skodanakonnun-i-reykjavik-9/) | Fielded 29 Apr – 6 May 2026 |
| 2026-05-12 | Maskína (for Sýn) | 1511 | [Vísir](https://www.visir.is/g/20262883450d/sjalf-staedis-flokkurinn-staerstur-og-fylgi-mid-flokks-eykst-a-ny) | Fielded 6–12 May 2026; date and n corrected from verification |
| 2026-05-15 | Gallup (for RÚV) | 800 (est) | [RÚV article](https://www.ruv.is/frettir/innlent/2026-05-15-sjalfstaedisflokkurinn-a-siglingu-i-lokakonnun-gallups-475158) | Fielded 11–15 May 2026; úrtak 3587, svarhlutfall 41.3% (1481 svör); "nefna flokk" not disclosed separately |
| 2026-05-15 | Maskína (for Vísir) | 2950 | [Maskína chart](https://maskina.is/wp-content/uploads/2026/05/tidni.png) ([Vísir](https://www.visir.is/g/20262884571d/sjalf-staedis-flokkur-staerstur-en-naer-ekki-meiri-hluta-med-midflokki-og-fram-sokn)) | Fielded 12–15 May 2026; n=2950 corrected from chart (RÚV says "tæplega 3.000 svarendur") |

---

## `historical_polls.csv` — 2022 cycle

Long format. One row per party per poll. Filtered with `election_year == 2022, !is.na(pct)`
plus a >= 95% completeness check.

| Date | Pollster | n | Source | Notes |
|---|---|---|---|---|
| 2021-11-30 | Maskína Borgarviti | 900 (est) | comparison chart in [Maskína Mars 2022 PDF](https://maskina.is/wp-content/uploads/2022/03/2022_03_Fylgi_Reykjav%C3%ADk_Mask%C3%ADnusk%C3%BDrsla-1.pdf) | Date approximated to end of November; n unknown |
| 2022-02-14 | Maskína Borgarviti | 900 (est) | same Mars 2022 PDF chart | Date approximated; n unknown |
| 2022-03-09 | Maskína Borgarviti | 971 | [PDF](https://maskina.is/wp-content/uploads/2022/03/2022_03_Fylgi_Reykjav%C3%ADk_Mask%C3%ADnusk%C3%BDrsla-1.pdf) ([post](https://maskina.is/miklar-sviptingar-i-fylgi-flokkanna-i-reykjavik/)) | Fielded 17 Feb – 9 Mar 2022 |
| 2022-03-29 | Maskína Borgarviti | 828 | [PDF](https://maskina.is/wp-content/uploads/2022/04/2022_04_Fylgi_RVK_Mask%C3%ADnusk%C3%BDrsla.pdf) ([post](https://maskina.is/samfylkingin-og-sjalfstaedisflokkurinn-hnifjofn-i-fylgi-i-reykjavik/)) | Fielded 22–29 Mar 2022, published 6 Apr |
| 2022-05-02 | Maskína Borgarviti | 825 | [PDF](https://maskina.is/wp-content/uploads/2022/05/2022_05_Fylgi_flokka_Rvk_Mask%C3%ADnusk%C3%BDrsla-2.pdf) ([post](https://maskina.is/samfylkingin-maelist-staerst-i-borginni-en-sjalfstaedisflokkurinn-fylgir-fast-a-haela-hennar/)) | Fielded 8 Apr – 2 May 2022 |
| 2022-05-10 | Fréttablaðið (Zenter) | 700 (est) | [kosningasaga](https://kosningasaga.wordpress.com/2022/05/10/skodanakonnun-i-reykjavik-5/) | n unknown |
| 2022-05-11 | Maskína (for Stöð 2) | 700 (est) | [kosningasaga](https://kosningasaga.wordpress.com/2022/05/12/skodanakonnun-i-reykjavik-7/) | Different from Maskína Borgarviti |
| 2022-05-13 | Maskína (paired) | 700 (est) | [kosningasaga "Tvær kannanir"](https://kosningasaga.wordpress.com/2022/05/13/tvaer-kannanir-i-reykjavik/) | First-column attribution to Maskína by word order — **flagged for verification** |
| 2022-05-13 | Gallup (paired) | 800 (est) | same "Tvær kannanir" post | Second-column attribution to Gallup |

## `historical_polls.csv` — 2018 cycle

| Date | Pollster | n | Source | Notes |
|---|---|---|---|---|
| 2018-04-04 | Gallup (Þjóðarpúls) | 1000 (est) | [Gallup PDF May 25 comparison chart](https://cdnx.gallup.is/media/documents/Puls_Fylgi_flokka_Reykjavik_web.pdf) | Fielded 8 Mar – 4 Apr 2018; n estimated |
| 2018-05-03 | Gallup (Þjóðarpúls) | 1000 (est) | same May 25 PDF chart | Fielded 4 Apr – 3 May 2018; n estimated |
| 2018-05-08 | Fréttablaðið (Zenter?) | NA | [kosningasaga](https://kosningasaga.wordpress.com/2018/05/08/skodanakonnun-i-reykjavik/) | **Incomplete** — only 6 parties (sum=87.5%); excluded by completeness filter |
| 2018-05-14 | Gallup (for VB) | 731 | [vb.is](https://vb.is/frettir/meirihlutanum-ekki-haggad/) | Fielded 2–14 May 2018; úrtak 1510, svarendur 862, svara 731 |
| 2018-05-17 | Gallup (seats-only legacy) | NA | [kosningasaga](https://kosningasaga.wordpress.com/2018/05/17/skodanakonnun-i-reykjavik-2/) | Same poll as 2018-05-14 above |
| 2018-05-23 | Félagsvísindastofnun | NA | [kosningasaga](https://kosningasaga.wordpress.com/2018/05/23/skodanakonnun-i-reykjavik-3/) | Seats-only; percentages not located |
| 2018-05-25 | Fréttablaðið (seats-only) | NA | [kosningasaga](https://kosningasaga.wordpress.com/2018/05/25/skodanakonnun-i-reykjavik-4/) | Seats-only; different poll from same-day Gallup-for-RÚV |
| 2018-05-25 | Gallup (for RÚV) | 954 | [mbl.is](https://www.mbl.is/frettir/kosning/2018/05/25/sjalfstaedisflokkurinn_staerstur_i_borginni/) | Fielded 22–25 May; úrtak 2215, 1104 svör, 954 nefna flokk |

## `historical_polls.csv` — inter-cycle Gallup polls (used for 2026 cycle)

Gallup did regular Reykjavík polls for Viðskiptablaðið throughout 2024–2026, plus
a December 2025 Þjóðarpúls for RÚV. Stored in `historical_polls.csv` with
`election_year = 2026` so they enter the 2026 cycle's random walk.

| Date | Pollster | n | Source | Notes |
|---|---|---|---|---|
| 2025-01-31 | Gallup (for VB) | 1200 (est) | [vb.is](https://vb.is/frettir/gallup-sjalfstaedisflokkurinn-eykur-fylgid-um-101/) | Fielded 13–31 Jan 2025; n estimated |
| 2025-03-30 | Gallup (for VB) | 1716 | [mbl.is](https://www.mbl.is/frettir/innlent/2025/04/01/talsverd_tidindi_i_konnun_gallup_i_reykjavik/) | Fielded 1–30 Mar 2025; úrtak 3598, svarhlutfall 47.7% |
| 2025-05-31 | Gallup (for VB) | 1850 | [mbl.is](https://www.mbl.is/frettir/innlent/2025/06/18/framsokn_missir_alla_fjora_i_borginni/) | Fielded 1–31 May 2025; úrtak 4263, svarhlutfall 43.4% |
| 2025-10-12 | Gallup (for VB) | 1545 | [vb.is](https://vb.is/frettir/meirihlutinn-fallinn-/) | Fielded 15 Sep – 12 Oct 2025; úrtak 3220, svarhlutfall 48% |
| 2025-12-31 | Gallup (for VB) | 1200 (est) | [mbl.is](https://www.mbl.is/frettir/innlent/2026/01/03/sjalfstaedisflokkurinn_a_storsiglingu/) | Fielded Dec 2025; first reported by Vísir then mbl.is on 3 Jan 2026; *NOT Þjóðarpúls — corrected via verification* |
| 2026-01-31 | Gallup (for VB) | 1200 (est) | [RÚV](https://www.ruv.is/frettir/innlent/2026-02-04-meirihlutinn-i-borginni-fallinn-samkvaemt-nyrri-konnun-466084) | Fielded 5–31 Jan 2026 (not 5-30 as originally — corrected via verification); published Feb 3 (VB/Vísir), Feb 4 (RÚV) |
| 2026-02-28 | Gallup (for VB) | 1200 (est) | [RÚV](https://www.ruv.is/frettir/innlent/2026-03-03-hnifjafnt-milli-sjalfstaedisflokks-og-samfylkingar-i-borginni-468635) | Fielded 1–28 Feb 2026; n estimated; Vinstrið slot is VG+Vor til vinstri |

## `historical_polls.csv` — 2014 cycle (not currently used by model)

The CSV contains additional rows for 2014 cycle polls (Wikipedia table for 2014
election), but the model filter `election_year >= 2018` excludes them. Kept for
future reference. Source: [Wikipedia Borgarstjórnarkosningar í Reykjavík 2014](https://is.wikipedia.org/wiki/Borgarstj%C3%B3rnarkosningar_%C3%AD_Reykjav%C3%ADk_2014).

---

## `historical_results.csv` — official election results

Long format, one row per party per election. Cycles 2002–2022.

| Election | Date | Source | Notes |
|---|---|---|---|
| 2022 Reykjavík city council | 2022-05-14 | [Wikipedia overview](https://is.wikipedia.org/wiki/Borgarstj%C3%B3rnarkosningar_%C3%AD_Reykjav%C3%ADk) | 23 seats, 59,949 valid votes; cross-checked: vote sums match within rounding |
| 2018 Reykjavík city council | 2018-05-26 | same Wikipedia overview | 23 seats, 58,966 valid votes |
| 2014 Reykjavík city council | 2014-05-31 | same | 15 seats, 54,655 valid votes |
| 2010 Reykjavík city council | 2010-05-29 | same | 15 seats, 59,523 valid votes |
| 2006 Reykjavík city council | 2006-05-27 | same | 15 seats, 64,895 valid votes |
| 2002 Reykjavík city council | 2002-05-25 | same | 15 seats, 68,364 valid votes |

---

## Verification log

Three verification agents dispatched 2026-05-16 to fetch each source URL and confirm
the numbers in our CSVs. Results appended below.

- **[done 2026-05-16]** 2026 cycle polls in polls.csv:
  - 9 of 11 polls verified exactly
  - **Fixes applied**:
    - 2026-05-13 Maskína (for Sýn): date 2026-05-13 → **2026-05-12** (end of fielding); n 700 → **1511** (Vísir disclosed)
    - 2026-05-15 Maskína (for Vísir): n 2854 → **2950** (Maskína chart says Gild svör=2950; corroborated by RÚV "tæplega 3.000 svarendur")
  - Minor noted: 2026-05-13 Samfylking 19.9% is correct (Vísir prose has a typo "19.0" but the 100% sum check confirms 19.9)
- **[done 2026-05-16]** 2022 cycle polls:
  - 7 of 9 polls verified exactly against sources
  - **Fixes applied**:
    - 2022-03-29 Maskína: Píratar 11.4 → **11.6** (source PDF page 4 detail; original p3 had a typo)
    - 2022-05-11 Maskína: Miðflokkur 3.0 → **3.3** (per Vísir direct source)
    - 2022-05-13 paired polls had **major attribution swap** in original kosningasaga post:
      - **2022-05-13 Maskína**: F 3.6→6.5, M 3.0→3.5, P 13.3→14.5, S 22.7→22.8; added n=660; source corrected to mbl.is direct
      - **2022-05-13 Gallup**: D 21.8→21.5, F 6.5→3.6, M 3.5→2.0, P 14.5→13.2, S 22.8→24.0, V 4.6→4.5, Y 1.8→0.6, E 1.5→0.3; added n=1133; source corrected to [Gallup's own PDF](https://www.gallup.is/documents/719/4033400_LokakonnunFKosningar_130522_an_greininga.pdf)
- **[done 2026-05-16]** Gallup-finder agent: added 2 new 2018 polls (Apr 4, May 3) + 7 new 2026-cycle Gallup polls (Jan 2025 through Feb 2026) — 24 new rows total
- **[done 2026-05-16]** Independent verification of the 9 newly-added Gallup polls:
  - All percentages, seats, sample frame figures verified exactly against canonical sources
  - **Fixes applied**:
    - 2025-12-31: notes corrected ("Þjóðarpúls" → "for VB" — this was a VB commission, not the national survey)
    - 2026-01-30 → **2026-01-31**: fielding end date was Jan 31 not Jan 30 (VB/Vísir both explicit "5. til 31. janúar"; RÚV article hadn't disclosed the window so the original date was a guess)
  - No pollster-attribution errors, no percentage errors, no n changes needed
  - n estimates (1000 for 2018 polls, 1200 for 4 of 7 2025-26 polls) remain flagged as estimates — source articles do not disclose them
- [pending] 2022 cycle polls (9 polls)
- **[done 2026-05-16]** 2018 cycle polls + 2018/2022 election results:
  - 2018-05-14 Gallup/VB: all percentages, seats, fielding dates, sample sizes verified exactly
  - 2018-05-25 Gallup/RÚV: verified; one cosmetic fix applied (Aðrir flokkar 3.9 → 3.8 to match source exactly)
  - 2018-05-08 Fréttablaðið: incompleteness confirmed intrinsic to source; B/F/J only inferable from "needs X% to win seat" framing — not added
  - 2018 election: all 16 party rows match Wikipedia exactly (58,966 valid votes, 23 seats)
  - 2022 election: all 11 party rows match Wikipedia exactly (59,949 valid votes, 23 seats)

A fourth agent is also running to find additional Gallup polls (inter-election
and earlier-in-cycle) — separate from verification.

---

## Maintenance

When adding a new poll:

1. Append the data row to the appropriate CSV.
2. Add a row to the matching table above with: date, pollster, n (or "est"), source URL, notes.
3. If `n` was disclosed in the source, record it without "(est)". Otherwise mark estimated.
4. Use the **end-of-fielding** date as the canonical poll date.
