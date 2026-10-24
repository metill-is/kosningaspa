# Official results 2016–2024

`elections_official.csv` holds votes and seats per election × kjördæmi × list (one row per list that ran anywhere, 0 votes and `ran = FALSE` where it did not run), with `flokkur` the poll party of the list letter. Rebuild with `write_truth()` (`R/harness/truth.R`); the rebuild is byte-identical. All sources are public results data.

## Sources

| Election | Votes | Seats |
|---|---|---|
| 2016-10-29 | Hagstofa KOS02118b from the PX API: `harness/sources/hagstofa/KOS02118b_2016.json` | `thingsaeti_2016.xlsx` (KOS02118b) in the snapshot |
| 2017-10-28 | `gild_atkvaedi_2017.xlsx` (KOS02118a) | `thingsaeti_2017.xlsx` (KOS02118a) |
| 2021-09-25 | `gild_atkvaedi_2021.xlsx` (KOS02115, now KOS02019) | Alþingi's results page: `harness/sources/seats_2021_official.csv` |
| 2024-11-30 | Hagstofa KOS02018 from the PX API: `harness/sources/hagstofa/KOS02018_2024.json` | `KOS02018_2024.json` |

- **2016 votes.** The snapshot's `gild_atkvaedi_2016.xlsx` is a truncated export of KOS02118b: it has no rows for S, T and V (44 336 votes). The complete table was fetched once, on 2026-10-23, by `POST https://px.hagstofa.is/pxis/api/v1/is/Ibuar/kosningar/althingi/althurslit/KOS02118b.px` with the body `{"query":[{"code":"Atriði","selection":{"filter":"item","values":["0","2"]}}],"response":{"format":"json-stat2"}}` (Atriði 0 = gild atkvæði, 2 = þingsæti; every list and kjördæmi), and stored unchanged (sha256 `529bc35475cb5a8dc06abb11d4a358f16222c39f09daa97c8def2c0218ca84ce`). `build_truth()` stops unless each of the 54 cells the xlsx does hold equals the API table, and unless the þingsæti sheet equals the API's seats.
- **2024.** `harness/sources/hagstofa/KOS02018_2024.json` is Hagstofa's table KOS02018 ("Úrslit alþingiskosninga eftir kjördæmum 2024"): valid votes and seats per list × kjördæmi, in JSON-stat as the PX API returns it, fetched in October 2026 and stored unchanged (sha256 `18a6e72e28d1c997326bdc2e08d85f210fcae4ede60ba98b6efe8bfaa6ff8644`). `official_2024_kjordaemi.csv` beside it (kjordaemi, flokkur, votes, seats_official; sha256 `d9a8f8bfaeb229338bb989f3ac59aed68c550dd9c097e0c62b43077469472e19`) is a fold of the same table to the poll parties (Annað = L + Y), made before the harness; the reconciliation test checks `build_truth()`'s own parse against it. Both were snapshot files until 24 Oct 2026 and were moved here byte for byte.
- **2021 seats.** `harness/sources/althingi/althingiskosningar_2021.txt` is the text of <https://www.althingi.is/thingmenn/althingiskosningar/kosningar-og-kosningaurslit/kosningar-fra-1919/althingiskosningar-2021/>, captured on 2026-10-23 in a browser (the file's header records why: althingi.is answers scripted fetches with HTTP 403) and committed unchanged. It lists the 63 members elected after the Norðvestur recount, by kjördæmi, party and seat type. `harness/sources/althingi/seats_2021_official.R` counts them into `seats_2021_official.csv` (seats and adjustment seats per list × kjördæmi) and writes nothing unless the counts reproduce the page's own national totals (D 16, B 13, V 8, S 6, F 6, P 6, C 5, M 3), its adjustment-seat summary (P 3, C 2, V 2, M 1, S 1) and its kjördæmi totals.

## Letters

Letters come from the list labels, `Name (L)`, never from codes. B, C, D, F, J, M, P, S and V map to their poll parties (`LETTER_PARTY`); every other list is Annað: Björt framtíð (A, 2016/17), E, H, R and T (2016), R and T (2017), Hagstofa's letterless 2021 row "Aðrir" (kept as list letter `Aðrir`; it aggregates Frjálslyndi lýðræðisflokkurinn and Ábyrg framtíð, 989 votes), and L and Y (2024). In the JSON-stat tables the Flokkur and Kjördæmi "Alls" rows are totals: they are checked against the cells and dropped.

## Reconciliation (tests/harness/test-truth.R)

- Valid votes 189 648, 196 259, 199 730 and 212 470; 63 seats each year.
- Folded to poll parties, 2016–2021 equal the constituency `Kosning` rows in all 180 cells, and 2024 equals `harness/sources/hagstofa/official_2024_kjordaemi.csv` in all 60 cells, votes and seats.
- `calculate_seats()` on unfolded lists, with no ineligible list, reproduces every official seat 2016–2024 (Björt framtíð's 4 in 2016 included), and in 2021 it places each adjustment seat where the Alþingi page does.

## Hagstofa's 2021 seat tables (documented, not used)

Both disagree with Alþingi's page, and with D'Hondt on their own vote counts. Seats in the six cells where any of the three differ:

| List × kjördæmi | `thingsaeti_2021.xlsx` (KOS02115, updated 2022-12-21) | KOS02019 (PX API, read 2026-10-23) | Alþingi |
|---|---|---|---|
| P Norðaustur | 1 | 0 | 0 |
| S Norðaustur | 0 | 1 | 1 |
| S Reykjavík Suður | 2 | 2 | 1 |
| S Suður | 0 | 0 | 1 |
| V Reykjavík Suður | 1 | 1 | 2 |
| V Suður | 1 | 1 | 0 |

The xlsx gives P 7 and S 5 nationally; KOS02019 gives P 6 and S 6 but swaps S and V between Suður and Reykjavík Suður. Both give S no seat in Suður, although S wins the 8th of Suður's 9 constituency seats by D'Hondt on Hagstofa's own votes (2 270, against M 2 207 and V 2 200), and the xlsx also gives S none in Norðaustur, where S wins the 5th constituency seat (2 465). A constituency seat cannot be lost in the adjustment step, so neither table can be the result.
