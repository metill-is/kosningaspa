# Analysis behind chapter 1.4 (4 Nov 2026)

Scripts that computed the worked example and the folding table in `forecast/seats.html`. Both read only committed files: `harness/truth/elections_official.csv` through `R/harness/truth.R` `read_truth()`, and the allocator in `R/election_utils.R`. Run them from the repository root with `LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/1.4-seats/<script>.R`.

| Script | Gives |
|---|---|
| `d_2021.R` | the 2021 national shares; the 54 kjördæmi seats; Norðvestur's D'Hondt quotients (halves rounded up); the nine adjustment seats in order, with each one's national quotient, divisor, kjördæmi, share quotient and the kjördæmi skipped because they were full; the ninth seat's share quotients in every kjördæmi; the next quotients after the ninth. It stops unless its trace equals `calculate_seats()` and the official seats in all 60 cells. |
| `d_fold.R` | for each election 2016–2024: Annað's share, its official seats, and the seats that move when the official votes are folded to poll parties and allocated with Annað ineligible; where seats move (2016), each cell that changes and the adjustment seats by list |
