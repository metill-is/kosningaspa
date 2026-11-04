# Analysis behind chapter 1.2 (not yet written)

Chapter 1.2 is paused until the fundamentals lag below is fixed. Run the scripts from the repository root with `LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/1.2-fundamentals/<script>.R`.

| Script | Reads | Gives |
|---|---|---|
| `d_align.R` | the live `data/fundamentals_data.csv` and `data/economy_data.csv` (git-ignored), through `prepare_fundamentals_data()` | the year each fundamentals matrix column holds and, for each training election, the lists its likelihood reads, the lists that ran but are left out (with their share of the votes) and the lists read with 0 votes; once for the CSV as it stands and once with the 2024 results blanked, the layout the 2024 forecast read |

`d_align.R` was run on 4 Nov 2026. It shows that `index_f` and `n_parties_f` keep the first election's column while `y_f`, `incumbent_f`, `incumbent_years`, `vnv` and `growth` drop it, so the model's training column d reads the lists of the election before the one it scores. With the CSV as it stands, which holds the 2024 results, `incumbent_years`, `vnv` and `growth` have D_f columns, not the D_f + 1 the model declares, so a fit needs the next election's rows first.
