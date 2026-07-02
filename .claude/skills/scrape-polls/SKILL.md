---
name: scrape-polls
description: Use this skill any time a new Icelandic political poll has been (or might have been) published and needs to land in the kosningaspá data set. Triggers strongly on "new poll", "Þjóðarpúls", "Gallup published", "Gallup poll", "Maskína dropped", "Maskína published", "scrape polls", "update polls", "update post-election polls", "check for new polls", "fill the polling gap", "kannanavakt update", "Fylgisvakt is stale". Use it even when the user only hints — "I think Gallup published this morning", "any new polls?", "is the data current?" — because the cost of running it on a quiet day is small and the cost of letting the data drift is high. Skill covers Gallup á Íslandi (Þjóðarpúls, RÚV) and Maskína (Vísir); for Prósent or Félagsvísindastofnun, hand off — those need separate scrapers and only matter close to the next election.
---

# scrape-polls

Pull newly-published Gallup and Maskína polls into the kosningaspá data set, with browser-mediated verification of the parts the article-body regex scraper gets wrong.

## When this skill applies

The user wants the post-election polling data refreshed. Cadence is monthly — Gallup publishes Þjóðarpúls around the start of the month (reporting the *previous* month's field period), Maskína publishes irregularly through the month. Run this skill whenever a new poll might be available — discovering "no new polls" in 30 seconds is fine; missing one for a month is not.

## Working directory assumption

All file paths are relative to the **kosningaspá repo root** (the directory containing `R/`, `Stan/`, `data/`). Run shell commands from there. If you're invoked from somewhere else, `cd` in first; if you're already inside a worktree of it, stay put.

## What this skill does — and doesn't

**Does:**
- Discovers new poll articles since the last tribble entry, primarily via RÚV's canonical poll list (a JSON object embedded in `ruv.is/kosningar/kannanir-a-landsvisu`).
- Verifies each candidate by reading the source article in Chrome — type-of-poll, field-period midpoint, sample size.
- Edits the `tribble()` in `R/scrape_polls.R::get_hardcoded_polls()` — the durable record.
- Regenerates `data/post_election_polls.csv` from the updated tribble.

**Does not:**
- Refit the polling-watch Stan model (`R/fit_polling_watch.R` — minutes-long; user decides when).
- Regenerate the metill-platform JSON (separate human step in `~/metill-platform`).
- Commit anything to git (user reviews the diff first).

Hand the next steps off to the user with a clean summary at the end.

## Source of truth (read this before editing anything)

The durable record of post-election polls is the `tribble()` literal inside `get_hardcoded_polls()` in `R/scrape_polls.R`. The CSV at `data/post_election_polls.csv` is **regenerated from that tribble each time the script runs**.

If you write only to the CSV, your changes vanish on the next run. Always edit the tribble. Treat the CSV as build output, not source.

## Where the data actually lives

Four reliable sources. Use whichever is freshest:

1. **RÚV's canonical poll list at `https://www.ruv.is/kosningar/kannanir-a-landsvisu`.** The page embeds a `window.pollsArray` JavaScript variable containing every poll RÚV has on file, with per-party `ratio` to 6 decimal places, the firm name, the canonical Icelandic title (e.g. "Þjóðarpúls Gallup mars 2026", "Maskína 23. mars 2026"), and a publication date. **The cleanest source when it's up to date.** Read it via `mcp__Claude_in_Chrome__javascript_tool` running `JSON.stringify(window.pollsArray)`.

2. **Gallup's own site at `https://www.gallup.is/frettir/` and the Þjóðarpúls PDF.** Gallup typically publishes the Þjóðarpúls on gallup.is several days before RÚV picks it up — in May 2026 the lag was ≥ 8 days. The news listing at `gallup.is/frettir/` shows the latest articles; each Þjóðarpúls article links through to a PDF viewer page (`gallup.is/<slug>/`) whose iframe `src` points at the actual PDF. **The PDF is authoritative for methodology** — it states the field period (e.g. "1. - 29. apríl 2026"), heildarúrtak, and response rate that the RÚV blob doesn't carry. Fetch it with `curl` and read it directly. There's also an internal Looker dashboard at `gallup.is/data/geytenbr/sso/` showing the time series, but its raw CSV download is gated and the embedded iframe is cross-origin; the news article + PDF path is more reliable.

   **PDF hosting paths (important):**
   - **May 2025 onward:** `cdnx.gallup.is/media/documents/Puls_MMYY_Fylgi_flokka.pdf` — deterministic and URL-guessable (`MMYY` = publication month, e.g. `0526` = May 2026 PDF reporting April 2026 field period).
   - **Pre-May 2025:** `gallup.is/documents/<opaque_id>/Puls_MMYY_Fylgi_flokka.pdf` — `<opaque_id>` is not enumerable; find it via the iframe `src` on the PDF viewer page, don't try to guess. For pre-May-2025 polls where this PDF can't be located, the next-month PDF's "comparison" column provides secondary confirmation.

3. **Maskína's own site at `https://maskina.is/frettir/` and the Fylgi-Althingi PDF report.** Maskína publishes a PDF report for every **regular monthly Alþingi reading** at `maskina.is/wp-content/uploads/YYYY/MM/YYYY-MM-Fylgi-Althingi_Maskinuskyrsla.pdf` (sometimes with `-DD` date suffix when multiple). This is the analogue of Gallup's PDF and the authoritative source for `svarendur` and exact field period. Discover the URL via the news listing at `maskina.is/frettir/` (each monthly poll gets its own news article whose body links to the PDF — paginate via `/frettir/page/N/`) or via `WebSearch site:maskina.is "<month> <year>"`.

   **Important: regular monthly vs commissioned side-publications.** Not every poll that appears under the "Maskína" brand goes through this channel. Maskína occasionally publishes commissioned polls for partner outlets — Vísir, Sýn (Stöð 2), Sýn's Kryddsíldarþáttur, etc. — that **don't** get their own Maskína-authored article or PDF, only the partner outlet's write-up. Examples: the 2025-11-10 reading was for Vísir; the 2026-02-24 was for Sýn. Both appear on the dashboard as monthly entries but Maskína never published a self-authored article or PDF for them. If `WebSearch site:maskina.is` returns nothing for a known poll date, assume it's a commissioned side-publication and accept the downstream article (source #4) as the canonical record.

   There's also a public dashboard at [`maskina.is/fylgi-flokka-a-althingi/`](https://maskina.is/fylgi-flokka-a-althingi/) — a Tableau Public viz with five tabs covering every Maskína poll back to Nov 2021, including both regular monthly readings and commissioned ones. **Not programmatically scrapable** (Tableau renders to canvas, dropdown clicks go to canvas event handlers, no `.csv` endpoint, URL parameters for `Veldu mælingu` don't take effect because it's a filter on a data field rather than a parameter control). Useful for manual verification — switch the "Veldu mælingu" dropdown to a specific month to see `svarendur` in the "Bakgrunnur" panel of "Nýjasta mæling".

4. **The downstream article on RÚV (`ruv.is/frettir/innlent/...`), Vísir (`visir.is/g/...`), or mbl.is (`mbl.is/frettir/innlent/...`).** Useful when the firm's own publication is delayed or you only have an indirect lead. Article bodies sometimes disclose `svarendur` and field period directly; when paywalled, per-party percentages can still be cross-checked via search snippets.

The R scraper (`scrape_polls.R::scrape_gallup` / `scrape_maskina`) historically did article-body regex against the tag pages on RÚV and Vísir. **Both tag pages now lazy-load via JavaScript and `rvest` sees nothing**, so the scraper silently returns empty results. Don't rely on it for discovery; treat it as a deprecated fallback.

## Procedure

### 1. Find the gap

Read `R/scrape_polls.R` and note the latest dated entry per firm in `get_hardcoded_polls()`. Quick way:

```bash
grep -E '^\s+"[0-9]{4}-' R/scrape_polls.R | awk -F'"' '{printf "%s  %s\n",$2,$4}' | sort
```

That's the lower bound for what to look for.

### 2. Pull the canonical poll list from RÚV

In Chrome, navigate to `https://www.ruv.is/kosningar/kannanir-a-landsvisu`. The page hydrates `window.pollsArray` after a brief delay (~2 seconds), so wait before reading or you'll get an empty array. Note that top-level `await` is not allowed in the eval context — use a promise:

```javascript
new Promise(r => setTimeout(() => r(JSON.stringify(window.pollsArray)), 2500))
```

Use `mcp__Claude_in_Chrome__javascript_tool`. This returns a clean JSON list of every poll RÚV has on file (~50 entries), each with `identifier`, `text`, `shorttext`, `type`, `date`, `ispoll`, `calculator`, plus per-party `ratio` to 6 decimal places.

Filter to entries dated *after* the last tribble entry per firm, and where the firm is **Gallup** or **Maskína** (skip Prósent and Félagsvísindastofnun).

If nothing's newer than the existing tribble in `pollsArray`, **don't immediately report "no new polls"** — RÚV can lag Gallup's own publication. Also check `gallup.is/frettir/` for a Þjóðarpúls article newer than the last tribble entry (the news listing is server-rendered, so `get_page_text` works); if one exists, follow its "Skoða PDF" link to find the PDF URL embedded in an iframe, then fetch and read the PDF for the values and methodology. Skip the gallup.is check if the user has confirmed nothing's been published.

### 3. Verify each candidate against its source article

The JSON entries don't include the article URL. To find each article:

- **Gallup:** browse `https://www.ruv.is/frettir/tag/thjodarpuls-gallup` in Chrome (this also lazy-loads — wait ~2 seconds before reading the page text). The article whose headline matches the JSON entry's `text` field is the one you want.
- **Maskína:** browse `https://www.visir.is/t/2296` similarly. Headlines match the JSON entry's `shorttext`.

Once you've found the article, navigate to it and confirm four things from the body:

| What | Where to look | Why |
|---|---|---|
| Type of poll | Headline + lede | Skip Reykjavík/Akureyri/Hafnarfjörður municipal polls, single-issue polls, leader-favourability polls — only national party support feeds the model. National polls always say "á landsvísu" or list multiple national parties; municipal ones name the city or a mayoral candidate |
| Collection dates | "Könnunin var framkvæmd dagana X til Y" / "fór fram dagana X.–Y." / "kannað dagana X til Y" | Take the **midpoint** as the date. Publication date is wrong; field-period midpoint is what the model expects |
| Sample size | See "Sample-size convention" below | Different convention per firm |
| Per-party percentages | Confirm article body matches the JSON blob | Cross-check is cheap and catches the rare cases where RÚV's canonical list is mid-update or wrong |

### 4. Sample-size convention (matches the existing tribble — important)

The existing rows in `get_hardcoded_polls()` use **different conventions per firm**, and you must match them so the column stays meaningful:

- **Gallup:** use `heildarúrtak` (gross sample, e.g. `9958`, `10000`, `10887`). The methodology line in the article reports both gross sample and response rate; take the gross sample. If the article omits methodology entirely (recap articles, mid-month follow-ups), default to **`10000`** — it's the median of the recent rows and the model isn't sensitive to ±5% on the contacted count.
- **Maskína:** use `svarendur` (response count, e.g. `2617`, `1786`, `886`). The Maskína article body reports the number of people who took a position on a party — that's the right column.

Do not switch conventions row-by-row, even if you think one is "more correct" — the entire prior data set sets the convention and the polling-watch model was fit against it.

### 5. Field-period midpoint rules

| Situation | Use this date |
|---|---|
| Single field period like "1.–14. apríl 2026" | Linear midpoint (April 7) |
| Field period spans two months ("28 March – 11 April") | Linear midpoint (April 4) — not the start, not the publication date |
| Composite Maskína poll pooling 2–3 sub-surveys | Linear midpoint of [first sub-period start, last sub-period end]. Don't average the per-sub-period midpoints — the existing rows use the span midpoint |

### 6. Update the tribble

Open `R/scrape_polls.R` and find `get_hardcoded_polls()`. Insert verified rows. The columns are:

```
~date,         ~fyrirtaeki, ~n_total,  ~S,    ~D,    ~B,    ~C,    ~M,    ~F,    ~P,    ~V,    ~J,
```

Match the existing column alignment when adding rows — R doesn't care about whitespace, but humans do, and `git diff` reads much cleaner when columns line up. Look at the surrounding rows for the spacing pattern.

Two structural notes:
- The existing tribble groups Gallup and Maskína into separate blocks. Add to the appropriate block; don't interleave.
- If a party isn't reported in the article (Maskína sometimes omits parties below 2%), use the value the JSON blob gives — `Other` is computed automatically as `100 − sum(named)`, so a literal `J = 0.0` for "didn't report" is fine.
- Don't enter "Annað" / "Other" explicitly; the script appends a remainder row.

### 7. Update `data_sources.md`

The provenance audit trail at `data_sources.md` (repo root) is the durable record of *where each tribble row came from*. It must stay in sync with the tribble — every row you added or changed in step 6 needs a matching row appended to the relevant pollster's table in `data_sources.md`.

Row format:

```markdown
| Date | n_total | Source | Notes |
|---|---|---|---|
| 2026-04-15 | 10,484 | [PDF](https://www.gallup.is/documents/2237/Puls_0526_Fylgi_flokka.pdf) ([RÚV](https://www.ruv.is/frettir/innlent/2026-05-08-thjodarpuls-...)) | Fielded 1.–29. apríl 2026; úrtak 10.484, svh 43% |
```

Conventions for the row:

- **Date**: same as the tribble (midpoint of fielding, YYYY-MM-DD).
- **n_total**: same as the tribble, with comma thousands separator (e.g. `10,484`, not `10484`).
- **Source**: primary URL first, secondary in parentheses if available.
  - Gallup → `[PDF](gallup.is/documents/...)` first, `[RÚV](ruv.is/frettir/innlent/...)` second
  - Maskína → `[Vísir](visir.is/g/...)`
  - Use `_not found_` only as a last resort
- **Notes**: terse, Icelandic field-period phrasing.
  - Gallup template: `Fielded X.–Y. mánuður YYYY; úrtak N, svh P%`
  - Maskína template: `Fielded X.–Y. mánuður YYYY; svarendur N`
  - If `n_total` was defaulted or estimated (e.g. Gallup `10000` placeholder), flag it: `n estimated — methodology not disclosed`

If you applied any **corrections** in step 3 (tribble date or n_total disagreed with the source), record the correction in the Notes column with **bold** prefix (`**CORRECTION: date was 2026-03-16, source says 2026-03-17**`) and add a one-line entry to the *Verification log* section at the bottom of `data_sources.md`.

If `data_sources.md` doesn't exist (someone deleted it, fresh clone), regenerate the skeleton from the existing tribble — headers, the Conventions section, and a Verification log entry noting the rebuild. The other historical rows can be backfilled later via parallel agents; what matters most is that **every new poll lands in the tribble with provenance recorded in the same commit**.

### 8. Regenerate the CSV

```bash
Rscript -e 'source("R/scrape_polls.R"); update_post_election_polls(scrape = FALSE)'
```

Note `scrape = FALSE` — we already have the verified values; we only want the tribble → CSV transformation. Confirm the new rows show up at the tail of `data/post_election_polls.csv` (one row per party per poll).

### 9. Hand off cleanly

End with a structured summary so the user knows what changed and what's still pending:

```
## Polls added
- YYYY-MM-DD Gallup   (n=NNNN) — S ##.# / D ##.# / B ##.# / C ##.# / M ##.# / F ##.# / P ##.# / V ##.# / J ##.#
- YYYY-MM-DD Maskína  (n=NNNN) — …

## Notes
- (only list anything where you had to override or interpret — defaulted n_total, composite field period, JSON vs article body discrepancy, etc.)

## Hand-off (run when ready)
1. Refit polling watch:   Rscript R/fit_polling_watch.R
2. Regenerate platform:   cd ~/metill-platform && uv run python scripts/regenerate_kosningaspa.py
3. Review git diff and commit
```

Don't run the hand-off commands. Stan fits take minutes; the user picks the moment.

## Edge cases & gotchas

- **Vísir tag stream is dominated by municipal polls during the May 2026 council election cycle.** Of ~30 visible articles on `visir.is/t/2296`, only a handful are national Alþingi readings. The differentiator is reliable: national polls say "á landsvísu" or list multiple parties; municipal polls name a city or a mayoral candidate.
- **Cross-poll articles.** When a Gallup article references the previous month's Maskína numbers (or vice versa), the article's "headline" percentages and the "comparison" percentages may both appear. The JSON blob isolates each poll cleanly, but if you fall back to article-body parsing, watch for the trap.
- **N+1 month publication.** "Þjóðarpúls Gallup febrúar 2026" is published in early March and reports a 2 Feb – 1 Mar field period. The existing tribble uses *previous-month-15th* as a stand-in date (e.g., the Jan reading is `2026-01-15`). Match that pattern: midpoint of February → `2026-02-15`, not `2026-03-03`.
- **Article structure changes.** RÚV and Vísir restructure their CMS occasionally. If the JSON blob at `kannanir-a-landsvisu` ever stops being a `window` variable (or gets renamed), check the page's `<script>` blocks for any inline JSON before going to article-body parsing.
- **RÚV lags Gallup's own publication.** Gallup publishes the Þjóðarpúls PDF on gallup.is on the day of release; RÚV's pollsArray and tag page can be a week or more behind. If a user mentions a Gallup poll that isn't in `pollsArray`, check `gallup.is/frettir/` before concluding the user is mistaken.
- **Gallup PDF URL pattern.** `gallup.is/documents/<id>/Puls_MMYY_Fylgi_flokka.pdf` where `MMYY` is the *publication* month (May 2026 → `0526`), reporting the previous month's field period. The `<id>` is opaque (e.g. `2237` for May 2026) — find it by reading the iframe `src` on the PDF viewer page, don't try to guess it.
- **Polls before the last election.** Don't add anything dated before 2024-11-30. Pre-election polls live in a Google Sheet and feed a different model.
- **The deprecated R scraper.** `Rscript -e 'source("R/scrape_polls.R"); update_post_election_polls(scrape = TRUE)'` will report "No new Gallup articles found" and may silently drop Maskína nationals — that's a known rvest-vs-JS-lazy-load issue, not a real "no polls". Trust the `pollsArray` JSON, not the scraper.

## Tooling cheat sheet

For the verification step, the most useful Chrome MCP calls are:

- `mcp__Claude_in_Chrome__tabs_context_mcp` — list available tabs (call once at the start)
- `mcp__Claude_in_Chrome__navigate` — go to an article
- `mcp__Claude_in_Chrome__get_page_text` — grab the body for reading
- `mcp__Claude_in_Chrome__javascript_tool` — read `window.pollsArray` from RÚV (use the `setTimeout` wrap shown in Step 2; the variable is empty on first paint)
- `mcp__Claude_in_Chrome__browser_batch` — bundle navigate + wait + read into one round-trip when you have a sequence

`browser_batch` is the right tool when you're verifying several articles in a row — it cuts the latency of each individual MCP call.

**Don't reach for `WebFetch` on ruv.is article pages** — they're rendered client-side and `WebFetch` returns mostly empty HTML. Vísir articles are server-rendered and `WebFetch` works on them, but for consistency and to handle the lazy-loaded tag pages, Chrome MCP is the right hammer for this whole skill.

## Reference: command quick list

| Goal | Command (run from kosningaspá repo root) |
|---|---|
| List current tribble dates | `grep -E '^\s+"[0-9]{4}-' R/scrape_polls.R \| awk -F'"' '{printf "%s  %s\n",$2,$4}' \| sort` |
| Regenerate CSV from tribble | `Rscript -e 'source("R/scrape_polls.R"); update_post_election_polls(scrape = FALSE)'` |
| Refit polling-watch model (hand-off, don't run) | `Rscript R/fit_polling_watch.R` |
| Regenerate platform JSON (hand-off, don't run) | `cd ~/metill-platform && uv run python scripts/regenerate_kosningaspa.py` |
