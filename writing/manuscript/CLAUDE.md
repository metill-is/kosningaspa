# writing/manuscript — Methodological Manuscript

Self-contained Quarto `manuscript` project documenting the joint fundamentals-and-polling dynamic GLM for the Icelandic parliamentary election forecast.

Authors: Brynjólfur Gauti Guðrúnar Jónsson, Rafael Daniel Vias, Hafsteinn Einarsson, Agnar Freyr Helgason.

## Source files

- `index.qmd` — main article (English)
- `adferdir.qmd` — methods notebook
- `umlikanid.qmd` — plain-language explainer (Icelandic, general audience)
- `references.bib` — shared bibliography for all writing outputs

## Rendering

From the repo root:

```bash
cd writing/manuscript && quarto render
```

Output goes to `writing/manuscript/docs/` in HTML, PDF (via lualatex), and DOCX. The output directory is regenerated on each render — do not place hand-edited files inside `docs/`. `execute: freeze: true` means R code cells are not re-run unless you pass `--execute` or delete `_freeze/`.

## Shared assets

The manuscript references repo-root assets with `../../` paths:

- `../../Stan/polling_and_fundamentals_kjordaemi.stan` — code-link in rendered HTML
- `../../Figures/...` — generated plots (when added; currently the manuscript has no figures)
- `../../R/...` — R modules invoked from .qmd code chunks (currently none)

## Notes

- The `manuscript` project type cleans `docs/` before each render. Anything you need to preserve must live outside `docs/`.
- `_freeze/` is the execution cache. Safe to delete — Quarto will re-run R chunks and rebuild it. Currently the manuscript has no R chunks, so `_freeze/` mostly holds Pandoc-stage caches.
- `site_libs/` and `_freeze/site_libs/` are shared web assets (Bootstrap, htmlwidgets, etc.). Regenerated on render.
