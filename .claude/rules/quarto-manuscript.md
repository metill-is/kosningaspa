---
paths:
  - "*.qmd"
  - "docs/**"
  - "_quarto.yml"
---

# Quarto Manuscript Conventions

- Project type is `manuscript` (academic paper format).
- `execute: freeze: true` means R code is NOT re-run on `quarto render` unless unfrozen.
- To force re-execution of a specific document: `quarto render index.qmd --execute`
- Output goes to `docs/` in HTML, PDF, and DOCX formats.
- The `_freeze/` directory is gitignored — do not check in execution cache.
- Cross-references use Quarto syntax: `@fig-name`, `@tbl-name`, `@eq-name`.
