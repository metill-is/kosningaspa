# docs — Rendered Manuscript Output

This directory contains Quarto-rendered output (HTML, PDF, DOCX). Do not edit files here directly — they are generated from the `.qmd` files in the project root.

## Rendering

```bash
quarto render
```

The project uses `execute: freeze: true`, so R code cells are not re-executed unless explicitly unfrozen. The `_freeze/` directory (gitignored) stores cached execution results.

## Project Type

Quarto `manuscript` format (academic paper). Source files:
- `index.qmd` — Main manuscript
- `adferdir.qmd` — Methods/approach section
- `umlikanid.qmd` — Model description section

Output formats: HTML, PDF (via LaTeX), DOCX.
