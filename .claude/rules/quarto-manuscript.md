---
paths:
  - "writing/**"
---

# Writing Outputs (Quarto)

- To add a new writing output (book chapter, conference paper, etc.), create `writing/<name>/` with its own `_quarto.yml`. Each output renders independently and can target different formats (docx, pdf, html). Reference shared assets with `../../` paths (e.g. `bibliography: ../manuscript/references.bib` or `../../Figures/foo.png`).
- The `_freeze/` directory is gitignored — do not check in execution cache.
- Cross-references use Quarto syntax: `@fig-name`, `@tbl-name`, `@eq-name`.
