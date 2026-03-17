# madagascar-rural-observatories-2025

Descriptive statistics report for the 2025 rural observatories campaign in Madagascar, covering the **Alaotra** and **Marovoay** observatories. The report is produced as a [Quarto Book](https://quarto.org/docs/books/) and rendered in multiple formats (HTML, PDF, DOCX) with three profile variants: a consolidated report combining both observatories, and one standalone report per observatory.

## Purpose

The Rural Observatories (*Observatoires Ruraux*) are a longitudinal household survey programme established in Madagascar in 1995. This repository contains the analytical report for the 2025 campaign, which re-surveys households in two rice-growing zones — Alaotra (Alaotra-Mangoro region) and Marovoay (Boeny region) — as part of the BETSAKA project studying the long-term impacts of conservation policies on rural livelihoods.

The report covers: survey description, household demographics, housing and living standards, land tenure, agriculture, food security, environment, disasters, project participation, data quality and survey representativeness.

## Repository structure

```
.
├── _quarto.yaml                  # Main Quarto config (shared across profiles)
├── _quarto-consolidated.yml      # Consolidated profile config (HTML + PDF + DOCX)
├── _quarto-marovoay.yml          # Marovoay profile config (PDF + DOCX)
├── _quarto-alaotra.yml           # Alaotra profile config (PDF + DOCX)
├── _common.R                     # Executed before every chapter (profile detection, gt fixes)
├── index.qmd                     # Book introduction
├── 01_description.qmd            # Ch 1: Survey description
├── 02_structure_menages.qmd      # Ch 2: Household demographics
├── 03_habitat_niveau_de_vie.qmd  # Ch 3: Housing and living standards
├── 04_foncier.qmd                # Ch 4: Land tenure
├── 05_agriculture.qmd            # Ch 5: Agriculture
├── 06_securite_alimentaire.qmd   # Ch 6: Food security
├── 07_rapport_environnement.qmd  # Ch 7: Environment
├── 08_cataclysme_catastrophe.qmd # Ch 8: Disasters
├── 09_insertion_projet.qmd       # Ch 9: Project participation
├── 10_enquete_qualite.qmd        # Ch 10: Data quality
├── 11_representativite_ponderation.qmd  # Ch 11: Representativeness
├── 12_bibliographie.qmd          # Ch 12: Bibliography
├── render_all.R                  # Script to build all report variants
├── references.bib                # Bibliography
├── utils/                        # R helper functions
│   ├── helpers_report.R          # Classification, tables, plots, gt styling
│   ├── report_variant.R          # Profile detection and data filtering
│   ├── sites.R                   # Site/hamlet definitions per observatory
│   ├── downloadable_output.R     # Site-level downloadable XLSX/PNG generation
│   ├── calc_incomes_all_years.R  # Multi-year income computation
│   └── ...
├── data/                         # Survey microdata (Stata .dta) and derived datasets
│   └── ROS_MDG_microdata/        # Raw data organised by year
├── output/                       # Cached intermediate results (trends, etc.)
├── styles/                       # CSS for HTML output
│   ├── observatory-sections.css  # Colour-coded observatory boxes
│   └── downloads.css             # Per-site download dropdown styling
├── images/                       # Static images (cover, maps)
├── docs/                         # Consolidated HTML output
├── docs-marovoay/                # Marovoay profile output
└── docs-alaotra/                 # Alaotra profile output
```

## Quarto profile system

The project uses [Quarto profiles](https://quarto.org/docs/projects/profiles.html) to produce three variants of the report from the **same set of `.qmd` source files**:

| Profile | Config file | Formats | Output directory |
|---|---|---|---|
| `consolidated` (default) | `_quarto-consolidated.yml` | HTML + PDF + DOCX | `docs/` |
| `marovoay` | `_quarto-marovoay.yml` | PDF + DOCX | `docs-marovoay/` |
| `alaotra` | `_quarto-alaotra.yml` | PDF + DOCX | `docs-alaotra/` |

The profile is declared in `_quarto.yaml` under `profile.group`, which tells Quarto that `consolidated`, `marovoay` and `alaotra` are mutually exclusive. The default profile is `consolidated`.

### How profiles affect content

Two mechanisms adapt content to each profile:

1. **Data filtering in R.** The function `filter_for_profile()` (in `utils/report_variant.R`) reads the `QUARTO_PROFILE` environment variable set by Quarto at render time. In consolidated mode it keeps all data; in observatory-specific mode it filters to the relevant observatory. This is called early in each chapter's setup chunk.

2. **Conditional content divs.** Quarto's `content-hidden`/`content-visible` directives show or hide narrative paragraphs based on the active profile. Three CSS classes style these blocks in the HTML output:
   - `.obs-alaotra` — blue left border (Alaotra-specific text)
   - `.obs-marovoay` — orange left border (Marovoay-specific text)
   - `.obs-technical` — grey left border (consolidated-only technical notes)

   Example pattern used throughout the chapters:

   ```markdown
   ::: {.content-hidden when-profile="marovoay"}
   ::: {.obs-alaotra}
   **Alaotra.** Observatory-specific commentary with inline R values.
   :::
   :::
   ```

### Profile-aware helper functions

The helper functions in `utils/helpers_report.R` automatically adapt to the number of observatories present in the data:

- `obs_gt()` — creates a `gt` table grouped by Observatory when consolidated, or drops the Observatory column when single-observatory.
- `obs_title()` — appends "par observatoire" or the observatory name to titles.
- `obs_facet()` — adds `facet_wrap(~Observatory)` only when multiple observatories are present.
- `safe_cols_label()` / `safe_tab_spanner()` — silently skip missing columns that can occur when pivoting single-observatory data.

## Rendering

### Render everything

The recommended way to build all outputs is:

```r
source("render_all.R")
```

This script:

1. Runs `quarto render` (consolidated profile) -> HTML + PDF + DOCX in `docs/`
2. Runs `quarto render --profile marovoay` -> PDF + DOCX in `docs-marovoay/`
3. Runs `quarto render --profile alaotra` -> PDF + DOCX in `docs-alaotra/`
4. Copies the Marovoay and Alaotra PDF/DOCX files into `docs/downloads/` so that the consolidated HTML sidebar can link to them

**Total: 3 code executions for 7 output files.** Within each `quarto render` call, each `.qmd` chapter's R code executes only once — Quarto then produces all the formats for that profile from the intermediate `.md`. The three separate calls are necessary because each profile filters different data.

### Render a single profile

```bash
quarto render                        # Consolidated (default)
quarto render --profile marovoay     # Marovoay only
quarto render --profile alaotra      # Alaotra only
```

### Render a single chapter (for development)

```bash
quarto render 04_foncier.qmd                        # Consolidated
quarto render 04_foncier.qmd --profile marovoay      # Marovoay
```

## Downloadable site-level outputs

In observatory-specific HTML reports, each table and figure is accompanied by a download dropdown (📥 *Télécharger par site*) that offers pre-computed XLSX (tables) and PNG (figures) files broken down by hamlet/site. This system is implemented in `utils/downloadable_output.R`:

- `dl_tbl_variants()` generates one XLSX file per site combination and emits an HTML `<details>` dropdown.
- `dl_fig_variants()` does the same with PNG files.
- Small cells (count < 5) are suppressed for statistical confidentiality.
- These downloads are only generated for observatory-specific HTML renders, not for the consolidated profile or PDF/DOCX.

## Key R utilities

| File | Role |
|---|---|
| `utils/report_variant.R` | Profile detection (`get_report_mode()`) and data filtering (`filter_for_profile()`) |
| `utils/helpers_report.R` | Classification functions (`classify_education`, `classify_activity`, etc.), gt table styling, bar/trend plot helpers, confidentiality suppression, accent fixes for gt → LaTeX |
| `utils/sites.R` | Site/hamlet definitions per observatory, `assign_site()`, `get_site_combos()` |
| `utils/downloadable_output.R` | Site-level XLSX/PNG generation and HTML dropdown emitter |
| `utils/calc_incomes_all_years.R` | Multi-year income aggregation (1995–2025) |
| `_common.R` | Loaded before every chapter; sets up `REPORT_MODE` and registers the `knit_print.gt_tbl` method for proper gt rendering in PDF |

## Data

Survey microdata are stored in `data/ROS_MDG_microdata/{year}/` as Stata `.dta` files. Each year folder contains household-level (`res_deb`), individual-level (`res_m_a`), and thematic module files (housing, land, agriculture, etc.). These files are not tracked in git due to confidentiality constraints.

Derived datasets cached in `output/` (e.g. `demog_trends_obs.rds`, `inc_trends_obs.rds`) store pre-computed multi-year trends to avoid re-processing historical data on every render.

## Dependencies

The project requires:

- **Quarto** ≥ 1.4
- **R** ≥ 4.3 with packages: `tidyverse`, `haven`, `labelled`, `gt`, `scales`, `writexl`, `sf`, `ggplot2`, `purrr`, `rlang`, `knitr`
- **XeLaTeX** (for PDF output via `pdf-engine: xelatex`)

## License

See [LICENSE](LICENSE).
