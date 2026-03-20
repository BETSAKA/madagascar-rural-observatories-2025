# utils/helpers_report.R
# Shared helper functions for the ROR observatory report
# ----------------------------------------------------------

# --- Report variant utilities (profile detection & filtering) ---------------
# These were originally in utils/report_variant.R and are included here
# so that every chapter that sources helpers_report.R gets them automatically.

source("utils/report_variant.R")
source("utils/plot_theme.R")
source("utils/ror_plots.R")
REPORT_MODE <- get_report_mode()

# --- Profile-aware display helpers -----------------------------------------

#' Create a gt table with conditional Observatory grouping
#'
#' When data contains a single observatory (observatory-specific report),
#' the Observatory column is dropped and no grouping is applied.
#' When data contains multiple observatories (consolidated report),
#' `groupname_col = "Observatory"` is applied.
#' @param data Data frame (should contain an Observatory column)
#' @param ... Additional arguments passed to gt::gt()
obs_gt <- function(data, ...) {
  n_groups <- dplyr::n_distinct(data$Observatory)
  if (n_groups <= 1) {
    data |>
      dplyr::select(-dplyr::any_of("Observatory")) |>
      gt::gt(...)
  } else {
    gt::gt(data, groupname_col = "Observatory", ...)
  }
}

#' Adapt a table/figure title based on the active report profile
#'
#' In consolidated mode, appends "par observatoire" to the base title.
#' In observatory-specific mode, appends the observatory name.
#' @param base_title Character string (without "par observatoire")
#' @return Adapted title string
obs_title <- function(base_title) {
  mode <- get_report_mode()
  if (mode$is_consolidated) {
    paste(base_title, "par observatoire")
  } else {
    base_title
  }
}

#' Conditionally add facet_wrap(~Observatory) to a ggplot
#'
#' Adds faceting only when data has multiple observatories.
#' @param data The data used in the plot (to check n observatories)
#' @param ... Additional arguments passed to facet_wrap
obs_facet <- function(data, ...) {
  n_fct <- dplyr::n_distinct(data$Observatory)
  if (n_fct > 1) {
    ggplot2::facet_wrap(~Observatory, ...)
  } else {
    NULL
  }
}

#' Safely label columns in gt — silently skips columns that don't exist
#'
#' Drop-in replacement for gt::cols_label() when some columns may be absent
#' (e.g. after pivot_wider(names_from = Observatory) in a single-observatory
#' report). Labels for non-existent columns are simply ignored.
#' @param .data A gt object
#' @param ... Name-value pairs passed to cols_label
safe_cols_label <- function(.data, ...) {
  labs <- list(...)
  existing_cols <- colnames(.data[["_data"]])
  valid_labs <- labs[names(labs) %in% existing_cols]
  if (length(valid_labs) > 0) {
    gt::cols_label(.data, .list = valid_labs)
  } else {
    .data
  }
}

#' Safely add a tab_spanner — skips if no matching columns exist
#'
#' When an observatory-specific report filters out one observatory,
#' pivot_wider columns like "Marovoay_Homme" may not exist.
#' This wrapper silently skips the spanner rather than erroring.
#' @param .data A gt object
#' @param label Spanner label
#' @param columns Tidyselect expression for columns
safe_tab_spanner <- function(.data, label, columns) {
  tryCatch(
    gt::tab_spanner(.data, label = label, columns = {{ columns }}),
    error = function(e) .data
  )
}

# --- gt LaTeX accent fix ------------------------------------------------
# gt 1.x converts Unicode accents to LaTeX accent commands in as_latex().
# Two problems arise with XeLaTeX:
#   1) Acute accents are double-escaped: é → \\'e (\\=linebreak, not accent)
#   2) \oe ligature merges with following letters: \oeuvre → undefined command
# Fix: convert ALL gt accent commands back to UTF-8, which XeLaTeX handles
# natively via fontspec.

fix_gt_latex_accents <- function(latex_str) {
  # Double-escaped acute accents (gt bug): \\'e → é, \\'E → É
  latex_str <- gsub("\\\\'e", "\u00e9", latex_str, fixed = TRUE)
  latex_str <- gsub("\\\\'E", "\u00c9", latex_str, fixed = TRUE)
  # Grave accents: \`a → à, \`e → è, \`u → ù
  latex_str <- gsub("\\`a", "\u00e0", latex_str, fixed = TRUE)
  latex_str <- gsub("\\`e", "\u00e8", latex_str, fixed = TRUE)
  latex_str <- gsub("\\`u", "\u00f9", latex_str, fixed = TRUE)
  latex_str <- gsub("\\`A", "\u00c0", latex_str, fixed = TRUE)
  latex_str <- gsub("\\`E", "\u00c8", latex_str, fixed = TRUE)
  # Circumflex: \^a → â, \^e → ê, \^i → î, \^o → ô, \^u → û
  latex_str <- gsub("\\^a", "\u00e2", latex_str, fixed = TRUE)
  latex_str <- gsub("\\^e", "\u00ea", latex_str, fixed = TRUE)
  latex_str <- gsub("\\^i", "\u00ee", latex_str, fixed = TRUE)
  latex_str <- gsub("\\^o", "\u00f4", latex_str, fixed = TRUE)
  latex_str <- gsub("\\^u", "\u00fb", latex_str, fixed = TRUE)
  latex_str <- gsub("\\^A", "\u00c2", latex_str, fixed = TRUE)
  latex_str <- gsub("\\^E", "\u00ca", latex_str, fixed = TRUE)
  latex_str <- gsub("\\^I", "\u00ce", latex_str, fixed = TRUE)
  latex_str <- gsub("\\^O", "\u00d4", latex_str, fixed = TRUE)
  # Dieresis: \"e → ë, \"i → ï
  latex_str <- gsub("\\\"e", "\u00eb", latex_str, fixed = TRUE)
  latex_str <- gsub("\\\"i", "\u00ef", latex_str, fixed = TRUE)
  latex_str <- gsub("\\\"o", "\u00f6", latex_str, fixed = TRUE)
  latex_str <- gsub("\\\"u", "\u00fc", latex_str, fixed = TRUE)
  # Ligatures and cedilla
  latex_str <- gsub("\\oe", "\u0153", latex_str, fixed = TRUE)
  latex_str <- gsub("\\OE", "\u0152", latex_str, fixed = TRUE)
  latex_str <- gsub("\\c{c}", "\u00e7", latex_str, fixed = TRUE)
  latex_str <- gsub("\\c{C}", "\u00c7", latex_str, fixed = TRUE)
  # Remove \cellcolor commands to avoid colortbl/booktabs conflicts in PDF.
  # Cell colours are cosmetic; bold text + row groups provide structure.
  latex_str <- gsub("\\\\cellcolor\\[HTML\\]\\{[A-F0-9]+\\}", "", latex_str)
  # Strip NA row group headers generated by gt when groupname_col has NAs.
  # Pattern: \multicolumn{N}{l}{{\bfseries {NA}}} \\[2.5pt]\n\midrule\addlinespace[2.5pt]
  latex_str <- gsub(
    "\\\\multicolumn\\{\\d+\\}\\{l\\}\\{\\{\\\\bfseries \\{NA\\}\\}\\} \\\\\\\\\\[2\\.5pt\\]\\s*\n\\\\midrule\\\\addlinespace\\[2\\.5pt\\]",
    "",
    latex_str
  )
  # Escape bare % signs in cell content. gt outputs "82 %" but in LaTeX %
  # is a comment character, swallowing the rest of the line (including \\ and &).
  # This causes "Misplaced \noalign" errors because \midrule never sees \cr.
  # We escape any % not preceded by \ (i.e. not already \%).
  latex_str <- gsub("(?<!\\\\)%", "\\\\%", latex_str, perl = TRUE)
  # Restore structural end-of-line %  used by gt (e.g. \end{table}%)
  latex_str <- gsub("\\\\end\\{table\\}\\\\%", "\\\\end{table}%", latex_str)
  latex_str
}

knit_print.gt_tbl <- function(x, ...) {
  if (knitr::is_latex_output()) {
    latex_str <- as.character(gt::as_latex(x))
    latex_str <- fix_gt_latex_accents(latex_str)
    # Reduce gt default font size from 12pt to 9pt for better fit in PDF
    latex_str <- gsub(
      "\\\\fontsize\\{12\\.0pt\\}\\{14\\.0pt\\}",
      "\\\\fontsize{9.0pt}{11.0pt}",
      latex_str
    )
    return(knitr::asis_output(latex_str))
  }
  # HTML / DOCX: default gt rendering
  knitr::knit_print(gt::as_raw_html(x), ...)
}
registerS3method(
  "knit_print",
  "gt_tbl",
  knit_print.gt_tbl,
  envir = asNamespace("knitr")
)

#' Add Observatory labels from j0 codes
#' @param x Either a data frame (adds Observatory column) or a vector of j0 codes
add_obs <- function(x) {
  if (is.data.frame(x)) {
    x |>
      dplyr::mutate(
        Observatory = dplyr::case_when(
          as.character(j0) == "3" ~ "Marovoay",
          as.character(j0) == "21" ~ "Alaotra",
          TRUE ~ as.character(j0)
        )
      )
  } else {
    dplyr::case_when(
      as.character(x) == "3" ~ "Marovoay",
      as.character(x) == "21" ~ "Alaotra",
      TRUE ~ as.character(x)
    )
  }
}

#' Fix hamlet name spelling variants
fix_hameau <- function(j4) {
  dplyr::case_when(
    stringr::str_starts(stringr::str_to_lower(j4), "bepako") ~ "Bepako",
    stringr::str_starts(stringr::str_to_lower(j4), "ampijoroa") ~ "Ampijoroa",
    stringr::str_starts(stringr::str_to_lower(j4), "maroala") ~ "Maroala",
    stringr::str_starts(
      stringr::str_to_lower(j4),
      "madiromiongana"
    ) ~ "Madiromiongana",
    TRUE ~ j4
  )
}

# ----------------------------------------------------------
# Confidentiality suppression utilities
# ----------------------------------------------------------

#' Suppress cells with small counts for confidentiality
#'
#' Replaces count and associated percentage columns with NA where n < threshold.
#' @param data Data frame
#' @param n_col Name of the count column (default "n")
#' @param pct_cols Names of percentage columns to also suppress
#' @param threshold Minimum count to keep (default 5)
#' @return Data frame with small cells replaced by NA
suppress_small_counts <- function(
  data,
  n_col = "n",
  pct_cols = NULL,
  threshold = 5
) {
  if (!n_col %in% names(data)) {
    return(data)
  }
  mask <- !is.na(data[[n_col]]) & data[[n_col]] < threshold
  if (!any(mask)) {
    return(data)
  }
  data[[n_col]][mask] <- NA_real_
  # Auto-detect percentage columns if not specified
  if (is.null(pct_cols)) {
    pct_cols <- intersect(
      names(data),
      c("pct", "%", "Percent", "percent", "prop", "proportion")
    )
  }
  for (pc in pct_cols) {
    if (pc %in% names(data)) data[[pc]][mask] <- NA_real_
  }
  data
}

#' Remove rows with counts below threshold (for plot data)
#'
#' Use this to clean data before passing to ggplot so that small
#' categories do not appear as bars/segments.
#' @param data Data frame
#' @param n_col Name of the count column (default "n")
#' @param threshold Minimum count to keep (default 5)
suppress_in_plot_data <- function(data, n_col = "n", threshold = 5) {
  if (!n_col %in% names(data)) {
    return(data)
  }
  data[is.na(data[[n_col]]) | data[[n_col]] >= threshold, ]
}

#' Auto-suppress small cells in a data frame for XLSX export
#'
#' Scans for common count/percentage column patterns and suppresses.
#' Used as post-processing in dl_tbl_variants.
#' @param data Data frame (output of tbl_fn)
#' @param threshold Minimum count to keep (default 5)
suppress_for_export <- function(data, threshold = 5) {
  # Identify likely count columns
  count_cols <- intersect(
    names(data),
    c(
      "n",
      "N",
      "Effectif",
      "effectif",
      "n_oui",
      "n_non",
      "n_envoyeurs",
      "n_total",
      "nb",
      "Nb",
      "count",
      "Count"
    )
  )
  pct_cols <- intersect(
    names(data),
    c("pct", "%", "Percent", "percent", "prop", "proportion", "Pct", "pct_oui")
  )
  for (nc in count_cols) {
    mask <- !is.na(data[[nc]]) & data[[nc]] < threshold
    if (any(mask)) {
      data[[nc]][mask] <- NA_real_
      for (pc in pct_cols) {
        if (pc %in% names(data)) data[[pc]][mask] <- NA_real_
      }
    }
  }
  data
}

#' Standard GT table styling for observatory reports
style_table <- function(gt_obj) {
  gt_obj |>
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold"),
        gt::cell_fill(color = "#4682B4")
      ),
      locations = gt::cells_column_labels(columns = gt::everything())
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold"),
        gt::cell_fill(color = "#B0E0E6")
      ),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_source_note(
      source_note = gt::md("**Source : Enquête auprès des OR 2025**")
    )
}

#' Classify education level from s3a code
classify_education <- function(s3a, s2 = NULL, age = NULL) {
  niv <- dplyr::case_when(
    (s3a >= 1 & s3a <= 5) | s3a == 99 ~ "Primaire",
    s3a >= 6 & s3a <= 9 ~ "Secondaire 1er cycle",
    s3a >= 10 ~ "Secondaire 2\u00e8me cycle & sup.",
    TRUE ~ NA_character_
  )
  # When s2 (school attendance) is available, recode NA as "Non scolarisé"

  # for those who never attended school (s2==2) or are too young (age < 3)
  if (!is.null(s2)) {
    niv <- dplyr::case_when(
      !is.na(niv) ~ niv,
      s2 == 2 ~ "Non scolaris\u00e9",
      !is.null(age) & !is.na(age) & age < 3 ~ "Non scolaris\u00e9",
      TRUE ~ NA_character_
    )
  }
  niv
}

#' Classify literacy level from s1a or s1b code
#' @param x Numeric vector (1 = Oui, 2 = Avec effort, 3 = Non)
#' @return Ordered factor with three levels
classify_literacy <- function(x) {
  lbl <- dplyr::case_when(
    x == 1 ~ "Oui",
    x == 2 ~ "Avec effort",
    x == 3 ~ "Non",
    TRUE ~ NA_character_
  )
  factor(lbl, levels = c("Oui", "Avec effort", "Non"))
}

#' Classify marital status from m7 code
classify_marital <- function(m7) {
  dplyr::case_when(
    m7 == 1 ~ "Célibataire",
    m7 == 2 ~ "Marié(e) selon la tradition",
    m7 == 3 ~ "En concubinage",
    m7 == 4 ~ "Divorcé(e)",
    m7 == 5 ~ "Veuf(ve)",
    m7 %in% c(6, 7, 8) ~ "Marié(e) civil / religieux / polygame",
    TRUE ~ NA_character_
  )
}

#' Classify sex from m4 code
classify_sex <- function(m4) {
  dplyr::case_when(
    m4 == 1 ~ "Homme",
    m4 == 2 ~ "Femme",
    TRUE ~ NA_character_
  )
}

#' Classify main activity into broad sectoral categories
#'
#' Groups the detailed ROR activity code (a1) into 8 categories loosely
#' aligned with ISIC Rev.4 sections: primary sector (agriculture, livestock,
#' fishing, natural resources), secondary (crafts, construction, transport),
#' tertiary (commerce, services), plus inactive/student.
#' This grouping ensures a minimum cell size per observatory suitable
#' for publication while preserving meaningful distinctions.
classify_activity <- function(a1) {
  dplyr::case_when(
    a1 == 95 ~ "Cultivateur exploitant",
    a1 == 76 ~ "Ouvrier agricole",
    a1 %in% c(45, 38, 41, 42, 43, 35, 8) ~ "Commerce & Restauration",
    a1 %in% c(96, 98, 99, 6, 59, 48, 81, 97, 4, 5, 70) ~
      "Elevage, P\u00eache & Ress. nat.",
    a1 %in%
      c(
        80,
        63,
        47,
        51,
        82,
        91,
        7,
        88,
        1,
        30,
        31,
        52,
        64,
        58,
        74,
        28,
        69,
        16,
        54,
        73,
        11
      ) ~
      "Artisanat, BTP & Transport",
    a1 == 14 ~ "El\u00e8ve / Etudiant",
    is.na(a1) ~ NA_character_,
    TRUE ~ "Services & Autres"
  )
}

#' Classify age into groups
classify_age_group <- function(m5) {
  dplyr::case_when(
    m5 <= 10 ~ "0-10 ans",
    dplyr::between(m5, 11, 20) ~ "11-20 ans",
    dplyr::between(m5, 21, 30) ~ "21-30 ans",
    dplyr::between(m5, 31, 40) ~ "31-40 ans",
    dplyr::between(m5, 41, 50) ~ "41-50 ans",
    dplyr::between(m5, 51, 60) ~ "51-60 ans",
    dplyr::between(m5, 61, 70) ~ "61-70 ans",
    m5 >= 71 ~ "71 ans et plus",
    TRUE ~ NA_character_
  )
}

#' Classify origin from m3 code
classify_origin <- function(m3) {
  dplyr::case_when(
    m3 == 1 ~ "Tompon-tany",
    m3 == 2 ~ "Zana-tany",
    m3 == 3 ~ "Valivotaka",
    m3 == 4 ~ "Mpihavy",
    m3 == 5 ~ "Mpandalo (Migrant temporaire)",
    TRUE ~ NA_character_
  )
}

#' Classify settlement reason from mg4a code
classify_reason <- function(mg4a) {
  dplyr::case_when(
    mg4a %in% c(1, 2, 4) ~ "Sociale (naissance, mariage, famille)",
    mg4a %in% c(3, 7) ~ "\u00c9conomique (travail, ressources)",
    mg4a %in% c(5, 6) ~ "Conjoncturelle (\u00e9cologie, ins\u00e9curit\u00e9)",
    mg4a %in% c(8, 9) ~ "Autre / NSP",
    TRUE ~ NA_character_
  )
}

#' Build origin/homeland/reason analysis table for CM and spouses
#' @param data Merged members data (res_deb + res_m_a with Observatory)
#' @param var_col Unquoted column name (m3, m3a, or mg4a)
#' @param classify_fn Function to decode the variable
#' @param var_label Label for the variable column
#' @param title Table title
make_origin_table <- function(data, var_col, classify_fn, var_label, title) {
  var_col <- rlang::enquo(var_col)

  base <- data |>
    dplyr::filter(m6 %in% c(1, 2)) |>
    dplyr::mutate(
      Member_Type = dplyr::if_else(m6 == 1, "Chef de ménage", "Conjoint"),
      Variable = classify_fn(!!var_col)
    ) |>
    dplyr::filter(!is.na(Variable))

  tbl <- base |>
    dplyr::count(Observatory, Member_Type, Variable) |>
    dplyr::mutate(
      pct = round(n / sum(n) * 100, 1),
      .by = c(Observatory, Member_Type)
    )

  # Suppress cells with n < 5 for confidentiality
  tbl <- suppress_small_counts(tbl, "n", "pct")

  wide <- tbl |>
    tidyr::complete(
      Observatory,
      Member_Type,
      Variable,
      fill = list(n = 0, pct = 0)
    ) |>
    tidyr::pivot_wider(
      names_from = Member_Type,
      values_from = c(n, pct),
      names_glue = "{Member_Type}_{.value}",
      values_fill = 0
    ) |>
    dplyr::rename(
      `Chef N` = `Chef de ménage_n`,
      `Chef %` = `Chef de ménage_pct`,
      `Conjoint N` = Conjoint_n,
      `Conjoint %` = Conjoint_pct
    ) |>
    dplyr::select(
      Observatory,
      Variable,
      `Chef N`,
      `Chef %`,
      `Conjoint N`,
      `Conjoint %`
    )

  obs_gt(wide) |>
    gt::tab_header(title = title, subtitle = "Pourcentages") |>
    gt::cols_hide(c(`Chef N`, `Conjoint N`)) |>
    gt::cols_label(
      Variable = var_label,
      `Chef %` = "%",
      `Conjoint %` = "%"
    ) |>
    gt::tab_spanner(
      label = "Chefs de ménage",
      columns = `Chef %`
    ) |>
    gt::tab_spanner(
      label = "Conjoints",
      columns = `Conjoint %`
    ) |>
    gt::fmt_number(columns = c(`Chef %`, `Conjoint %`), decimals = 1) |>
    gt::sub_missing(missing_text = "< 5") |>
    style_table()
}

#' Build origin histogram
make_origin_histogram <- function(
  data,
  var_col,
  classify_fn,
  var_label,
  title
) {
  var_col <- rlang::enquo(var_col)

  base <- data |>
    dplyr::filter(m6 %in% c(1, 2)) |>
    dplyr::mutate(
      Member_Type = dplyr::if_else(m6 == 1, "Chef de ménage", "Conjoint"),
      Variable = classify_fn(!!var_col)
    ) |>
    dplyr::filter(!is.na(Variable)) |>
    dplyr::count(Observatory, Member_Type, Variable) |>
    dplyr::mutate(
      pct = round(n / sum(n) * 100, 1),
      .by = c(Observatory, Member_Type)
    ) |>
    suppress_in_plot_data()

  base |>
    ror_bar_grouped(
      x = Variable,
      fill = Member_Type,
      title = title,
      y_label = "%",
      direction = "vertical",
      x_angle = 45
    )
}

# ----------------------------------------------------------
# Helpers for categorical / binary tabulations (ch05+)
# ----------------------------------------------------------

#' Decode a labelled Stata variable to a clean UTF-8 character vector
decode_labelled <- function(x) {
  lab <- iconv(as.character(haven::as_factor(x)), from = "latin1", to = "UTF-8")
  gsub("&#39;", "'", lab, fixed = TRUE)
}

#' Tabulate a set of binary indicator variables by Observatory
#'
#' For variables like h6_1, h6_2 … or h7a, h7b … where 1 = yes.
#' @param data Data frame that already contains an `Observatory` column
#' @param prefix Character prefix to match (e.g. "h6", "h7")
#' @return Tibble with Observatory, Type, n, pct
tabulate_binary_set <- function(
  data,
  prefix,
  exclude_pattern = NULL,
  count_mode = FALSE
) {
  vars <- names(data)[grepl(paste0("^", prefix), names(data))]
  if (!is.null(exclude_pattern)) {
    vars <- vars[!grepl(exclude_pattern, vars)]
  }
  labels_tbl <- tibble::tibble(
    var_name = vars,
    Type = vapply(
      data[vars],
      function(x) {
        lab <- iconv(
          labelled::var_label(x) %||% "",
          from = "latin1",
          to = "UTF-8"
        )
        if (grepl(":", lab)) {
          lab <- sub(".*?:\\s*", "", lab)
        }
        stringr::str_squish(stringr::str_to_title(lab))
      },
      character(1)
    )
  )
  n_hh <- data |> dplyr::count(Observatory, name = "N")
  pivoted <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), as.numeric)) |>
    tidyr::pivot_longer(
      dplyr::all_of(vars),
      names_to = "var_name",
      values_to = "value"
    )
  if (count_mode) {
    # For count variables (0 = none, >=1 = owns at least one)
    pivoted <- pivoted |> dplyr::filter(value >= 1)
  } else {
    # For binary variables (1 = yes, 2 = no)
    pivoted <- pivoted |> dplyr::filter(value == 1)
  }
  pivoted |>
    dplyr::left_join(labels_tbl, by = "var_name") |>
    dplyr::count(Observatory, Type) |>
    dplyr::left_join(n_hh, by = "Observatory") |>
    dplyr::mutate(pct = round(n / N * 100, 1)) |>
    dplyr::select(-N)
}
