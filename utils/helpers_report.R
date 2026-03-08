# utils/helpers_report.R
# Shared helper functions for the ROR observatory report
# ----------------------------------------------------------

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
classify_education <- function(s3a) {
  dplyr::case_when(
    (s3a >= 1 & s3a <= 5) | s3a == 99 ~ "Primaire",
    s3a >= 6 & s3a <= 9 ~ "Secondaire 1er cycle",
    s3a >= 10 ~ "Secondaire 2\u00e8me cycle & sup.",
    TRUE ~ NA_character_
  )
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
    mg4a == 1 ~ "Natif",
    mg4a == 2 ~ "Mariage",
    mg4a == 3 ~ "Travail agricole",
    mg4a == 4 ~ "Travail non agricole",
    mg4a %in% c(5, 6, 7, 8, 9) ~ "Autre raison",
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

  gt::gt(wide, groupname_col = "Observatory") |>
    gt::tab_header(title = title, subtitle = "Effectifs et pourcentages") |>
    gt::cols_label(
      Variable = var_label,
      `Chef N` = "N",
      `Chef %` = "%",
      `Conjoint N` = "N",
      `Conjoint %` = "%"
    ) |>
    gt::tab_spanner(
      label = "Chefs de ménage",
      columns = c(`Chef N`, `Chef %`)
    ) |>
    gt::tab_spanner(
      label = "Conjoints",
      columns = c(`Conjoint N`, `Conjoint %`)
    ) |>
    gt::fmt_number(columns = c(`Chef N`, `Conjoint N`), decimals = 0) |>
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

  ggplot2::ggplot(
    base,
    ggplot2::aes(x = Variable, y = pct, fill = Member_Type)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~Observatory) +
    ggplot2::labs(title = title, x = var_label, y = "%", fill = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
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
tabulate_binary_set <- function(data, prefix) {
  vars <- names(data)[grepl(paste0("^", prefix), names(data))]
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
  data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), as.numeric)) |>
    tidyr::pivot_longer(
      dplyr::all_of(vars),
      names_to = "var_name",
      values_to = "value"
    ) |>
    dplyr::filter(value == 1) |>
    dplyr::left_join(labels_tbl, by = "var_name") |>
    dplyr::count(Observatory, Type) |>
    dplyr::mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory)
}

#' Standard horizontal bar chart faceted by Observatory
#'
#' @param data Tibble with Observatory, a label column, and a percentage column
#' @param x Unquoted column for labels (default: Type)
#' @param y Unquoted column for values  (default: pct)
#' @param title Plot title
make_bar_obs <- function(data, x = Type, y = pct, title = "") {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = stats::reorder({{ x }}, {{ y }}),
      y = {{ y }},
      fill = Observatory
    )
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~Observatory, ncol = 1, scales = "free_y") +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0({{ y }}, "%")),
      hjust = -0.1,
      size = 3.5
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 13)
}

#' Standard multi-year trend plot with solid/gap line style
#'
#' @param data Full trends data
#' @param y_var Unquoted y variable
#' @param y_label Y-axis label
#' @param gap_year Year marking the start of the data gap (default 2014)
make_trend_plot <- function(data, y_var, y_label, gap_year = 2014) {
  solid <- data |> dplyr::filter(year <= gap_year)
  gap <- data |> dplyr::filter(year >= gap_year)
  ggplot2::ggplot(
    mapping = ggplot2::aes(x = year, y = {{ y_var }}, colour = Observatory)
  ) +
    ggplot2::geom_line(data = solid, linewidth = 0.8) +
    ggplot2::geom_line(data = gap, linewidth = 0.8, linetype = "31") +
    ggplot2::geom_point(data = data, size = 2) +
    ggplot2::scale_x_continuous(breaks = seq(1995, 2025, 5)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x = NULL,
      y = y_label,
      colour = "Observatoire",
      linetype = NULL
    ) +
    ggplot2::theme_minimal()
}
