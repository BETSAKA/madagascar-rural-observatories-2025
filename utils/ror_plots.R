# utils/ror_plots.R
# ─────────────────────────────────────────────────────────────────
# Generic plot functions for the ROR observatory report.
#
# Typology:
#   make_bar_obs()     Horizontal bar chart (single fill colour)
#   ror_bar_v()        Vertical bar chart   (single fill colour)
#   ror_bar_grouped()  Grouped (dodged) bar chart (fill = grouping var)
#   ror_bar_stacked()  Stacked bar chart          (fill = grouping var)
#   make_trend_plot()  Multi-year trend line with data-gap dashed segment
#   ror_pyramid()      Population pyramid (paired horizontal bars)
#   ror_gt()           Enhanced gt table (obs_gt + style_table)
#
# All functions apply theme_ror() for consistent typography and
# conditionally facet by Observatory when the data has > 1 level.
# Categories are kept uniform across facets via tidyr::complete().
# ─────────────────────────────────────────────────────────────────

ROR_BLUE <- "#2c7bb6"

# ── Hook-skip flag ────────────────────────────────────────────
# Quarto's !expr YAML evaluation may strip R attributes, so
# instead of relying on an attribute to tell the knitr hook
# "I already computed the multi-row height", we set a global flag
# that the hook checks and clears.
.ror_hook_env <- new.env(parent = emptyenv())
.ror_hook_env$skip <- FALSE

# ── Figure-height helpers ─────────────────────────────────────

#' Number of Observatory facets in the current report profile
#'
#' In consolidated mode, counts distinct Observatory values in the
#' data (typically 2).  In per-observatory mode the plot chunk will
#' call `expand_sites_for_profile()`, which replaces Observatory
#' with site names + "Tous".  If the data has already been expanded
#' we detect that (> 2 distinct values, or values matching known
#' site names); otherwise we look up the site count from SITES.
#'
#' NAs in Observatory (from partial joins) are always excluded.
.ror_n_obs <- function(data) {
  mode <- get_report_mode()
  n <- if ("Observatory" %in% names(data)) {
    obs_vals <- data$Observatory[!is.na(data$Observatory)]
    if (length(obs_vals) == 0L) 1L else dplyr::n_distinct(obs_vals)
  } else {
    1L
  }

  if (mode$is_consolidated) {
    return(n)
  }

  # Per-observatory mode: if data was already expanded by

  # expand_sites_for_profile(), n will be sites + 1 ("Tous").
  # If not yet expanded, n == 1; look up from SITES + 1 for "Tous".
  if (n <= 1L) {
    obs <- mode$observatory
    n <- length(SITES[[obs]]) + 1L
  }
  n
}

#' Compute figure height (additive model with per-panel cap)
#'
#' Uses an additive formula:
#'   `base + n_rows * min(n_items * per_item + strip_h, max_panel_h)`
#'
#' For single-row layouts (consolidated) the cap is not applied so that
#' height grows linearly with the category count.  For multi-row layouts
#' the cap prevents very long figures when there are many categories.
#'
#' @param data   Data frame (with an `Observatory` column if faceted).
#' @param x_col  Character name of the category column (default `"Type"`).
#' @param per_item  Height per category item in inches (default 0.45).
#' @param base    Global base height for title/legend in inches (default 1.0).
#' @param strip_h Per-panel overhead: strip title + axis (default 0.5).
#' @param max_panel_h  Maximum panel height per row when faceted (default 3.0).
#' @param max_height  Absolute ceiling in inches (default 20).
#' @return Numeric: figure height in inches.
ror_fig_height <- function(
  data,
  x_col = "Type",
  per_item = 0.45,
  base = 1.0,
  strip_h = 0.5,
  max_panel_h = 3.0,
  max_height = 20
) {
  n_items <- dplyr::n_distinct(data[[x_col]])
  .ror_compute_height(
    n_items,
    data = data,
    per_item = per_item,
    base = base,
    strip_h = strip_h,
    max_panel_h = max_panel_h,
    max_height = max_height
  )
}

#' Compute figure height from a known category count
#'
#' Variant of [ror_fig_height()] for cases where the x column is
#' created during a pipeline (e.g. via `factor()`) and does not
#' yet exist in the source data.  Pass the number of categories
#' (typically `length(labels_vector)`) and the source data frame
#' for the Observatory count.
#'
#' @param n_items  Integer: number of y-axis categories.
#' @param data     Data frame with `Observatory` (for facet-row count),
#'                 or `NULL` (single panel).
#' @inheritParams ror_fig_height
#' @return Numeric: figure height in inches.
ror_fig_height_n <- function(
  n_items,
  data = NULL,
  per_item = 0.45,
  base = 1.0,
  strip_h = 0.5,
  max_panel_h = 3.0,
  max_height = 20
) {
  .ror_compute_height(
    n_items,
    data = data,
    per_item = per_item,
    base = base,
    strip_h = strip_h,
    max_panel_h = max_panel_h,
    max_height = max_height
  )
}

#' Internal: shared height computation
#' @noRd
.ror_compute_height <- function(
  n_items,
  data = NULL,
  per_item = 0.45,
  base = 1.0,
  strip_h = 0.5,
  max_panel_h = 3.0,
  max_height = 20
) {
  if (!is.null(data)) {
    n_obs <- .ror_n_obs(data)
  } else {
    mode <- get_report_mode()
    n_obs <- if (!mode$is_consolidated) {
      length(SITES[[mode$observatory]]) + 1L
    } else {
      1L
    }
  }
  ncol <- .ror_ncol(n_obs)
  n_rows <- if (n_obs > 1) ceiling(n_obs / ncol) else 1L

  panel_h <- n_items * per_item + strip_h
  if (n_rows > 1L) {
    panel_h <- min(panel_h, max_panel_h)
  }
  h <- min(round(base + n_rows * panel_h, 1), max_height)

  # Signal to the knitr hook that this height is auto-computed
  .ror_hook_env$skip <- TRUE
  h
}

# ── Internal helpers ──────────────────────────────────────────

#' Auto-compute facet ncol
.ror_ncol <- function(n_fct, ncol = NULL) {
  ncol %||% (if (n_fct > 3) 2L else n_fct)
}

#' Ensure all Observatory × category combinations exist (fills missing with 0)
.ror_complete <- function(data, x_quo, y_name, n_fct) {
  if (n_fct <= 1) {
    return(data)
  }
  fill_vals <- stats::setNames(list(0), y_name)
  if ("n" %in% names(data)) {
    fill_vals[["n"]] <- 0L
  }
  data |> tidyr::complete(Observatory, !!x_quo, fill = fill_vals)
}

#' Reorder a column globally by sum of y
.ror_global_order <- function(data, x_quo, y_quo) {
  global_order <- data |>
    dplyr::summarise(.total = sum(!!y_quo, na.rm = TRUE), .by = !!x_quo) |>
    dplyr::arrange(.total) |>
    dplyr::pull(!!x_quo)
  dplyr::mutate(data, !!x_quo := factor(!!x_quo, levels = global_order))
}


# ── make_bar_obs ──────────────────────────────────────────────
#' Horizontal bar chart, optionally faceted by Observatory
#'
#' Produces a `geom_col() + coord_flip()` plot with:
#' * Uniform categories across all facets (via `complete()`).
#' * Global ordering by descending overall total.
#' * Percentage labels on each bar.
#' * theme_ror() for consistent typography & legend placement.
#' * Single fill colour (no wasteful `fill = Observatory`).
#'
#' @param data  Tibble with an Observatory column, a category column, and a value column.
#' @param x     Unquoted column for category labels (default: `Type`).
#' @param y     Unquoted column for values  (default: `pct`).
#' @param title Plot title (character).
#' @param y_label Axis label for the value axis (`NULL` = none).
#' @param fill_color Bar fill colour (default: `#2c7bb6`).
#' @param show_pct  Logical – show "XX%" labels on bars?
#' @param pct_suffix Suffix for labels (default "%").
#' @param ncol  Number of facet columns (`NULL` = auto).
#' @return A ggplot object.
make_bar_obs <- function(
  data,
  x = Type,
  y = pct,
  title = "",
  y_label = NULL,
  fill_color = ROR_BLUE,
  show_pct = TRUE,
  pct_suffix = "%",
  ncol = NULL
) {
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  y_name <- rlang::as_name(y_quo)
  n_fct <- dplyr::n_distinct(data$Observatory)

  # Uniform categories + global ordering
  data <- .ror_complete(data, x_quo, y_name, n_fct)
  data <- .ror_global_order(data, x_quo, y_quo)

  label_sz <- if (n_fct > 3) 2.2 else 3.5

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y = !!y_quo)) +
    ggplot2::geom_col(fill = fill_color, show.legend = FALSE) +
    ggplot2::coord_flip()

  if (show_pct) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          label = ifelse(!!y_quo > 0, paste0(!!y_quo, pct_suffix), "")
        ),
        hjust = -0.1,
        size = label_sz
      )
  }

  p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(title = title, x = NULL, y = y_label) +
    theme_ror(n_facets = n_fct)

  if (n_fct > 1) {
    p <- p +
      ggplot2::facet_wrap(
        ~Observatory,
        ncol = .ror_ncol(n_fct, ncol),
        scales = "fixed"
      )
  }

  p
}


# ── ror_bar_v ─────────────────────────────────────────────────
#' Vertical bar chart, optionally faceted by Observatory
#'
#' Like `make_bar_obs()` but without `coord_flip()`.  Best suited for ordinal
#' categories (time periods, months) where left-to-right reading is natural.
#'
#' @inheritParams make_bar_obs
#' @param x_angle Angle (degrees) for x-axis text rotation (0 = horizontal).
ror_bar_v <- function(
  data,
  x = Type,
  y = pct,
  title = "",
  y_label = NULL,
  fill_color = ROR_BLUE,
  show_pct = TRUE,
  x_angle = 0,
  ncol = NULL
) {
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  y_name <- rlang::as_name(y_quo)
  n_fct <- dplyr::n_distinct(data$Observatory)

  data <- .ror_complete(data, x_quo, y_name, n_fct)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y = !!y_quo)) +
    ggplot2::geom_col(fill = fill_color, show.legend = FALSE)

  if (show_pct) {
    label_sz <- if (n_fct > 3) 2.2 else 3
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(!!y_quo > 0, paste0(!!y_quo, "%"), "")),
        vjust = -0.3,
        size = label_sz
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.15))
      )
  }

  p <- p +
    ggplot2::labs(title = title, x = NULL, y = y_label) +
    theme_ror(n_facets = n_fct)

  if (x_angle > 0) {
    p <- p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1)
      )
  }

  if (n_fct > 1) {
    p <- p +
      ggplot2::facet_wrap(
        ~Observatory,
        ncol = .ror_ncol(n_fct, ncol)
      )
  }

  p
}


# ── ror_bar_grouped ───────────────────────────────────────────
#' Grouped (dodged) bar chart with a fill variable
#'
#' @param data  Data with Observatory, category, value, and fill columns.
#' @param x     Unquoted category column.
#' @param y     Unquoted value column (default: `pct`).
#' @param fill  Unquoted grouping column mapped to fill.
#' @param direction "horizontal" (coord_flip) or "vertical".
#' @param x_angle Rotation angle for x-axis labels (vertical mode only).
#' @param palette Named colour vector (`NULL` = default ggplot2 palette).
ror_bar_grouped <- function(
  data,
  x,
  y = pct,
  fill,
  title = "",
  y_label = NULL,
  direction = "vertical",
  x_angle = 45,
  ncol = NULL,
  palette = NULL,
  facet = TRUE,
  show_pct = TRUE,
  pct_suffix = "%"
) {
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  fill_quo <- rlang::enquo(fill)
  y_name <- rlang::as_name(y_quo)
  n_fct <- dplyr::n_distinct(data$Observatory)

  # Uniform categories across facets
  fill_name <- rlang::as_name(fill_quo)
  if (n_fct > 1) {
    fill_vals <- stats::setNames(list(0), y_name)
    if ("n" %in% names(data)) {
      fill_vals[["n"]] <- 0L
    }
    # Avoid duplicate columns when fill IS Observatory
    if (fill_name == "Observatory") {
      data <- data |>
        tidyr::complete(Observatory, !!x_quo, fill = fill_vals)
    } else {
      data <- data |>
        tidyr::complete(Observatory, !!x_quo, !!fill_quo, fill = fill_vals)
    }
  }

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = !!x_quo, y = !!y_quo, fill = !!fill_quo)
  ) +
    ggplot2::geom_col(position = "dodge")

  if (show_pct) {
    label_sz <- if (n_fct > 3) 2 else 2.8
    if (direction == "horizontal") {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = ifelse(!!y_quo > 0, paste0(!!y_quo, pct_suffix), "")
          ),
          position = ggplot2::position_dodge(width = 0.9),
          hjust = -0.1,
          size = label_sz
        )
    } else {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = ifelse(!!y_quo > 0, paste0(!!y_quo, pct_suffix), "")
          ),
          position = ggplot2::position_dodge(width = 0.9),
          vjust = -0.3,
          size = label_sz
        )
    }
  }

  if (direction == "horizontal") {
    p <- p + ggplot2::coord_flip()
  }

  if (show_pct) {
    p <- p +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.15))
      )
  }

  p <- p +
    ggplot2::labs(title = title, x = NULL, y = y_label, fill = NULL) +
    theme_ror(n_facets = n_fct)

  if (!is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette)
  }

  if (direction == "vertical" && x_angle > 0) {
    p <- p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1)
      )
  }

  if (facet && n_fct > 1) {
    p <- p +
      ggplot2::facet_wrap(
        ~Observatory,
        ncol = .ror_ncol(n_fct, ncol)
      )
  }

  p
}


# ── ror_bar_stacked ───────────────────────────────────────────
#' Stacked bar chart
#'
#' @param proportion If `TRUE` (default), use `position = "fill"` (show shares);
#'   if `FALSE`, use `position = "stack"` (show counts).
#' @inheritParams ror_bar_grouped
ror_bar_stacked <- function(
  data,
  x,
  y,
  fill,
  title = "",
  y_label = NULL,
  proportion = TRUE,
  direction = "horizontal",
  x_angle = 0,
  ncol = NULL,
  palette = NULL,
  show_pct = TRUE,
  min_pct = 5
) {
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  fill_quo <- rlang::enquo(fill)
  n_fct <- dplyr::n_distinct(data$Observatory)

  pos <- if (proportion) "fill" else "stack"

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = !!x_quo, y = !!y_quo, fill = !!fill_quo)
  ) +
    ggplot2::geom_col(position = pos)

  if (show_pct && proportion) {
    label_sz <- if (n_fct > 3) 2 else 2.8
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          label = ifelse(!!y_quo >= min_pct / 100,
                         paste0(round(!!y_quo * 100), "%"), "")
        ),
        position = ggplot2::position_fill(vjust = 0.5),
        size = label_sz,
        colour = "white"
      )
  }

  if (direction == "horizontal") {
    p <- p + ggplot2::coord_flip()
  }

  if (proportion) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::percent)
  }

  p <- p +
    ggplot2::labs(title = title, x = NULL, y = y_label, fill = NULL) +
    theme_ror(n_facets = n_fct)

  if (!is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette)
  }

  if (direction != "horizontal" && x_angle > 0) {
    p <- p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1)
      )
  }

  if (n_fct > 1) {
    p <- p +
      ggplot2::facet_wrap(
        ~Observatory,
        ncol = .ror_ncol(n_fct, ncol)
      )
  }

  p
}


# ── make_trend_plot ───────────────────────────────────────────
#' Multi-year trend line with dashed gap segment
#'
#' Draws solid lines up to `gap_year`, then a dashed line from `gap_year`
#' onward to materialise the data gap.
#'
#' @param data    Data with `year` and `Observatory` columns.
#' @param y_var   Unquoted y variable.
#' @param y_label Y-axis label.
#' @param gap_year Year marking the start of the gap (default 2014).
#' @param title   Plot title.
make_trend_plot <- function(data, y_var, y_label, gap_year = 2014, title = "") {
  y_quo <- rlang::enquo(y_var)

  solid <- data |> dplyr::filter(year <= gap_year)
  gap <- data |> dplyr::filter(year >= gap_year)

  ggplot2::ggplot(
    mapping = ggplot2::aes(x = year, y = !!y_quo, colour = Observatory)
  ) +
    ggplot2::geom_line(data = solid, linewidth = 0.8) +
    ggplot2::geom_line(data = gap, linewidth = 0.8, linetype = "31") +
    ggplot2::geom_point(data = data, size = 2) +
    ggplot2::scale_x_continuous(breaks = seq(1995, 2025, 5)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_label,
      colour = "Observatoire"
    ) +
    theme_ror()
}


# ── ror_pyramid ───────────────────────────────────────────────
#' Population pyramid (paired horizontal bars)
#'
#' Expects the data to have negative values for one sex and positive for the
#' other (the caller must prepare this).
#'
#' @param data    Data with Observatory, age group, sex, and value columns.
#' @param age_col Unquoted age-group column.
#' @param sex_col Unquoted sex column.
#' @param y       Unquoted value column (default: `pct`).
#' @param palette Named colour vector for the two sexes.
ror_pyramid <- function(
  data,
  age_col,
  sex_col,
  y = pct,
  title = "",
  ncol = NULL,
  palette = c("Homme" = "#4682B4", "Femme" = "#CD5C5C")
) {
  age_quo <- rlang::enquo(age_col)
  sex_quo <- rlang::enquo(sex_col)
  y_quo <- rlang::enquo(y)
  n_fct <- dplyr::n_distinct(data$Observatory)

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = !!age_quo, y = !!y_quo, fill = !!sex_quo)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(abs(x))) +
    ggplot2::labs(title = title, x = NULL, y = "%", fill = NULL) +
    theme_ror(n_facets = n_fct)

  if (n_fct > 1) {
    p <- p +
      ggplot2::facet_wrap(
        ~Observatory,
        ncol = .ror_ncol(n_fct, ncol)
      )
  }

  p
}


# ── ror_gt ────────────────────────────────────────────────────
#' Enhanced gt table with Observatory grouping + standard styling
#'
#' Combines `obs_gt()` (conditional Observatory grouping) and
#' `style_table()` (header + row-group styling + source note) in one call.
#'
#' @param data     Data frame with an Observatory column.
#' @param title    Table title (NULL = none).
#' @param subtitle Table subtitle (NULL = none).
#' @param ...      Additional arguments passed to `gt::gt()`.
ror_gt <- function(data, title = NULL, subtitle = NULL, ...) {
  tbl <- obs_gt(data, ...)
  if (!is.null(title)) {
    tbl <- tbl |> gt::tab_header(title = title, subtitle = subtitle)
  }
  tbl |> style_table()
}
