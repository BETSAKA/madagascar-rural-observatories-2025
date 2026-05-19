# R/ror_plots.R
# Minimal plot functions for the cloud calibration app.
# Extracted from utils/ror_plots.R — no microdata dependencies.
# ─────────────────────────────────────────────────────────────

ROR_BLUE <- "#2c7bb6"

# ── theme_ror ─────────────────────────────────────────────────

theme_ror <- function(base_size = 12, n_facets = 1) {
  sz <- if (n_facets > 4) max(base_size - 2, 8) else base_size
  ggplot2::theme_minimal(base_size = sz) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = sz + 1),
      strip.text    = ggplot2::element_text(size = sz - 1, face = "plain"),
      legend.position = if (n_facets > 1) "bottom" else "right",
      legend.title  = ggplot2::element_text(size = sz - 1)
    )
}

# ── Internal helpers ──────────────────────────────────────────

.ror_ncol <- function(n_fct, ncol = NULL) {
  ncol %||% (if (n_fct > 6) 3L else if (n_fct > 3) 2L else n_fct)
}

.ror_complete <- function(data, x_quo, y_name, n_fct) {
  if (n_fct <= 1) return(data)
  fill_vals <- stats::setNames(list(0), y_name)
  if ("n" %in% names(data)) fill_vals[["n"]] <- 0L
  data |> tidyr::complete(Observatory, !!x_quo, fill = fill_vals)
}

.ror_global_order <- function(data, x_quo, y_quo) {
  global_order <- data |>
    dplyr::summarise(.total = sum(!!y_quo, na.rm = TRUE), .by = !!x_quo) |>
    dplyr::arrange(.total) |>
    dplyr::pull(!!x_quo)
  dplyr::mutate(data, !!x_quo := factor(!!x_quo, levels = global_order))
}

# ── make_bar_obs ──────────────────────────────────────────────

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
  x_quo  <- rlang::enquo(x)
  y_quo  <- rlang::enquo(y)
  y_name <- rlang::as_name(y_quo)
  n_fct  <- dplyr::n_distinct(data$Observatory)

  data <- .ror_complete(data, x_quo, y_name, n_fct)
  data <- .ror_global_order(data, x_quo, y_quo)

  label_sz <- if (n_fct > 3) 2.5 else 3.8

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y = !!y_quo)) +
    ggplot2::geom_col(fill = fill_color, show.legend = FALSE) +
    ggplot2::coord_flip()

  if (show_pct) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(!!y_quo > 0, round(!!y_quo), "")),
        hjust = -0.1, size = label_sz
      )
  }

  p <- p +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(title = title, x = NULL, y = y_label) +
    theme_ror(n_facets = n_fct)

  if (n_fct > 1) {
    p <- p + ggplot2::facet_wrap(~Observatory, ncol = .ror_ncol(n_fct, ncol),
                                  scales = "fixed")
  }
  p
}

# ── ror_bar_v ─────────────────────────────────────────────────

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
  x_quo  <- rlang::enquo(x)
  y_quo  <- rlang::enquo(y)
  y_name <- rlang::as_name(y_quo)
  n_fct  <- dplyr::n_distinct(data$Observatory)

  data <- .ror_complete(data, x_quo, y_name, n_fct)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y = !!y_quo)) +
    ggplot2::geom_col(fill = fill_color, show.legend = FALSE)

  if (show_pct) {
    label_sz <- if (n_fct > 3) 2.5 else 3.3
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(!!y_quo > 0, round(!!y_quo), "")),
        vjust = -0.3, size = label_sz
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
  }

  p <- p +
    ggplot2::labs(title = title, x = NULL, y = y_label) +
    theme_ror(n_facets = n_fct)

  if (x_angle > 0) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1)
    )
  }

  if (n_fct > 1) {
    p <- p + ggplot2::facet_wrap(~Observatory, ncol = .ror_ncol(n_fct, ncol))
  }
  p
}

# ── ror_bar_grouped ───────────────────────────────────────────

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
  x_quo    <- rlang::enquo(x)
  y_quo    <- rlang::enquo(y)
  fill_quo <- rlang::enquo(fill)
  y_name   <- rlang::as_name(y_quo)
  n_fct    <- dplyr::n_distinct(data$Observatory)
  fill_name <- rlang::as_name(fill_quo)

  if (n_fct > 1) {
    fill_vals <- stats::setNames(list(0), y_name)
    if ("n" %in% names(data)) fill_vals[["n"]] <- 0L
    if (fill_name == "Observatory") {
      data <- data |> tidyr::complete(Observatory, !!x_quo, fill = fill_vals)
    } else {
      data <- data |> tidyr::complete(Observatory, !!x_quo, !!fill_quo, fill = fill_vals)
    }
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y = !!y_quo, fill = !!fill_quo)) +
    ggplot2::geom_col(position = "dodge")

  if (show_pct) {
    label_sz <- if (n_fct > 3) 2.3 else 3.2
    if (direction == "horizontal") {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = ifelse(!!y_quo > 0, round(!!y_quo), "")),
        position = ggplot2::position_dodge(width = 0.9), hjust = -0.1, size = label_sz)
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = ifelse(!!y_quo > 0, round(!!y_quo), "")),
        position = ggplot2::position_dodge(width = 0.9), vjust = -0.3, size = label_sz)
    }
  }

  if (direction == "horizontal") p <- p + ggplot2::coord_flip()

  if (show_pct) {
    p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
  }

  p <- p +
    ggplot2::labs(title = title, x = NULL, y = y_label, fill = NULL) +
    theme_ror(n_facets = n_fct)

  if (!is.null(palette)) p <- p + ggplot2::scale_fill_manual(values = palette)

  if (direction == "vertical" && x_angle > 0) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1))
  }

  if (facet && n_fct > 1) {
    p <- p + ggplot2::facet_wrap(~Observatory, ncol = .ror_ncol(n_fct, ncol))
  }
  p
}

# ── ror_bar_stacked ───────────────────────────────────────────

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
  x_quo    <- rlang::enquo(x)
  y_quo    <- rlang::enquo(y)
  fill_quo <- rlang::enquo(fill)
  n_fct    <- dplyr::n_distinct(data$Observatory)

  pos <- if (proportion) "fill" else "stack"

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y = !!y_quo, fill = !!fill_quo)) +
    ggplot2::geom_col(position = pos)

  if (show_pct && proportion) {
    label_sz <- if (n_fct > 3) 2.3 else 3.2
    y_vals <- data[[rlang::as_name(y_quo)]]
    is_pct <- max(y_vals, na.rm = TRUE) > 1.5
    if (is_pct) {
      data$.pct_label <- ifelse(y_vals >= min_pct, round(y_vals), "")
    } else {
      data$.pct_label <- ifelse(y_vals >= min_pct / 100, round(y_vals * 100), "")
    }
    p <- p + ggplot2::geom_text(
      data = data, ggplot2::aes(label = .pct_label),
      position = ggplot2::position_fill(vjust = 0.5),
      size = label_sz, colour = "white"
    )
  }

  if (direction == "horizontal") p <- p + ggplot2::coord_flip()
  if (proportion) p <- p + ggplot2::scale_y_continuous(labels = scales::percent)

  p <- p +
    ggplot2::labs(title = title, x = NULL, y = y_label, fill = NULL) +
    theme_ror(n_facets = n_fct)

  if (!is.null(palette)) p <- p + ggplot2::scale_fill_manual(values = palette)

  if (direction != "horizontal" && x_angle > 0) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1))
  }

  if (n_fct > 1) {
    p <- p + ggplot2::facet_wrap(~Observatory, ncol = .ror_ncol(n_fct, ncol))
  }
  p
}

# ── Font override helpers ─────────────────────────────────────

DEFAULT_FONTS <- list(
  base_size = 14, title = 16, axis_text = 12, axis_title = 13,
  legend_text = 12, legend_title = 13, strip_text = 12, geom_text = 4.5
)

apply_presentation_fonts <- function(p, fonts) {
  if (is.null(p) || !inherits(p, "gg")) return(p)
  p <- p + ggplot2::theme(
    text          = ggplot2::element_text(size = fonts$base_size),
    plot.title    = ggplot2::element_text(size = fonts$title, face = "bold"),
    axis.text     = ggplot2::element_text(size = fonts$axis_text),
    axis.text.x   = ggplot2::element_text(size = fonts$axis_text),
    axis.text.y   = ggplot2::element_text(size = fonts$axis_text),
    axis.title    = ggplot2::element_text(size = fonts$axis_title),
    legend.text   = ggplot2::element_text(size = fonts$legend_text),
    legend.title  = ggplot2::element_text(size = fonts$legend_title),
    strip.text    = ggplot2::element_text(size = fonts$strip_text)
  )
  for (i in seq_along(p$layers)) {
    geom <- p$layers[[i]]$geom
    if (inherits(geom, "GeomText") || inherits(geom, "GeomLabel")) {
      p$layers[[i]]$aes_params$size <- fonts$geom_text
    }
  }
  p
}
