# utils/plot_theme.R
# Centralised ggplot2 theme and layout helpers for the ROR report
# ----------------------------------------------------------------

#' Standard ggplot2 theme for ROR observatory reports
#'
#' Adapts font sizes, strip text, and legend placement based on the number
#' of facets.  Strip text is intentionally NOT bold and slightly smaller
#' so that long site names fit without cropping.
#' @param base_size Base font size (default 11)
#' @param n_facets  Number of facets in the plot (1 = no faceting)
#' @return A ggplot2 theme object
theme_ror <- function(base_size = 11, n_facets = 1) {
  sz <- if (n_facets > 4) max(base_size - 2, 8) else base_size
  ggplot2::theme_minimal(base_size = sz) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = sz + 1),
      strip.text = ggplot2::element_text(size = sz - 1, face = "plain"),
      legend.position = if (n_facets > 1) "bottom" else "right",
      legend.title = ggplot2::element_text(size = sz - 1)
    )
}

#' Compute a reasonable figure height given the number of facets
#'
#' @param n_facets     Number of facet panels
#' @param n_categories Number of categories on the category axis
#' @param has_legend   Whether a legend is shown below the plot
#' @param base         Minimum height in inches
#' @param ncol         Number of facet columns (NULL = auto)
#' @return Height in inches, capped at 20
calc_fig_height <- function(
  n_facets,
  n_categories = 8,
  has_legend = FALSE,
  base = 3,
  ncol = NULL
) {
  if (is.null(ncol)) {
    ncol <- if (n_facets <= 2) n_facets else 2L
  }
  rows <- ceiling(n_facets / ncol)
  panel_h <- max(n_categories * 0.28, 2)
  h <- base + rows * panel_h
  if (has_legend) {
    h <- h + 0.8
  }
  min(h, 20)
}
