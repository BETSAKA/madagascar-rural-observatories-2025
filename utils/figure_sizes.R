# utils/figure_sizes.R
# Helpers to read per-figure, per-profile dimensions from data/figure_sizes.yml
# Loaded by _common.R — provides fig_h() and fig_w() for use in chunk headers.
# ------------------------------------------------------------------

.fig_sizes_env <- new.env(parent = emptyenv())

#' (Re)load figure sizes from YAML
#' @noRd
.load_fig_sizes <- function(force = FALSE) {
 if (force || is.null(.fig_sizes_env$sizes)) {
   yml_path <- "data/figure_sizes.yml"
   if (!file.exists(yml_path)) {
     # Try from project root (when wd is a subdirectory)
     yml_path <- file.path(here::here(), "data", "figure_sizes.yml")
   }
   if (file.exists(yml_path)) {
     .fig_sizes_env$sizes <- yaml::read_yaml(yml_path)
   } else {
     warning("figure_sizes.yml not found — using defaults")
     .fig_sizes_env$sizes <- list()
   }
 }
 invisible(.fig_sizes_env$sizes)
}

#' Get the current profile key for lookup
#' @noRd
.current_profile <- function() {
  mode <- get_report_mode()
  if (mode$is_consolidated) "consolidated" else tolower(mode$observatory)
}

#' Get figure height for the current profile
#'
#' Use in chunk YAML as: `#| fig-height: !expr fig_h("fig-label")`
#'
#' Lookup order:
#'   1. sizes[[label]][[profile]]$height
#'   2. sizes[[label]]$default$height
#'   3. `default` argument
#'
#' Also sets `.ror_hook_env$skip = TRUE` so the multiplicative knitr
#' hook in _common.R does not further scale the height.
#'
#' @param label   Character: the chunk label (e.g. "fig-chronogramme").
#' @param default Numeric: fallback height in inches if not in YAML.
#' @return Numeric height in inches.
fig_h <- function(label, default = 5) {
  .load_fig_sizes()
  sizes <- .fig_sizes_env$sizes
  profile <- .current_profile()

  # Alaotra auto-derives from Marovoay: same width, height × 1.25
  if (profile == "alaotra") {
    maro_h <- sizes[[label]]$marovoay$height %||%
              sizes[[label]]$default$height %||%
              default
    h <- maro_h * 1.25
  } else {
    h <- sizes[[label]][[profile]]$height %||%
         sizes[[label]]$default$height %||%
         default
  }

  # Skip the multiplicative hook — this height is already calibrated
  if (exists(".ror_hook_env", envir = globalenv())) {
    .ror_hook_env$skip <- TRUE
  }
  h
}

#' Get figure width for the current profile
#'
#' Use in chunk YAML as: `#| fig-width: !expr fig_w("fig-label")`
#'
#' @param label   Character: the chunk label.
#' @param default Numeric: fallback width in inches if not in YAML (Quarto default: 7).
#' @return Numeric width in inches.
fig_w <- function(label, default = 7) {
  .load_fig_sizes()
  sizes <- .fig_sizes_env$sizes
  profile <- .current_profile()

  # Alaotra auto-derives from Marovoay: same width
  if (profile == "alaotra") {
    sizes[[label]]$marovoay$width %||%
      sizes[[label]]$default$width %||%
      default
  } else {
    sizes[[label]][[profile]]$width %||%
      sizes[[label]]$default$width %||%
      default
  }
}

#' Force-reload sizes (useful after calibration app saves changes)
fig_sizes_reload <- function() {
  .load_fig_sizes(force = TRUE)
}
