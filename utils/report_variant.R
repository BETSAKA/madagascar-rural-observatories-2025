# ── Report variant utilities ──────────────────────────────────────────────
# Helpers to adapt content based on the active Quarto profile
# (consolidated, marovoay, alaotra).
#
# Usage:
#   source("utils/report_variant.R")
#   mode <- get_report_mode()
#   if (!mode$is_consolidated) {
#     data <- data |> filter(Observatory == mode$observatory)
#   }

#' Return the current report mode based on the QUARTO_PROFILE env var.
#'
#' @return A list with:
#'   - `profile`        : character, the active profile name
#'   - `is_consolidated`: logical, TRUE when rendering the consolidated report
#'   - `observatory`    : character or NULL, the observatory name for filtering
get_report_mode <- function() {
  profile <- Sys.getenv("QUARTO_PROFILE", "consolidated")
  list(
    profile        = profile,
    is_consolidated = profile == "consolidated",
    observatory    = switch(profile,
      "marovoay" = "Marovoay",
      "alaotra"  = "Alaotra",
      NULL
    )
  )
}

#' Filter a data frame to the active observatory when not in consolidated mode.
#'
#' @param data A data frame with an `Observatory` column.
#' @param mode Optional: result of `get_report_mode()`. If NULL, calls it.
#' @return The (possibly filtered) data frame.
filter_for_profile <- function(data, mode = NULL) {
  if (is.null(mode)) mode <- get_report_mode()
  if (mode$is_consolidated || is.null(mode$observatory)) {
    return(data)
  }
  data |> dplyr::filter(Observatory == mode$observatory)
}
