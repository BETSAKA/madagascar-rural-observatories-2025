# _common.R — runs before every chapter via knitr$opts_knit
# Sets up report mode (consolidated vs observatory-specific) and
# gt LaTeX rendering fixes.

# --- Report mode (profile-aware filtering) ---
source("utils/report_variant.R")
REPORT_MODE <- get_report_mode()

# --- gt LaTeX accent fix (simple version; helpers_report.R has comprehensive) ---
knit_print.gt_tbl <- function(x, ...) {
  if (knitr::is_latex_output()) {
    latex_str <- as.character(gt::as_latex(x))
    latex_str <- gsub("\\\\'e", "\u00e9", latex_str, fixed = TRUE)
    latex_str <- gsub("\\\\'E", "\u00c9", latex_str, fixed = TRUE)
    return(knitr::asis_output(latex_str))
  }
  knitr::knit_print(gt::as_raw_html(x), ...)
}

registerS3method(
  "knit_print",
  "gt_tbl",
  knit_print.gt_tbl,
  envir = asNamespace("knitr")
)
