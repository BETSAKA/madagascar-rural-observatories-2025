# _common.R — runs before every chapter via knitr$opts_knit
# Sets up report mode (consolidated vs observatory-specific) and
# gt LaTeX rendering fixes.

# --- Report mode (profile-aware filtering) ---
source("utils/report_variant.R")
source("utils/sites.R")
source("utils/plot_theme.R")
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

# --- Figure height scaling for observatory profiles ---
# Chunks using !expr ror_fig_height() already account for multi-site
# facets and set the .ror_hook_env$skip flag.  All other chunks with a
# hardcoded fig.height still need the proportional scale-up so that
# faceted boxplots, histograms, etc. remain readable.
if (!REPORT_MODE$is_consolidated) {
  .obs_fig_h_scale <- if (REPORT_MODE$observatory == "Marovoay") 2.0 else 2.5
  knitr::opts_hooks$set(fig.height = function(options) {
    if (.ror_hook_env$skip) {
      .ror_hook_env$skip <- FALSE
      return(options)
    }
    options$fig.height <- min(options$fig.height * .obs_fig_h_scale, 20)
    options
  })
}
