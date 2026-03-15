# _common.R — runs before every chapter via knitr$opts_knit
# gt tables render as native LaTeX (XeLaTeX handles Unicode natively).
# We override knit_print.gt_tbl to fix gt 1.x's double-escaping of
# acute accents in LaTeX output (é → \\'e instead of é).

knit_print.gt_tbl <- function(x, ...) {
  if (knitr::is_latex_output()) {
    latex_str <- as.character(gt::as_latex(x))
    # gt 1.x double-escapes acute accents: é → \\'e, É → \\'E
    # Fix: convert back to UTF-8 (safe with XeLaTeX)
    latex_str <- gsub("\\\\'e", "\u00e9", latex_str, fixed = TRUE)
    latex_str <- gsub("\\\\'E", "\u00c9", latex_str, fixed = TRUE)
    return(knitr::asis_output(latex_str))
  }
  # HTML / DOCX: default gt rendering
  knitr::knit_print(gt::as_raw_html(x), ...)
}

registerS3method("knit_print", "gt_tbl", knit_print.gt_tbl,
                 envir = asNamespace("knitr"))
