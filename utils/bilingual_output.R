# utils/bilingual_output.R
# ─────────────────────────────────────────────────────────────────
# Bilingual FR/MG output infrastructure for HTML reports.
#
# Two mechanisms:
#   1. TABLES – auto-translated via knit_print.gt_tbl in helpers_report.R
#   2. PLOTS  – auto-translated via a knitr plot hook (below)
#
# Both mechanisms:
#   - Only activate for HTML output
#   - Detect translatable French terms in the data via FR_TO_MG
#   - Only emit an MG <details> section when translation actually changed something
#   - Can be globally disabled by setting BILINGUAL_OUTPUT <- FALSE
#
# Additionally provides emit_mg_plot() for cases where the automatic
# hook cannot handle the plot (e.g., the data was preprocessed in a
# way that loses the original French labels).
# ─────────────────────────────────────────────────────────────────

source("utils/translations.R")

# Global flag: set to FALSE to disable all MG output
BILINGUAL_OUTPUT <- TRUE

# ── CSS for MG collapsible sections ──────────────────────────────
.mg_css_injected <- FALSE

#' Inject CSS for MG collapsible sections (called once per document)
emit_mg_css <- function() {
  if (.mg_css_injected) {
    return(invisible(NULL))
  }
  css <- '
<style>
details.mg-version {
  margin-top: 0.5rem;
  margin-bottom: 1rem;
  border-left: 3px solid #2c7bb6;
  padding-left: 0.75rem;
}
details.mg-version summary {
  cursor: pointer;
  font-weight: 600;
  color: #2c7bb6;
  font-size: 0.9em;
}
details.mg-version summary:hover {
  text-decoration: underline;
}
details.mg-version img {
  margin-top: 0.5rem;
}
</style>
'
  cat(css)
  .mg_css_injected <<- TRUE
  invisible(NULL)
}


# ── Knitr plot hook: auto-append MG versions ─────────────────────
# Intercepts knitr's plot rendering. After the default plot output,
# checks last_plot() for translatable data; if found, re-renders
# with MG labels and appends a collapsible <details> section.
#
# Chunks can opt out with: #| mg: false

if (requireNamespace("knitr", quietly = TRUE)) {
  .default_plot_hook <- knitr::knit_hooks$get("plot")

  knitr::knit_hooks$set(plot = function(x, options) {
    # Get default output from the original hook
    default_output <- .default_plot_hook(x, options)

    # Skip if bilingual output is disabled
    if (!exists("BILINGUAL_OUTPUT") || !BILINGUAL_OUTPUT) {
      return(default_output)
    }
    if (!knitr::is_html_output()) {
      return(default_output)
    }

    # Skip if chunk explicitly opts out
    if (isFALSE(options$mg)) {
      return(default_output)
    }

    # Get the ggplot object that was just rendered
    p <- tryCatch(ggplot2::last_plot(), error = function(e) NULL)
    if (is.null(p) || is.null(p$data)) {
      return(default_output)
    }

    # Auto-translate the plot data
    d_mg <- tryCatch(auto_translate_data_mg(p$data), error = function(e) p$data)
    if (identical(as.data.frame(d_mg), as.data.frame(p$data))) {
      return(default_output)
    }

    # Build MG version using %+% data replacement
    p_mg <- tryCatch(
      {
        pm <- p %+% d_mg
        # Translate axis/legend labels
        for (lab in c("title", "x", "y", "fill", "colour", "color")) {
          val <- p$labels[[lab]]
          if (!is.null(val) && is.character(val) && nchar(val) > 0) {
            tr <- translate_mg(val)
            if (!identical(tr, val)) {
              args <- stats::setNames(list(tr), lab)
              pm <- pm + do.call(ggplot2::labs, args)
            }
          }
        }
        pm
      },
      error = function(e) NULL
    )

    if (is.null(p_mg)) {
      return(default_output)
    }

    # Render MG plot to base64 PNG
    mg_html <- tryCatch(
      {
        tmp <- tempfile(fileext = ".png")
        on.exit(unlink(tmp), add = TRUE)
        w <- options$fig.width %||% 10
        h <- options$fig.height %||% 6
        ggplot2::ggsave(
          tmp,
          p_mg,
          width = w,
          height = h,
          dpi = 150,
          bg = "white"
        )
        img_uri <- xfun::base64_uri(tmp)
        paste0(
          '\n<details class="mg-version">\n',
          '<summary>\U0001F1F2\U0001F1EC Dikanteny Malagasy</summary>\n\n',
          '<img src="',
          img_uri,
          '" style="max-width:100%;" alt="Sary Malagasy" />\n',
          '\n</details>\n\n'
        )
      },
      error = function(e) ""
    )

    paste0(default_output, mg_html)
  })
}


# ── Manual emit functions (for complex cases) ────────────────────

#' Emit a Malagasy version of a ggplot in a collapsible <details> block
#'
#' For use in chunks where the automatic plot hook cannot handle the
#' translation (e.g., custom data prep is needed). Call from a chunk
#' with `#| results: asis`.
#'
#' @param data Data frame (same as used for the FR plot)
#' @param translate_cols Character vector of column names to translate
#' @param build_fn Function(data) -> ggplot object
#' @param width PNG width in inches (default 10)
#' @param height PNG height in inches (default 6)
#' @param dpi PNG resolution (default 150)
emit_mg_plot <- function(
  data,
  translate_cols,
  build_fn,
  width = 10,
  height = 6,
  dpi = 150
) {
  if (!BILINGUAL_OUTPUT) {
    return(invisible(NULL))
  }
  if (!knitr::is_html_output()) {
    return(invisible(NULL))
  }

  emit_mg_css()

  data_mg <- translate_data_mg(data, translate_cols)
  p_mg <- tryCatch(build_fn(data_mg), error = function(e) NULL)
  if (is.null(p_mg)) {
    return(invisible(NULL))
  }

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ggplot2::ggsave(
    tmp,
    p_mg,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  img_uri <- xfun::base64_uri(tmp)

  cat('<details class="mg-version">\n')
  cat('<summary>\U0001F1F2\U0001F1EC Dikanteny Malagasy</summary>\n\n')
  cat(sprintf(
    '<img src="%s" style="max-width:100%%;" alt="Sary Malagasy" />\n',
    img_uri
  ))
  cat('\n</details>\n\n')
  invisible(NULL)
}

#' Emit a MG plot with automatic height from data
#'
#' Convenience wrapper for horizontal bar charts where height
#' scales with the number of categories.
#'
#' @inheritParams emit_mg_plot
#' @param x_col Character: name of the category column (for height calc)
#' @param per_item Height per category in inches (default 0.45)
emit_mg_plot_auto <- function(
  data,
  translate_cols,
  build_fn,
  x_col = "Type",
  per_item = 0.45,
  width = 10,
  dpi = 150
) {
  n_items <- dplyr::n_distinct(data[[x_col]])
  n_obs <- dplyr::n_distinct(data$Observatory)
  height <- max(3, min(1 + n_items * per_item * ceiling(n_obs / 2), 20))
  emit_mg_plot(
    data,
    translate_cols,
    build_fn,
    width = width,
    height = height,
    dpi = dpi
  )
}

#' Emit a MG version of a ggplot using auto-translation on its data
#'
#' Takes a ggplot object, auto-detects translatable columns, replaces
#' data using `%+%`, translates labels, and emits as a collapsible block.
#' For use in `#| results: asis` chunks as a manual fallback.
#'
#' @param p A ggplot object
#' @param title_mg Optional Malagasy title override
#' @param width PNG width in inches
#' @param height PNG height in inches
#' @param dpi PNG resolution (default 150)
emit_mg_from_plot <- function(
  p,
  title_mg = NULL,
  width = NULL,
  height = NULL,
  dpi = 150
) {
  if (!BILINGUAL_OUTPUT) {
    return(invisible(NULL))
  }
  if (!knitr::is_html_output()) {
    return(invisible(NULL))
  }
  if (is.null(p)) {
    return(invisible(NULL))
  }

  emit_mg_css()

  d_mg <- auto_translate_data_mg(p$data)
  if (identical(d_mg, p$data)) {
    return(invisible(NULL))
  }

  p_mg <- p %+% d_mg
  for (lab in c("title", "x", "y", "fill", "colour", "color")) {
    val <- p$labels[[lab]]
    if (!is.null(val) && is.character(val)) {
      translated <- translate_mg(val)
      if (translated != val) {
        args <- stats::setNames(list(translated), lab)
        p_mg <- p_mg + do.call(ggplot2::labs, args)
      }
    }
  }
  if (!is.null(title_mg)) {
    p_mg <- p_mg + ggplot2::labs(title = title_mg)
  }

  w <- width %||% knitr::opts_current$get("fig.width") %||% 10
  h <- height %||% knitr::opts_current$get("fig.height") %||% 6

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ggplot2::ggsave(tmp, p_mg, width = w, height = h, dpi = dpi, bg = "white")
  img_uri <- xfun::base64_uri(tmp)

  cat('<details class="mg-version">\n')
  cat('<summary>\U0001F1F2\U0001F1EC Dikanteny Malagasy</summary>\n\n')
  cat(sprintf(
    '<img src="%s" style="max-width:100%%;" alt="Sary Malagasy" />\n',
    img_uri
  ))
  cat('\n</details>\n\n')
  invisible(NULL)
}
