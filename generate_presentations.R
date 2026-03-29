# generate_presentations.R
# ─────────────────────────────────────────────────────────────────
# Generates two PPTX slide decks (Marovoay & Alaotra) with one
# figure per slide, using the figures listed in
# documentation/list_for_presentation.md
#
# Dimensions come from data/figure_sizes.yml:
#   - Marovoay: marovoay width × marovoay height
#   - Alaotra:  marovoay width × (marovoay height × 1.25)
#
# Usage:
#   source("generate_presentations.R")
# ─────────────────────────────────────────────────────────────────

library(officer)
library(rvg)
library(yaml)
library(dplyr)
library(ggplot2)
library(writexl)

PROJECT_ROOT <- here::here()

# ── 1. Parse the figure list ──────────────────────────────────

#' Extract figure labels from the markdown list
#' The URLs have the form: .../XX_chapter_files/figure-html/<label>-1.png
parse_figure_list <- function(md_path = file.path(PROJECT_ROOT, "documentation/list_for_presentation.md")) {
  lines <- readLines(md_path, warn = FALSE)
  # Keep only lines containing a URL to a figure-html PNG
  url_lines <- lines[grepl("figure-html/", lines)]

  figures <- tibble(raw = url_lines) |>
    mutate(
      # Extract the chapter QMD stem from the path (e.g. "02_structure_menages")
      chapter = sub(".*/(\\d{2}_[^/]+)_files/figure-html/.*", "\\1", raw),
      qmd_file = paste0(chapter, ".qmd"),
      # Extract the label from the PNG name: <label>-1.png
      label = sub(".*/figure-html/(.+)-1\\.png.*", "\\1", raw)
    ) |>
    select(qmd_file, label) |>
    distinct()  # remove duplicates (fig-tenure-riz listed twice)

  figures
}

# ── 2. Parse QMD code chunks (reused from calibrate_figures.R) ─

#' Extract figure-producing code chunks from a .qmd file
parse_figure_chunks <- function(qmd_path) {
  lines <- readLines(qmd_path, warn = FALSE)
  chunks <- list()
  in_chunk <- FALSE
  chunk_lines <- character()
  chunk_label <- NULL

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Start of R chunk
    if (!in_chunk && grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      chunk_lines <- character()
      chunk_label <- NULL

      # Check for label in chunk header: ```{r fig_name} or ```{r fig_name,}
      header_label <- sub("^```\\{r\\s+([^,}]+).*", "\\1", line)
      if (header_label != line && header_label != "") {
        chunk_label <- trimws(header_label)
      }
      next
    }

    # End of R chunk
    if (in_chunk && grepl("^```\\s*$", line)) {
      in_chunk <- FALSE
      if (!is.null(chunk_label)) {
        chunks[[chunk_label]] <- chunk_lines
      }
      next
    }

    if (in_chunk) {
      # Check YAML-style label
      if (grepl("^#\\|\\s*label:\\s*", line)) {
        lbl <- sub("^#\\|\\s*label:\\s*", "", line)
        chunk_label <- trimws(lbl)
      }
      # Keep only executable code (skip #| options)
      if (!grepl("^#\\|", line)) {
        chunk_lines <- c(chunk_lines, line)
      }
    }
  }
  chunks
}

#' Extract fig-cap values from chunk YAML options, keyed by label
extract_fig_captions <- function(qmd_path) {
  lines <- readLines(qmd_path, warn = FALSE)
  captions <- list()
  in_chunk <- FALSE
  chunk_label <- NULL
  chunk_cap <- NULL

  for (i in seq_along(lines)) {
    line <- lines[i]

    if (!in_chunk && grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      chunk_label <- NULL
      chunk_cap <- NULL
      header_label <- sub("^```\\{r\\s+([^,}]+).*", "\\1", line)
      if (header_label != line && header_label != "") {
        chunk_label <- trimws(header_label)
      }
      next
    }

    if (in_chunk && grepl("^```\\s*$", line)) {
      in_chunk <- FALSE
      if (!is.null(chunk_label) && !is.null(chunk_cap)) {
        captions[[chunk_label]] <- chunk_cap
      }
      next
    }

    if (in_chunk) {
      if (grepl("^#\\|\\s*label:\\s*", line)) {
        lbl <- sub("^#\\|\\s*label:\\s*", "", line)
        chunk_label <- trimws(lbl)
      }
      if (grepl("^#\\|\\s*fig-cap:\\s*", line)) {
        cap <- sub("^#\\|\\s*fig-cap:\\s*", "", line)
        # Strip surrounding quotes
        cap <- gsub('^["\']|["\']$', "", trimws(cap))
        chunk_cap <- cap
      }
    }
  }
  captions
}

#' Extract all R code from a .qmd file up to (but not including) a target chunk
extract_setup_code <- function(qmd_path, target_label) {
  lines <- readLines(qmd_path, warn = FALSE)
  setup_lines <- character()
  in_chunk <- FALSE
  current_label <- NULL
  chunk_code <- character()

  for (i in seq_along(lines)) {
    line <- lines[i]

    if (!in_chunk && grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      current_label <- NULL
      chunk_code <- character()
      header_label <- sub("^```\\{r\\s+([^,}]+).*", "\\1", line)
      if (header_label != line && header_label != "") {
        current_label <- trimws(header_label)
      }
      next
    }

    if (in_chunk && grepl("^```\\s*$", line)) {
      in_chunk <- FALSE
      if (!is.null(current_label) && current_label == target_label) break
      setup_lines <- c(setup_lines, chunk_code)
      next
    }

    if (in_chunk) {
      if (grepl("^#\\|\\s*label:\\s*", line)) {
        lbl <- sub("^#\\|\\s*label:\\s*", "", line)
        current_label <- trimws(lbl)
      }
      if (!grepl("^#\\|", line)) {
        chunk_code <- c(chunk_code, line)
      }
    }
  }
  setup_lines
}

# ── 3. Figure dimension & font lookup ─────────────────────────

load_figure_sizes <- function() {
  yml_path <- file.path(PROJECT_ROOT, "data/figure_sizes.yml")
  if (file.exists(yml_path)) yaml::read_yaml(yml_path) else list()
}

load_presentation_sizes <- function() {
  yml_path <- file.path(PROJECT_ROOT, "data/presentation_sizes.yml")
  if (file.exists(yml_path)) yaml::read_yaml(yml_path) else list()
}

DEFAULT_FONTS <- list(
  base_size = 14, title = 16, axis_text = 12, axis_title = 13,
  legend_text = 12, legend_title = 13, strip_text = 12, geom_text = 4.5
)

#' Get font settings for a figure: defaults → _defaults → per-figure override
get_pres_fonts <- function(label, pres_sizes = NULL) {
  if (is.null(pres_sizes)) pres_sizes <- load_presentation_sizes()
  # Start with hard-coded defaults

  fonts <- DEFAULT_FONTS
  # Merge global defaults from YAML
  global <- pres_sizes[["_defaults"]]$fonts
  if (!is.null(global)) {
    for (k in names(global)) fonts[[k]] <- global[[k]]
  }
  # Merge per-figure overrides
  fig_fonts <- pres_sizes[[label]]$fonts
  if (!is.null(fig_fonts)) {
    for (k in names(fig_fonts)) fonts[[k]] <- fig_fonts[[k]]
  }
  fonts
}

#' Apply presentation font overrides to a ggplot object
apply_presentation_fonts <- function(p, fonts) {
  if (is.null(p) || !inherits(p, "gg")) return(p)
  p <- p + ggplot2::theme(
    text             = ggplot2::element_text(size = fonts$base_size),
    plot.title       = ggplot2::element_text(size = fonts$title, face = "bold"),
    axis.text        = ggplot2::element_text(size = fonts$axis_text),
    axis.text.x      = ggplot2::element_text(size = fonts$axis_text),
    axis.text.y      = ggplot2::element_text(size = fonts$axis_text),
    axis.title       = ggplot2::element_text(size = fonts$axis_title),
    legend.text      = ggplot2::element_text(size = fonts$legend_text),
    legend.title     = ggplot2::element_text(size = fonts$legend_title),
    strip.text       = ggplot2::element_text(size = fonts$strip_text)
  )
  # Override geom_text / geom_label layer sizes
  for (i in seq_along(p$layers)) {
    geom <- p$layers[[i]]$geom
    if (inherits(geom, "GeomText") || inherits(geom, "GeomLabel")) {
      p$layers[[i]]$aes_params$size <- fonts$geom_text
    }
  }
  p
}

#' Get figure dimensions for a given label and profile
#' Prefers presentation_sizes.yml, falls back to figure_sizes.yml
#' @return list(width, height) in inches
get_fig_dims <- function(label, profile = "marovoay",
                         sizes = NULL, pres_sizes = NULL) {
  if (is.null(sizes)) sizes <- load_figure_sizes()
  if (is.null(pres_sizes)) pres_sizes <- load_presentation_sizes()

  # Prefer presentation sizes (always stored as marovoay base)
  w <- pres_sizes[[label]]$marovoay$width %||%
    sizes[[label]]$marovoay$width %||%
    sizes[[label]]$default$width %||%
    6.5
  h <- pres_sizes[[label]]$marovoay$height %||%
    sizes[[label]]$marovoay$height %||%
    sizes[[label]]$default$height %||%
    5

  if (profile == "alaotra") {
    h <- h * 1.25
  }

  list(width = w, height = h)
}

# ── 4. Environment setup & figure rendering ───────────────────

#' Set up the R environment for a given chapter and profile, then
#' evaluate figure code and return a ggplot object.
render_figure <- function(qmd_file, label, profile) {
  qmd_path <- file.path(PROJECT_ROOT, qmd_file)
  if (!file.exists(qmd_path)) {
    message("  QMD not found: ", qmd_file)
    return(NULL)
  }

  # Parse all chunks from this file
  all_chunks <- parse_figure_chunks(qmd_path)

  if (!label %in% names(all_chunks)) {
    message("  Label '", label, "' not found in ", qmd_file)
    return(NULL)
  }

  figure_code <- all_chunks[[label]]

  # Set profile
  Sys.setenv(QUARTO_PROFILE = profile)

  saved_wd <- setwd(PROJECT_ROOT)
  on.exit(setwd(saved_wd), add = TRUE)

  tryCatch({
    result <- eval(parse(text = paste(figure_code, collapse = "\n")),
                   envir = globalenv())
    if (inherits(result, "gg") || inherits(result, "ggplot")) {
      return(result)
    } else {
      message("  Chunk '", label, "' did not return a ggplot object")
      return(NULL)
    }
  }, error = function(e) {
    message("  Error rendering '", label, "': ", e$message)
    return(NULL)
  })
}

#' Load all setup code for a chapter into the global environment
load_chapter_env <- function(qmd_file, first_label, profile) {
  Sys.setenv(QUARTO_PROFILE = profile)

  saved_wd <- setwd(PROJECT_ROOT)
  on.exit(setwd(saved_wd), add = TRUE)

  # Load base libraries
  suppressPackageStartupMessages({
    library(haven)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(ggplot2)
    library(gt)
    library(forcats)
    library(purrr)
    library(scales)
    library(lubridate)
    library(labelled)
  })

  # Source project utilities into global scope
  source(file.path(PROJECT_ROOT, "utils/report_variant.R"), local = FALSE)
  source(file.path(PROJECT_ROOT, "utils/sites.R"), local = FALSE)
  source(file.path(PROJECT_ROOT, "utils/plot_theme.R"), local = FALSE)
  source(file.path(PROJECT_ROOT, "utils/figure_sizes.R"), local = FALSE)

  tryCatch(
    source(file.path(PROJECT_ROOT, "utils/helpers_report.R"), local = FALSE),
    error = function(e) message("  helpers_report.R: ", e$message)
  )
  tryCatch(
    source(file.path(PROJECT_ROOT, "utils/ror_plots.R"), local = FALSE),
    error = function(e) message("  ror_plots.R: ", e$message)
  )

  dl_path <- file.path(PROJECT_ROOT, "utils/downloadable_output.R")
  if (file.exists(dl_path)) {
    tryCatch(
      source(dl_path, local = FALSE),
      error = function(e) message("  downloadable_output.R: ", e$message)
    )
  }

  # Extract and run setup code (everything before this figure's chunk)
  qmd_path <- file.path(PROJECT_ROOT, qmd_file)
  setup_code <- extract_setup_code(qmd_path, first_label)
  if (length(setup_code) > 0) {
    # Filter problematic lines
    setup_code <- setup_code[!grepl("^\\s*knitr::", setup_code)]
    setup_code <- setup_code[!grepl("knit_print", setup_code)]
    setup_code <- setup_code[!grepl("registerS3method", setup_code)]
    eval(parse(text = paste(setup_code, collapse = "\n")), envir = globalenv())
  }
}

# ── 5. PPTX generation ───────────────────────────────────────

#' Create a landscape PPTX slide deck with one figure per slide
#'
#' @param figures  Tibble with columns qmd_file, label
#' @param profile  "marovoay" or "alaotra"
#' @param title    Presentation title
#' @param output_path  Path for the output .pptx file
generate_pptx <- function(figures, profile, title, output_path) {

  sizes      <- load_figure_sizes()
  pres_sizes <- load_presentation_sizes()

  # Landscape slide: 13.33 × 7.5 inches (widescreen)
  slide_w <- 13.33
  slide_h <- 7.5

  pptx <- read_pptx()
  # Set slide size to landscape widescreen
  pptx <- pptx |>
    add_slide(layout = "Title Slide", master = "Office Theme") |>
    ph_with(value = title,
            location = ph_location_type(type = "ctrTitle")) |>
    ph_with(value = "Restitution locale — Campagne 2025",
            location = ph_location_type(type = "subTitle"))

  # Group figures by chapter so we load each chapter's data once
  chapter_groups <- split(figures, figures$qmd_file)

  for (qmd_file in names(chapter_groups)) {
    chapter_figs <- chapter_groups[[qmd_file]]
    first_label <- chapter_figs$label[1]

    # Extract fig-cap captions for this chapter
    qmd_path <- file.path(PROJECT_ROOT, qmd_file)
    captions <- if (file.exists(qmd_path)) extract_fig_captions(qmd_path) else list()

    message("\n=== Loading chapter: ", qmd_file, " (profile: ", profile, ") ===")
    tryCatch(
      load_chapter_env(qmd_file, first_label, profile),
      error = function(e) message("  Setup error: ", e$message)
    )

    # For subsequent figures in the same chapter, we need incremental setup.
    # Load all setup code up to each figure in order.
    for (i in seq_len(nrow(chapter_figs))) {
      label <- chapter_figs$label[i]
      message("  Rendering: ", label)

      # If not the first figure, load additional setup between prev and this
      if (i > 1) {
        tryCatch({
          Sys.setenv(QUARTO_PROFILE = profile)
          saved_wd <- setwd(PROJECT_ROOT)
          on.exit(setwd(saved_wd), add = TRUE)

          # Get incremental code between previous and current figure
          qmd_path <- file.path(PROJECT_ROOT, qmd_file)
          setup_code <- extract_setup_code(qmd_path, label)
          prev_setup <- extract_setup_code(qmd_path, chapter_figs$label[i - 1])
          # Only run lines after what we already ran
          if (length(setup_code) > length(prev_setup)) {
            new_code <- setup_code[(length(prev_setup) + 1):length(setup_code)]
            new_code <- new_code[!grepl("^\\s*knitr::", new_code)]
            new_code <- new_code[!grepl("knit_print", new_code)]
            new_code <- new_code[!grepl("registerS3method", new_code)]
            if (length(new_code) > 0) {
              eval(parse(text = paste(new_code, collapse = "\n")),
                   envir = globalenv())
            }
          }
        }, error = function(e) {
          message("  Incremental setup error for '", label, "': ", e$message)
        })
      }

      # Render the figure
      p <- render_figure(qmd_file, label, profile)

      if (!is.null(p)) {
        # Apply presentation font overrides
        fonts <- get_pres_fonts(label, pres_sizes)
        p <- apply_presentation_fonts(p, fonts)

        dims <- get_fig_dims(label, profile, sizes, pres_sizes)

        # Scale to fit slide, preserving aspect ratio
        margin_top <- 1.5  # space for title
        avail_w <- slide_w          # full width, no horizontal margin
        avail_h <- slide_h - margin_top

        scale_factor <- min(avail_w / dims$width, avail_h / dims$height, 1.5)
        plot_w <- dims$width * scale_factor
        plot_h <- dims$height * scale_factor

        # Left-align, no margin
        left <- 0
        top <- margin_top

        # Use fig-cap as slide title, fall back to label
        slide_title <- captions[[label]] %||% label

        pptx <- pptx |>
          add_slide(layout = "Title Only", master = "Office Theme") |>
          ph_with(value = slide_title,
                  location = ph_location_type(type = "title")) |>
          ph_with(value = dml(ggobj = p),
                  location = ph_location(
                    left = left, top = top,
                    width = plot_w, height = plot_h
                  ))

        message("    \u2713 Added (", round(plot_w, 1), " \u00d7 ", round(plot_h, 1), " in)")
      } else {
        # Add a placeholder slide
        slide_title <- captions[[label]] %||% label
        pptx <- pptx |>
          add_slide(layout = "Title Only", master = "Office Theme") |>
          ph_with(value = slide_title,
                  location = ph_location_type(type = "title")) |>
          ph_with(value = paste("Figure could not be rendered:", label),
                  location = ph_location(left = 2, top = 3,
                                         width = 8, height = 2))
        message("    ✗ Placeholder slide added")
      }
    }
  }

  # Save
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  print(pptx, target = output_path)
  message("\n=== Saved: ", output_path, " ===")
}


# ── 6. XLSX generation ────────────────────────────────────────

#' Create one XLSX workbook per observatory with one sheet per figure
#'
#' For each presentation figure, renders the ggplot and extracts its
#' underlying data frame (p$data).  Applies suppress_for_export() for
#' confidentiality, then writes all sheets into a single XLSX file.
#'
#' @param figures     Tibble with columns qmd_file, label
#' @param profile     "marovoay" or "alaotra"
#' @param output_path Path for the output .xlsx file
generate_xlsx <- function(figures, profile, output_path) {

  sheets <- list()

  # Group figures by chapter so we load each chapter's data once
  chapter_groups <- split(figures, figures$qmd_file)

  for (qmd_file in names(chapter_groups)) {
    chapter_figs <- chapter_groups[[qmd_file]]
    first_label  <- chapter_figs$label[1]

    message("\n=== Loading chapter: ", qmd_file, " (profile: ", profile, ") ===")
    tryCatch(
      load_chapter_env(qmd_file, first_label, profile),
      error = function(e) message("  Setup error: ", e$message)
    )

    for (i in seq_len(nrow(chapter_figs))) {
      label <- chapter_figs$label[i]
      message("  Extracting data: ", label)

      # Incremental setup for subsequent figures in same chapter
      if (i > 1) {
        tryCatch({
          Sys.setenv(QUARTO_PROFILE = profile)
          saved_wd <- setwd(PROJECT_ROOT)
          on.exit(setwd(saved_wd), add = TRUE)

          qmd_path   <- file.path(PROJECT_ROOT, qmd_file)
          setup_code <- extract_setup_code(qmd_path, label)
          prev_setup <- extract_setup_code(qmd_path, chapter_figs$label[i - 1])

          if (length(setup_code) > length(prev_setup)) {
            new_code <- setup_code[(length(prev_setup) + 1):length(setup_code)]
            new_code <- new_code[!grepl("^\\s*knitr::", new_code)]
            new_code <- new_code[!grepl("knit_print", new_code)]
            new_code <- new_code[!grepl("registerS3method", new_code)]
            if (length(new_code) > 0)
              eval(parse(text = paste(new_code, collapse = "\n")),
                   envir = globalenv())
          }
        }, error = function(e) {
          message("  Incremental setup error for '", label, "': ", e$message)
        })
      }

      # Render the figure
      p <- render_figure(qmd_file, label, profile)

      if (!is.null(p) && inherits(p, "gg")) {
        df <- p$data
        if (is.data.frame(df) && nrow(df) > 0) {
          # Apply confidentiality suppression
          df <- suppress_for_export(df)
          # Excel sheet names: max 31 chars, no special chars
          sheet_name <- substr(gsub("[\\[\\]\\*\\?\\/\\\\:]", "_", label), 1, 31)
          sheets[[sheet_name]] <- df
          message("    \u2713 Data extracted (",
                  nrow(df), " rows, ", ncol(df), " cols)")
        } else {
          message("    \u2717 No data in plot")
        }
      } else {
        message("    \u2717 Could not render figure")
      }
    }
  }

  if (length(sheets) > 0) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writexl::write_xlsx(sheets, output_path)
    message("\n=== Saved: ", output_path, " (", length(sheets), " sheets) ===")
  } else {
    message("\n=== No data to export ===")
  }
}

# ── 7. DOCX generation ─────────────────────────────────────────

#' Create a landscape DOCX document with one figure per page
#'
#' Uses the same display settings (dimensions, fonts) as the PPTX
#' slides so you can verify how figures look in a Word document.
#'
#' @param figures     Tibble with columns qmd_file, label
#' @param profile     "marovoay" or "alaotra"
#' @param title       Document title
#' @param output_path Path for the output .docx file
generate_docx <- function(figures, profile, title, output_path) {

  sizes      <- load_figure_sizes()
  pres_sizes <- load_presentation_sizes()

  # Portrait A4 usable area (margins 0.5 in)
  margin  <- 0.5
  avail_w <-  8.27 - 2 * margin        # ~7.27 in
  avail_h <- 11.69 - 2 * margin - 0.8  # ~9.89 in (allow for heading)

  docx <- read_docx()

  # Title page
  docx <- docx |>
    body_add_par(title, style = "heading 1") |>
    body_add_par("Restitution locale \u2014 Campagne 2025") |>
    body_add_break()

  chapter_groups <- split(figures, figures$qmd_file)
  is_first_figure <- TRUE

  for (qmd_file in names(chapter_groups)) {
    chapter_figs <- chapter_groups[[qmd_file]]
    first_label  <- chapter_figs$label[1]

    qmd_path <- file.path(PROJECT_ROOT, qmd_file)
    captions <- if (file.exists(qmd_path)) extract_fig_captions(qmd_path) else list()

    message("\n=== Loading chapter: ", qmd_file, " (profile: ", profile, ") ===")
    tryCatch(
      load_chapter_env(qmd_file, first_label, profile),
      error = function(e) message("  Setup error: ", e$message)
    )

    for (i in seq_len(nrow(chapter_figs))) {
      label <- chapter_figs$label[i]
      message("  Rendering: ", label)

      # Incremental setup for subsequent figures in same chapter
      if (i > 1) {
        tryCatch({
          Sys.setenv(QUARTO_PROFILE = profile)
          saved_wd <- setwd(PROJECT_ROOT)
          on.exit(setwd(saved_wd), add = TRUE)

          qmd_path   <- file.path(PROJECT_ROOT, qmd_file)
          setup_code <- extract_setup_code(qmd_path, label)
          prev_setup <- extract_setup_code(qmd_path, chapter_figs$label[i - 1])

          if (length(setup_code) > length(prev_setup)) {
            new_code <- setup_code[(length(prev_setup) + 1):length(setup_code)]
            new_code <- new_code[!grepl("^\\s*knitr::", new_code)]
            new_code <- new_code[!grepl("knit_print", new_code)]
            new_code <- new_code[!grepl("registerS3method", new_code)]
            if (length(new_code) > 0) {
              eval(parse(text = paste(new_code, collapse = "\n")),
                   envir = globalenv())
            }
          }
        }, error = function(e) {
          message("  Incremental setup error for '", label, "': ", e$message)
        })
      }

      # Render the figure
      p <- render_figure(qmd_file, label, profile)

      if (!is.null(p)) {
        fonts <- get_pres_fonts(label, pres_sizes)
        p <- apply_presentation_fonts(p, fonts)

        dims <- get_fig_dims(label, profile, sizes, pres_sizes)

        # Shrink to fit page if needed, but never enlarge beyond calibrated size
        scale_factor <- min(avail_w / dims$width, avail_h / dims$height, 1.0)
        plot_w <- dims$width * scale_factor
        plot_h <- dims$height * scale_factor

        fig_title <- captions[[label]] %||% label

        # Page break before each figure (first one follows title page break)
        if (!is_first_figure) {
          docx <- body_add_break(docx)
        }
        is_first_figure <- FALSE

        docx <- docx |>
          body_add_par(fig_title, style = "heading 2") |>
          body_add_gg(value = p, width = plot_w, height = plot_h)

        message("    \u2713 Added (", round(plot_w, 1), " \u00d7 ",
                round(plot_h, 1), " in)")
      } else {
        fig_title <- captions[[label]] %||% label

        if (!is_first_figure) {
          docx <- body_add_break(docx)
        }
        is_first_figure <- FALSE

        docx <- docx |>
          body_add_par(fig_title, style = "heading 2") |>
          body_add_par(paste("Figure could not be rendered:", label))

        message("    \u2717 Placeholder added")
      }
    }
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  print(docx, target = output_path)
  message("\n=== Saved: ", output_path, " ===")
}

# ── 8. Main ──────────────────────────────────────────────────

main <- function() {
  figures <- parse_figure_list()
  message("Found ", nrow(figures), " figures to render across ",
          n_distinct(figures$qmd_file), " chapters\n")

  # # Marovoay presentation
  # generate_pptx(
  #   figures = figures,
  #   profile = "marovoay",
  #   title = "Campagne 2025 de l\u2019Observatoire rural de Marovoay",
  #   output_path = file.path(PROJECT_ROOT, "output/presentation_marovoay.pptx")
  # )
  #
  # # Alaotra presentation
  # generate_pptx(
  #   figures = figures,
  #   profile = "alaotra",
  #   title = "Campagne 2025 de l\u2019Observatoire rural de l\u2019Alaotra",
  #   output_path = file.path(PROJECT_ROOT, "output/presentation_alaotra.pptx")
  # )
  #
  # # ── XLSX data exports (one workbook per observatory) ──
  #
  # # Marovoay XLSX
  # generate_xlsx(
  #   figures = figures,
  #   profile = "marovoay",
  #   output_path = file.path(PROJECT_ROOT, "output/presentation_data_marovoay.xlsx")
  # )
  #
  # # Alaotra XLSX
  # generate_xlsx(
  #   figures = figures,
  #   profile = "alaotra",
  #   output_path = file.path(PROJECT_ROOT, "output/presentation_data_alaotra.xlsx")
  # )

  # ── DOCX proof documents (same display settings as PPTX) ──

  # Marovoay DOCX
  generate_docx(
    figures = figures,
    profile = "marovoay",
    title = "Campagne 2025 de l\u2019Observatoire rural de Marovoay",
    output_path = file.path(PROJECT_ROOT, "output/presentation_marovoay.docx")
  )

  # Alaotra DOCX
  generate_docx(
    figures = figures,
    profile = "alaotra",
    title = "Campagne 2025 de l\u2019Observatoire rural de l\u2019Alaotra",
    output_path = file.path(PROJECT_ROOT, "output/presentation_alaotra.docx")
  )

  message("\nDone! Presentations saved in output/")
}

if (interactive()) {
  main()
}
