# utils/calibrate_figures.R
# ─────────────────────────────────────────────────────────────────
# Shiny app for interactively calibrating figure dimensions.
#
# Usage:
#   source("utils/calibrate_figures.R")   # or click "Run App" in RStudio/Positron
#
# The app parses all .qmd files, extracts figure-producing code chunks,
# and lets you preview each figure at adjustable height × width for each
# profile (consolidated, marovoay, alaotra).  Dimensions are saved to
# data/figure_sizes.yml.
# ─────────────────────────────────────────────────────────────────

library(shiny)
library(yaml)
library(ggplot2)
library(dplyr)

# ── 0. Project root detection ─────────────────────────────────
# shiny::runApp() changes wd to the app directory (utils/),
# so we need the project root for all relative paths.
PROJECT_ROOT <- normalizePath(
  file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
  winslash = "/", mustWork = FALSE
)
# Fallback: if .Rproj file is visible from wd or parent
if (!file.exists(file.path(PROJECT_ROOT, "_quarto.yaml"))) {
  for (candidate in c(".", "..", "../..")) {
    if (file.exists(file.path(candidate, "_quarto.yaml"))) {
      PROJECT_ROOT <- normalizePath(candidate, winslash = "/")
      break
    }
  }
}
message("Project root: ", PROJECT_ROOT)

# ── 1. Parse .qmd files for figure chunks ─────────────────────

#' Extract figure-producing code chunks from a .qmd file
#' @param qmd_path Path to the .qmd file
#' @return A list of lists, each with: label, code (character vector),
#'         file (basename), line (start line number)
parse_figure_chunks <- function(qmd_path) {
  lines <- readLines(qmd_path, warn = FALSE)
  chunks <- list()
  in_chunk <- FALSE
  chunk_start <- NULL
  chunk_lines <- character()
  chunk_label <- NULL
  chunk_has_fig <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Start of R chunk
    if (!in_chunk && grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      chunk_start <- i
      chunk_lines <- character()
      chunk_label <- NULL
      chunk_has_fig <- FALSE

      # Check for label in chunk header: ```{r fig_name} or ```{r label="fig_name"}
      header_label <- sub("^```\\{r\\s+([^,}]+).*", "\\1", line)
      if (header_label != line && header_label != "") {
        header_label <- trimws(header_label)
        if (grepl("^fig[_-]", header_label)) {
          chunk_label <- header_label
          chunk_has_fig <- TRUE
        }
      }
      next
    }

    # End of R chunk
    if (in_chunk && grepl("^```\\s*$", line)) {
      in_chunk <- FALSE
      if (chunk_has_fig && !is.null(chunk_label)) {
        chunks <- c(chunks, list(list(
          label = chunk_label,
          code  = chunk_lines,
          file  = basename(qmd_path),
          line  = chunk_start
        )))
      }
      next
    }

    # Inside a chunk
    if (in_chunk) {
      # Check YAML-style chunk options
      if (grepl("^#\\|\\s*label:\\s*", line)) {
        lbl <- sub("^#\\|\\s*label:\\s*", "", line)
        lbl <- trimws(lbl)
        if (grepl("^fig[_-]", lbl) || grepl("^g\\d+_", lbl)) {
          chunk_label <- lbl
          chunk_has_fig <- TRUE
        }
      }
      # Skip chunk option lines from the executable code
      if (!grepl("^#\\|", line)) {
        chunk_lines <- c(chunk_lines, line)
      }
    }
  }
  chunks
}

#' Parse all .qmd files and return a combined chunk inventory
parse_all_chunks <- function(project_dir = PROJECT_ROOT) {
  qmd_files <- list.files(project_dir, pattern = "^\\d+.*\\.qmd$",
                           full.names = TRUE)
  all_chunks <- list()
  for (f in qmd_files) {
    all_chunks <- c(all_chunks, parse_figure_chunks(f))
  }
  all_chunks
}


# ── 2. Chapter setup code extraction ──────────────────────────

#' Extract all R code from a .qmd file up to (but not including) a target chunk
#' This gives us the data-loading code needed before rendering a figure.
#' @param qmd_path Path to the .qmd file
#' @param target_label The label of the target figure chunk
#' @return Character vector of R code lines (setup code)
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
      # Check header label
      header_label <- sub("^```\\{r\\s+([^,}]+).*", "\\1", line)
      if (header_label != line && header_label != "") {
        current_label <- trimws(header_label)
      }
      next
    }

    if (in_chunk && grepl("^```\\s*$", line)) {
      in_chunk <- FALSE
      # If this was the target chunk, stop — don't include its code in setup
      if (!is.null(current_label) && current_label == target_label) break

      # Include this chunk's code in setup (data loading, transformations, etc.)
      # Skip pure display chunks (only ggplot calls with no data prep)
      setup_lines <- c(setup_lines, chunk_code)
      next
    }

    if (in_chunk) {
      # Check YAML-style label
      if (grepl("^#\\|\\s*label:\\s*", line)) {
        lbl <- sub("^#\\|\\s*label:\\s*", "", line)
        current_label <- trimws(lbl)
      }
      # Skip chunk options but keep code
      if (!grepl("^#\\|", line)) {
        chunk_code <- c(chunk_code, line)
      }
    }
  }
  setup_lines
}


# ── 3. Shiny app ─────────────────────────────────────────────

PROFILES <- c("consolidated", "marovoay", "alaotra")
YML_PATH <- file.path(PROJECT_ROOT, "data/figure_sizes.yml")

ui <- fluidPage(
  titlePanel("Figure Dimension Calibrator"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("chapter", "Chapter",
                  choices = NULL),
      selectInput("figure", "Figure",
                  choices = NULL),
      selectInput("profile", "Profile",
                  choices = PROFILES, selected = "consolidated"),
      hr(),
      sliderInput("height", "Height (inches)",
          min = 2, max = 20, value = 5, step = 0.5),
      sliderInput("width", "Width (inches)",
          min = 4, max = 14, value = 8, step = 0.5),
      hr(),
      actionButton("save_btn", "Save dimensions",
                   class = "btn-primary", width = "100%"),
      hr(),
      actionButton("prev_btn", "← Previous", width = "48%",
                   style = "display:inline-block"),
      actionButton("next_btn", "Next →", width = "48%",
                   style = "display:inline-block"),
      hr(),
      verbatimTextOutput("status"),
      hr(),
      h5("Saved YAML values for this figure:"),
      verbatimTextOutput("yaml_preview")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Preview",
          plotOutput("fig_preview", height = "auto"),
          hr(),
          verbatimTextOutput("render_log")
        ),
        tabPanel("Code",
          verbatimTextOutput("chunk_code")
        ),
        tabPanel("All figures",
          DT::dataTableOutput("all_figs_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # --- Parse chunks on startup ---
  all_chunks <- parse_all_chunks(PROJECT_ROOT)
  chunk_labels <- vapply(all_chunks, `[[`, character(1), "label")
  chunk_files  <- vapply(all_chunks, `[[`, character(1), "file")
  chapters     <- unique(chunk_files)

  message("Found ", length(all_chunks), " figure chunks in ", length(chapters), " chapters")

  if (length(chapters) > 0) {
    updateSelectInput(session, "chapter", choices = chapters,
                      selected = chapters[1])
  }

  # --- Filter figures by chapter ---
  observeEvent(input$chapter, {
    idx <- which(chunk_files == input$chapter)
    labels <- chunk_labels[idx]
    updateSelectInput(session, "figure", choices = labels,
                      selected = labels[1])
  })

  # --- Load YAML sizes ---
  sizes <- reactiveVal({
    if (file.exists(YML_PATH)) yaml::read_yaml(YML_PATH) else list()
  })

  # --- Update sliders when figure/profile changes ---
  observeEvent(list(input$figure, input$profile), {
    req(input$figure)
        s <- sizes()
        profile <- input$profile
        # height: prefer profile-specific, then figure default, then global default 5
        h <- s[[input$figure]][[profile]]$height %||%
          s[[input$figure]]$default$height %||% 5
        # width: prefer profile-specific, then figure default, then profile-level default
        profile_default_w <- if (profile == "consolidated") 8 else 6.5
        w <- s[[input$figure]][[profile]]$width %||%
          s[[input$figure]]$default$width %||% profile_default_w
    updateSliderInput(session, "height", value = h)
    updateSliderInput(session, "width", value = w)
  })

  # --- Preview figure ---
  # Track the current environment with loaded data per chapter+profile
  data_env <- reactiveVal(new.env(parent = globalenv()))
  loaded_key <- reactiveVal("")

  # Set up environment for the chapter + profile
  setup_env <- reactive({
    req(input$figure, input$profile, input$chapter)

    key <- paste(input$chapter, input$profile, sep = "::")
    if (key == loaded_key()) return(data_env())

    # Set profile env var
    Sys.setenv(QUARTO_PROFILE = input$profile)

    # Create a fresh environment
    env <- new.env(parent = globalenv())

    # Temporarily switch wd to project root so that source() / readRDS()
    # calls in setup code resolve correctly (they use relative paths).
    saved_wd <- setwd(PROJECT_ROOT)
    on.exit(setwd(saved_wd), add = TRUE)

    # Source common setup in this env
    tryCatch({
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
      })

      # Source project utilities into the global env so they're visible
      # both inside env and in the calling scope
      source(file.path(PROJECT_ROOT, "utils/report_variant.R"), local = FALSE)
      source(file.path(PROJECT_ROOT, "utils/sites.R"), local = FALSE)
      source(file.path(PROJECT_ROOT, "utils/plot_theme.R"), local = FALSE)
      source(file.path(PROJECT_ROOT, "utils/figure_sizes.R"), local = FALSE)
      env$REPORT_MODE <- get_report_mode()

      # Source helpers into global too (they define utility functions)
      tryCatch(
        source(file.path(PROJECT_ROOT, "utils/helpers_report.R"), local = FALSE),
        error = function(e) message("helpers_report.R: ", e$message)
      )
      tryCatch(
        source(file.path(PROJECT_ROOT, "utils/ror_plots.R"), local = FALSE),
        error = function(e) message("ror_plots.R: ", e$message)
      )

      # Try downloadable_output.R (some chapters use it)
      dl_path <- file.path(PROJECT_ROOT, "utils/downloadable_output.R")
      if (file.exists(dl_path)) {
        tryCatch(
          source(dl_path, local = FALSE),
          error = function(e) message("downloadable_output.R: ", e$message)
        )
      }

      # Extract and run setup code for this figure
      qmd_path <- file.path(PROJECT_ROOT, input$chapter)
      setup_code <- extract_setup_code(qmd_path, input$figure)
      if (length(setup_code) > 0) {
        # Filter out lines that would cause issues in non-knitr context
        setup_code <- setup_code[!grepl("^\\s*knitr::", setup_code)]
        setup_code <- setup_code[!grepl("knit_print", setup_code)]
        setup_code <- setup_code[!grepl("registerS3method", setup_code)]
        # Run in the global env so all objects are accessible
        eval(parse(text = paste(setup_code, collapse = "\n")), envir = globalenv())
      }
    }, error = function(e) {
      message("Setup error: ", e$message)
    })

    data_env(env)
    loaded_key(key)
    env
  })

  # Dynamic plot height
  plot_height <- reactive({
    input$height * 96  # 96 px per inch for screen
  })

  output$fig_preview <- renderPlot({
    req(input$figure)
    env <- setup_env()

    # Find the chunk
    idx <- which(chunk_labels == input$figure)
    if (length(idx) == 0) return(NULL)
    chunk <- all_chunks[[idx[1]]]

    # Execute the figure code
    code <- paste(chunk$code, collapse = "\n")
    tryCatch({
      result <- eval(parse(text = code), envir = globalenv())
      if (inherits(result, "gg") || inherits(result, "ggplot")) {
        result
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:\n", e$message),
           cex = 0.9, col = "red")
    })
  }, height = function() plot_height(),
     width = function() input$width * 96)

  # --- Render log ---
  output$render_log <- renderPrint({
    req(input$figure)
    idx <- which(chunk_labels == input$figure)
    if (length(idx) == 0) return("No chunk found")
    chunk <- all_chunks[[idx[1]]]
    cat(sprintf("File: %s (line %d)\nLabel: %s\nProfile: %s\nDimensions: %.1f × %.1f in\n",
                chunk$file, chunk$line, chunk$label,
                input$profile, input$width, input$height))
  })

  # --- Show chunk code ---
  output$chunk_code <- renderPrint({
    req(input$figure)
    idx <- which(chunk_labels == input$figure)
    if (length(idx) == 0) return("No chunk found")
    cat(paste(all_chunks[[idx[1]]]$code, collapse = "\n"))
  })

  # --- Save dimensions ---
  observeEvent(input$save_btn, {
    s <- sizes()
    label <- input$figure
    profile <- input$profile

    if (is.null(s[[label]])) s[[label]] <- list()
    s[[label]][[profile]] <- list(
      height = input$height,
      width  = input$width
    )
    sizes(s)

    # Write YAML
    yaml::write_yaml(s, YML_PATH)
    showNotification(
      sprintf("Saved %s / %s: h=%.1f, w=%.1f",
              label, profile, input$height, input$width),
      type = "message"
    )
  })

  # --- Navigation ---
  observeEvent(input$prev_btn, {
    req(input$figure, input$chapter)
    idx <- which(chunk_files == input$chapter)
    labels <- chunk_labels[idx]
    pos <- which(labels == input$figure)
    if (length(pos) > 0 && pos > 1) {
      updateSelectInput(session, "figure", selected = labels[pos - 1])
    } else {
      # Go to previous chapter
      ch_pos <- which(chapters == input$chapter)
      if (ch_pos > 1) {
        prev_ch <- chapters[ch_pos - 1]
        prev_idx <- which(chunk_files == prev_ch)
        prev_labels <- chunk_labels[prev_idx]
        updateSelectInput(session, "chapter", selected = prev_ch)
        # Will be picked up by the chapter observer
      }
    }
  })

  observeEvent(input$next_btn, {
    req(input$figure, input$chapter)
    idx <- which(chunk_files == input$chapter)
    labels <- chunk_labels[idx]
    pos <- which(labels == input$figure)
    if (length(pos) > 0 && pos < length(labels)) {
      updateSelectInput(session, "figure", selected = labels[pos + 1])
    } else {
      # Go to next chapter
      ch_pos <- which(chapters == input$chapter)
      if (ch_pos < length(chapters)) {
        next_ch <- chapters[ch_pos + 1]
        updateSelectInput(session, "chapter", selected = next_ch)
      }
    }
  })

  # --- Status ---
  output$status <- renderText({
    req(input$figure, input$chapter)
    idx <- which(chunk_files == input$chapter)
    labels <- chunk_labels[idx]
    pos <- which(labels == input$figure)
    total <- length(chunk_labels)
    global_pos <- which(chunk_labels == input$figure)
    sprintf("Figure %d/%d in chapter (%d/%d overall)", pos, length(labels),
            global_pos[1], total)
  })

  # --- YAML preview ---
  output$yaml_preview <- renderPrint({
    req(input$figure)
    s <- sizes()
    entry <- s[[input$figure]]
    if (is.null(entry)) {
      cat("(no saved values)")
    } else {
      cat(yaml::as.yaml(setNames(list(entry), input$figure)))
    }
  })

  # --- All figures table ---
  output$all_figs_table <- DT::renderDataTable({
    s <- sizes()
    df <- data.frame(
      File    = chunk_files,
      Label   = chunk_labels,
      stringsAsFactors = FALSE
    )
    # Add current saved dimensions
    for (p in PROFILES) {
      df[[paste0(p, "_h")]] <- vapply(chunk_labels, function(lbl) {
        s[[lbl]][[p]]$height %||% s[[lbl]]$default$height %||% NA_real_
      }, numeric(1))
      df[[paste0(p, "_w")]] <- vapply(chunk_labels, function(lbl) {
        s[[lbl]][[p]]$width %||% s[[lbl]]$default$width %||% NA_real_
      }, numeric(1))
    }
    DT::datatable(df, options = list(pageLength = 50),
                  selection = "single", rownames = FALSE)
  })

  # Click on table row → navigate to that figure
  observeEvent(input$all_figs_table_rows_selected, {
    row <- input$all_figs_table_rows_selected
    if (length(row) == 1) {
      updateSelectInput(session, "chapter", selected = chunk_files[row])
      # Delay to let chapter observer fire
      shinyjs::delay(200, {
        updateSelectInput(session, "figure", selected = chunk_labels[row])
      })
    }
  })
}

# ── Launch ────────────────────────────────────────────────────
if (interactive() || identical(Sys.getenv("SHINY_APP"), "1")) {
  shinyApp(ui, server)
}
