# utils/calibrate_presentation.R
# ─────────────────────────────────────────────────────────────────
# Shiny app for calibrating presentation figure dimensions AND
# font sizes (legend, axes, strip text, bar labels, etc.).
#
# Only includes the 38 figures listed in
# documentation/list_for_presentation.md (local restitution).
#
# Settings are saved to data/presentation_sizes.yml
# (separate from the report's figure_sizes.yml).
#
# Usage:
#   source("utils/calibrate_presentation.R")
# ─────────────────────────────────────────────────────────────────

library(shiny)
library(yaml)
library(ggplot2)
library(dplyr)

# ── 0. Project root ─────────────────────────────────────────

PROJECT_ROOT <- normalizePath(
  file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
  winslash = "/", mustWork = FALSE
)
if (!file.exists(file.path(PROJECT_ROOT, "_quarto.yaml"))) {
  for (candidate in c(".", "..", "../..")) {
    if (file.exists(file.path(candidate, "_quarto.yaml"))) {
      PROJECT_ROOT <- normalizePath(candidate, winslash = "/")
      break
    }
  }
}
message("Project root: ", PROJECT_ROOT)

# ── 1. Parse presentation figure list ───────────────────────

parse_figure_list <- function(md_path = file.path(PROJECT_ROOT,
    "documentation/list_for_presentation.md")) {
  lines <- readLines(md_path, warn = FALSE)
  url_lines <- lines[grepl("figure-html/", lines)]
  tibble(raw = url_lines) |>
    mutate(
      chapter = sub(".*/(\\d{2}_[^/]+)_files/figure-html/.*", "\\1", raw),
      qmd_file = paste0(chapter, ".qmd"),
      label = sub(".*/figure-html/(.+)-1\\.png.*", "\\1", raw)
    ) |>
    select(qmd_file, label) |>
    distinct()
}

# ── 2. QMD parsing ──────────────────────────────────────────

parse_figure_chunks <- function(qmd_path) {
  lines <- readLines(qmd_path, warn = FALSE)
  chunks <- list()
  in_chunk <- FALSE
  chunk_lines <- character()
  chunk_label <- NULL

  for (i in seq_along(lines)) {
    line <- lines[i]
    if (!in_chunk && grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      chunk_lines <- character()
      chunk_label <- NULL
      header_label <- sub("^```\\{r\\s+([^,}]+).*", "\\1", line)
      if (header_label != line && header_label != "")
        chunk_label <- trimws(header_label)
      next
    }
    if (in_chunk && grepl("^```\\s*$", line)) {
      in_chunk <- FALSE
      if (!is.null(chunk_label)) chunks[[chunk_label]] <- chunk_lines
      next
    }
    if (in_chunk) {
      if (grepl("^#\\|\\s*label:\\s*", line))
        chunk_label <- trimws(sub("^#\\|\\s*label:\\s*", "", line))
      if (!grepl("^#\\|", line)) chunk_lines <- c(chunk_lines, line)
    }
  }
  chunks
}

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
      if (header_label != line && header_label != "")
        current_label <- trimws(header_label)
      next
    }
    if (in_chunk && grepl("^```\\s*$", line)) {
      in_chunk <- FALSE
      if (!is.null(current_label) && current_label == target_label) break
      setup_lines <- c(setup_lines, chunk_code)
      next
    }
    if (in_chunk) {
      if (grepl("^#\\|\\s*label:\\s*", line))
        current_label <- trimws(sub("^#\\|\\s*label:\\s*", "", line))
      if (!grepl("^#\\|", line)) chunk_code <- c(chunk_code, line)
    }
  }
  setup_lines
}

# ── 3. Font override mechanism ──────────────────────────────

DEFAULT_FONTS <- list(
  base_size    = 14,
  title        = 16,
  axis_text    = 12,
  axis_title   = 13,
  legend_text  = 12,
  legend_title = 13,
  strip_text   = 12,
  geom_text    = 4.5   # ggplot2 mm units (report default ≈ 3.2)
)

#' Apply presentation font overrides to a ggplot object
#'
#' Overrides theme text elements and modifies geom_text/geom_label
#' layer sizes.  Call on a freshly-created ggplot (not a cached one)
#' because geom layer modification is done in place.
#'
#' @param p A ggplot object
#' @param fonts Named list of font sizes (see DEFAULT_FONTS)
#' @return The modified ggplot
apply_presentation_fonts <- function(p, fonts) {
  if (is.null(p) || !inherits(p, "gg")) return(p)

  # Theme overrides (additive: later theme() calls override earlier)
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

  # Override geom_text / geom_label size in each layer
  if (!is.null(fonts$geom_text)) {
    for (i in seq_along(p$layers)) {
      geom <- p$layers[[i]]$geom
      if (inherits(geom, "GeomText") || inherits(geom, "GeomLabel")) {
        p$layers[[i]]$aes_params$size <- fonts$geom_text
      }
    }
  }
  p
}

# ── 4. Dimension helpers ────────────────────────────────────

load_report_sizes <- function() {
  yml <- file.path(PROJECT_ROOT, "data/figure_sizes.yml")
  if (file.exists(yml)) yaml::read_yaml(yml) else list()
}

#' Fallback dimensions from the report's figure_sizes.yml
get_default_dims <- function(label, profile, report_sizes = NULL) {
  if (is.null(report_sizes)) report_sizes <- load_report_sizes()
  w <- report_sizes[[label]]$marovoay$width %||%
    report_sizes[[label]]$default$width %||% 6.5
  h <- report_sizes[[label]]$marovoay$height %||%
    report_sizes[[label]]$default$height %||% 5
  if (profile == "alaotra") h <- h * 1.25
  list(width = w, height = h)
}

# ── 5. Paths & constants ───────────────────────────────────

PRES_YML <- file.path(PROJECT_ROOT, "data/presentation_sizes.yml")
PROFILES <- c("marovoay", "alaotra")

# ── 6. UI ───────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("Presentation Figure Calibrator"),
  tags$head(tags$style(HTML("
    .font-row { margin-bottom: 4px; }
    .font-row .form-group { margin-bottom: 2px; }
    .btn-nav { font-size: 13px; }
  "))),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("chapter", "Chapter", choices = NULL),
      selectInput("figure", "Figure", choices = NULL),
      selectInput("profile", "Profile",
                  choices = PROFILES, selected = "marovoay"),
      fluidRow(
        column(6, actionButton("prev_btn", "\u25c0 Prev",
                               class = "btn-nav", width = "100%")),
        column(6, actionButton("next_btn", "Next \u25b6",
                               class = "btn-nav", width = "100%"))
      ),

      hr(),
      h4("Dimensions (inches)"),
      sliderInput("width", "Width",
                  min = 3, max = 16, value = 6.5, step = 0.5),
      sliderInput("height", "Height",
                  min = 2, max = 20, value = 5, step = 0.5),

      hr(),
      h4("Font sizes"),
      checkboxInput("fonts_global", "Apply fonts to ALL figures",
                    value = FALSE),
      div(class = "font-row", fluidRow(
        column(6, numericInput("font_base", "Base size", 14,
                               min = 6, max = 30, step = 1)),
        column(6, numericInput("font_title", "Title", 16,
                               min = 6, max = 36, step = 1))
      )),
      div(class = "font-row", fluidRow(
        column(6, numericInput("font_axis_text", "Axis tick labels", 12,
                               min = 4, max = 24, step = 1)),
        column(6, numericInput("font_axis_title", "Axis titles", 13,
                               min = 4, max = 24, step = 1))
      )),
      div(class = "font-row", fluidRow(
        column(6, numericInput("font_legend_text", "Legend text", 12,
                               min = 4, max = 24, step = 1)),
        column(6, numericInput("font_legend_title", "Legend title", 13,
                               min = 4, max = 24, step = 1))
      )),
      div(class = "font-row", fluidRow(
        column(6, numericInput("font_strip", "Facet labels", 12,
                               min = 4, max = 24, step = 1)),
        column(6, numericInput("font_geom", "Bar labels", 4.5,
                               min = 1, max = 12, step = 0.5))
      )),

      hr(),
      fluidRow(
        column(6, actionButton("save_btn", "\U0001f4be Save",
                               class = "btn-primary", width = "100%")),
        column(6, actionButton("reset_fonts_btn", "\u21ba Reset fonts",
                               width = "100%"))
      ),
      hr(),
      verbatimTextOutput("status"),
      hr(),
      h5("YAML preview"),
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

# ── 7. Server ───────────────────────────────────────────────

server <- function(input, output, session) {

  # --- Inventory ---
  figures      <- parse_figure_list()
  chunk_labels <- figures$label
  chunk_files  <- figures$qmd_file
  chapters     <- unique(chunk_files)
  report_sizes <- load_report_sizes()

  message("Found ", nrow(figures), " presentation figures in ",
          length(chapters), " chapters")

  updateSelectInput(session, "chapter", choices = chapters,
                    selected = chapters[1])

  # --- Navigation target (for cross-chapter jumps) ---
  nav_target <- reactiveValues(figure = NULL)

  observeEvent(input$chapter, {
    idx    <- which(chunk_files == input$chapter)
    labels <- chunk_labels[idx]
    target <- if (!is.null(isolate(nav_target$figure)) &&
                  isolate(nav_target$figure) %in% labels) {
      isolate(nav_target$figure)
    } else {
      labels[1]
    }
    updateSelectInput(session, "figure", choices = labels, selected = target)
    nav_target$figure <- NULL
  })

  # --- Presentation sizes YAML ---
  pres <- reactiveVal({
    if (file.exists(PRES_YML)) yaml::read_yaml(PRES_YML)
    else list(`_defaults` = list(fonts = DEFAULT_FONTS))
  })

  # --- Effective fonts for a figure (global merged with per-figure) ---
  get_fonts_for <- function(label) {
    s      <- pres()
    global <- s[["_defaults"]]$fonts %||% DEFAULT_FONTS
    fig_f  <- s[[label]]$fonts
    if (is.null(fig_f)) return(global)
    merged <- global
    for (k in names(fig_f)) merged[[k]] <- fig_f[[k]]
    merged
  }

  # --- Update controls on figure / profile change ---
  observeEvent(list(input$figure, input$profile), {
    req(input$figure, input$profile)
    s <- pres()

    # Dimensions
    dim_entry <- s[[input$figure]][[input$profile]]
    if (!is.null(dim_entry$width) && !is.null(dim_entry$height)) {
      w <- dim_entry$width;  h <- dim_entry$height
    } else {
      d <- get_default_dims(input$figure, input$profile, report_sizes)
      w <- d$width;  h <- d$height
    }
    updateSliderInput(session, "width",  value = w)
    updateSliderInput(session, "height", value = h)

    # Fonts
    f <- get_fonts_for(input$figure)
    updateNumericInput(session, "font_base",         value = f$base_size)
    updateNumericInput(session, "font_title",        value = f$title)
    updateNumericInput(session, "font_axis_text",    value = f$axis_text)
    updateNumericInput(session, "font_axis_title",   value = f$axis_title)
    updateNumericInput(session, "font_legend_text",  value = f$legend_text)
    updateNumericInput(session, "font_legend_title", value = f$legend_title)
    updateNumericInput(session, "font_strip",        value = f$strip_text)
    updateNumericInput(session, "font_geom",         value = f$geom_text)
  })

  # --- Chapter data loading (cached by chapter::profile) ---
  loaded_chapter <- reactiveVal("")
  loaded_figure  <- reactiveVal("")

  setup_chapter <- function() {
    req(input$chapter, input$profile)
    key <- paste(input$chapter, input$profile, sep = "::")
    if (key == loaded_chapter()) return(invisible())

    Sys.setenv(QUARTO_PROFILE = input$profile)
    saved_wd <- setwd(PROJECT_ROOT)
    on.exit(setwd(saved_wd), add = TRUE)

    tryCatch({
      suppressPackageStartupMessages({
        library(haven);    library(dplyr);   library(tidyr)
        library(stringr);  library(ggplot2); library(gt)
        library(forcats);  library(purrr);   library(scales)
        library(lubridate); library(labelled)
      })

      source(file.path(PROJECT_ROOT, "utils/report_variant.R"), local = FALSE)
      source(file.path(PROJECT_ROOT, "utils/sites.R"),          local = FALSE)
      source(file.path(PROJECT_ROOT, "utils/plot_theme.R"),     local = FALSE)
      source(file.path(PROJECT_ROOT, "utils/figure_sizes.R"),   local = FALSE)
      tryCatch(source(file.path(PROJECT_ROOT, "utils/helpers_report.R"),
                      local = FALSE), error = function(e) NULL)
      tryCatch(source(file.path(PROJECT_ROOT, "utils/ror_plots.R"),
                      local = FALSE), error = function(e) NULL)
      dl <- file.path(PROJECT_ROOT, "utils/downloadable_output.R")
      if (file.exists(dl))
        tryCatch(source(dl, local = FALSE), error = function(e) NULL)

      # Run setup code up to the first presentation figure in this chapter
      first_lbl <- chunk_labels[which(chunk_files == input$chapter)[1]]
      qmd_path  <- file.path(PROJECT_ROOT, input$chapter)
      setup     <- extract_setup_code(qmd_path, first_lbl)
      if (length(setup) > 0) {
        setup <- setup[!grepl("^\\s*knitr::", setup)]
        setup <- setup[!grepl("knit_print|registerS3method", setup)]
        eval(parse(text = paste(setup, collapse = "\n")),
             envir = globalenv())
      }
    }, error = function(e) message("Setup error: ", e$message))

    loaded_chapter(key)
    loaded_figure("")
  }

  ensure_figure_setup <- function() {
    req(input$figure, input$chapter, input$profile)
    setup_chapter()

    fig_key <- paste(input$figure, input$profile, input$chapter, sep = "::")
    if (fig_key == loaded_figure()) return(invisible())

    # Incremental code between the first figure and the current one
    first_lbl <- chunk_labels[which(chunk_files == input$chapter)[1]]
    if (input$figure != first_lbl) {
      tryCatch({
        Sys.setenv(QUARTO_PROFILE = input$profile)
        saved_wd <- setwd(PROJECT_ROOT)
        on.exit(setwd(saved_wd), add = TRUE)

        qmd_path   <- file.path(PROJECT_ROOT, input$chapter)
        all_setup  <- extract_setup_code(qmd_path, input$figure)
        base_setup <- extract_setup_code(qmd_path, first_lbl)

        if (length(all_setup) > length(base_setup)) {
          new_code <- all_setup[(length(base_setup) + 1):length(all_setup)]
          new_code <- new_code[!grepl("^\\s*knitr::", new_code)]
          new_code <- new_code[!grepl("knit_print|registerS3method", new_code)]
          if (length(new_code) > 0)
            eval(parse(text = paste(new_code, collapse = "\n")),
                 envir = globalenv())
        }
      }, error = function(e) message("Incremental setup: ", e$message))
    }
    loaded_figure(fig_key)
  }

  # --- Current font settings from UI inputs ---
  current_fonts <- reactive({
    list(
      base_size    = input$font_base         %||% 14,
      title        = input$font_title        %||% 16,
      axis_text    = input$font_axis_text    %||% 12,
      axis_title   = input$font_axis_title   %||% 13,
      legend_text  = input$font_legend_text  %||% 12,
      legend_title = input$font_legend_title %||% 13,
      strip_text   = input$font_strip        %||% 12,
      geom_text    = input$font_geom         %||% 4.5
    )
  })

  # --- Figure preview ---
  output$fig_preview <- renderPlot({
    req(input$figure, input$profile)
    ensure_figure_setup()

    qmd_path <- file.path(PROJECT_ROOT, input$chapter)
    chunks   <- parse_figure_chunks(qmd_path)
    if (!input$figure %in% names(chunks)) return(NULL)

    code <- paste(chunks[[input$figure]], collapse = "\n")
    tryCatch({
      Sys.setenv(QUARTO_PROFILE = input$profile)
      saved_wd <- setwd(PROJECT_ROOT)
      on.exit(setwd(saved_wd), add = TRUE)
      p <- eval(parse(text = code), envir = globalenv())
      if (inherits(p, "gg")) {
        apply_presentation_fonts(p, current_fonts())
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:\n", e$message),
           cex = 0.9, col = "red")
    })
  },
  height = function() input$height * 96,
  width  = function() input$width  * 96,
  res    = 96)

  # --- Render info ---
  output$render_log <- renderPrint({
    req(input$figure)
    f <- current_fonts()
    cat(sprintf("Label: %s\nProfile: %s\nDimensions: %.1f \u00d7 %.1f in\n\n",
                input$figure, input$profile, input$width, input$height))
    cat(sprintf(paste0(
      "Fonts:  base=%g  title=%g\n",
      "        axis_text=%g  axis_title=%g\n",
      "        legend_text=%g  legend_title=%g\n",
      "        strip=%g  geom_text=%g\n"),
      f$base_size, f$title, f$axis_text, f$axis_title,
      f$legend_text, f$legend_title, f$strip_text, f$geom_text))
  })

  # --- Code tab ---
  output$chunk_code <- renderPrint({
    req(input$figure)
    qmd_path <- file.path(PROJECT_ROOT,
                          chunk_files[which(chunk_labels == input$figure)[1]])
    chunks <- parse_figure_chunks(qmd_path)
    if (input$figure %in% names(chunks))
      cat(paste(chunks[[input$figure]], collapse = "\n"))
    else
      cat("Chunk not found")
  })

  # --- Save ---
  observeEvent(input$save_btn, {
    s <- pres()

    # Dimensions
    if (is.null(s[[input$figure]])) s[[input$figure]] <- list()
    s[[input$figure]][[input$profile]] <- list(
      width  = input$width,
      height = input$height
    )

    # Fonts
    fonts <- current_fonts()
    if (isTRUE(input$fonts_global)) {
      s[["_defaults"]]$fonts <- fonts
    } else {
      s[[input$figure]]$fonts <- fonts
    }

    pres(s)
    yaml::write_yaml(s, PRES_YML)
    showNotification(
      sprintf("Saved %s / %s + fonts (%s)",
              input$figure, input$profile,
              if (isTRUE(input$fonts_global)) "global" else "per-figure"),
      type = "message")
  })

  # --- Reset fonts to defaults ---
  observeEvent(input$reset_fonts_btn, {
    updateNumericInput(session, "font_base",         value = DEFAULT_FONTS$base_size)
    updateNumericInput(session, "font_title",        value = DEFAULT_FONTS$title)
    updateNumericInput(session, "font_axis_text",    value = DEFAULT_FONTS$axis_text)
    updateNumericInput(session, "font_axis_title",   value = DEFAULT_FONTS$axis_title)
    updateNumericInput(session, "font_legend_text",  value = DEFAULT_FONTS$legend_text)
    updateNumericInput(session, "font_legend_title", value = DEFAULT_FONTS$legend_title)
    updateNumericInput(session, "font_strip",        value = DEFAULT_FONTS$strip_text)
    updateNumericInput(session, "font_geom",         value = DEFAULT_FONTS$geom_text)
  })

  # --- Navigation ---
  observeEvent(input$prev_btn, {
    req(input$figure)
    pos <- which(chunk_labels == input$figure)
    if (length(pos) > 0 && pos[1] > 1) {
      new_pos <- pos[1] - 1
      if (chunk_files[new_pos] != input$chapter) {
        nav_target$figure <- chunk_labels[new_pos]
        updateSelectInput(session, "chapter",
                          selected = chunk_files[new_pos])
      } else {
        updateSelectInput(session, "figure",
                          selected = chunk_labels[new_pos])
      }
    }
  })

  observeEvent(input$next_btn, {
    req(input$figure)
    pos <- which(chunk_labels == input$figure)
    if (length(pos) > 0 && pos[1] < length(chunk_labels)) {
      new_pos <- pos[1] + 1
      if (chunk_files[new_pos] != input$chapter) {
        nav_target$figure <- chunk_labels[new_pos]
        updateSelectInput(session, "chapter",
                          selected = chunk_files[new_pos])
      } else {
        updateSelectInput(session, "figure",
                          selected = chunk_labels[new_pos])
      }
    }
  })

  # --- Status ---
  output$status <- renderText({
    req(input$figure)
    pos <- which(chunk_labels == input$figure)
    sprintf("Figure %d / %d", pos[1], length(chunk_labels))
  })

  # --- YAML preview ---
  output$yaml_preview <- renderPrint({
    req(input$figure)
    s <- pres()
    cat("--- Global font defaults ---\n")
    cat(yaml::as.yaml(s[["_defaults"]]))
    entry <- s[[input$figure]]
    if (!is.null(entry)) {
      cat("\n--- ", input$figure, " ---\n")
      cat(yaml::as.yaml(setNames(list(entry), input$figure)))
    } else {
      cat("\n(no saved entry for ", input$figure, ")\n")
    }
  })

  # --- All figures table ---
  output$all_figs_table <- DT::renderDataTable({
    s <- pres()
    df <- data.frame(
      Chapter = chunk_files,
      Label   = chunk_labels,
      stringsAsFactors = FALSE
    )
    for (p in PROFILES) {
      df[[paste0(p, "_w")]] <- vapply(chunk_labels, function(lbl) {
        s[[lbl]][[p]]$width %||% NA_real_
      }, numeric(1))
      df[[paste0(p, "_h")]] <- vapply(chunk_labels, function(lbl) {
        s[[lbl]][[p]]$height %||% NA_real_
      }, numeric(1))
    }
    df$custom_fonts <- vapply(chunk_labels, function(lbl) {
      !is.null(s[[lbl]]$fonts)
    }, logical(1))
    DT::datatable(df, options = list(pageLength = 50),
                  selection = "single", rownames = FALSE)
  })

  observeEvent(input$all_figs_table_rows_selected, {
    row <- input$all_figs_table_rows_selected
    if (length(row) == 1) {
      if (chunk_files[row] != input$chapter) {
        nav_target$figure <- chunk_labels[row]
        updateSelectInput(session, "chapter",
                          selected = chunk_files[row])
      } else {
        updateSelectInput(session, "figure",
                          selected = chunk_labels[row])
      }
    }
  })
}

# ── Launch ────────────────────────────────────────────────────
if (interactive() || identical(Sys.getenv("SHINY_APP"), "1")) {
  shinyApp(ui, server)
}
