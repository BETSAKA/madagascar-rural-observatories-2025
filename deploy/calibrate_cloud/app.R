# deploy/calibrate_cloud/app.R
# ─────────────────────────────────────────────────────────────────
# Cloud-deployable Shiny app for calibrating presentation figure
# dimensions, fonts, and facet layout.
#
# Uses pre-aggregated XLSX data (no microdata) so it can be safely
# deployed to shinyapps.io.
#
# Deploy:
#   rsconnect::deployApp("deploy/calibrate_cloud")
# ─────────────────────────────────────────────────────────────────

library(shiny)
library(yaml)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(rlang)
library(scales)

source("R/ror_plots.R")

# ── 1. Load data ────────────────────────────────────────────

PROFILES <- c("marovoay", "alaotra")

# Pre-aggregated data: one XLSX per profile, one sheet per figure
xlsx_data <- list()
for (prof in PROFILES) {
  path <- file.path("data", paste0("presentation_data_", prof, ".xlsx"))
  if (file.exists(path)) {
    sheets <- readxl::excel_sheets(path)
    for (s in sheets) {
      xlsx_data[[prof]][[s]] <- readxl::read_xlsx(path, sheet = s)
    }
  }
}

# Figure metadata (plot type, column mappings, params)
# NOTE: YAML 1.1 parses bare `y` as boolean TRUE.  Keys are quoted in the
# YAML file ("y": …), but add a defensive fixup just in case.
fig_meta <- yaml::read_yaml("data/figure_metadata.yml")
for (.nm in names(fig_meta)) {
  if ("TRUE" %in% names(fig_meta[[.nm]]) && !"y" %in% names(fig_meta[[.nm]])) {
    fig_meta[[.nm]][["y"]] <- fig_meta[[.nm]][["TRUE"]]
    fig_meta[[.nm]][["TRUE"]] <- NULL
  }
}

# Presentation sizes (dimensions + fonts)
PRES_YML_PATH <- "data/presentation_sizes.yml"
load_pres_sizes <- function() {
  if (file.exists(PRES_YML_PATH)) {
    yaml::read_yaml(PRES_YML_PATH)
  } else {
    list(`_defaults` = list(fonts = DEFAULT_FONTS))
  }
}

# Build figure list from metadata
all_labels <- names(fig_meta)

# ── 2. Plot builder ─────────────────────────────────────────

#' Rebuild a ggplot from aggregate data + metadata
#'
#' @param data   Data frame (one XLSX sheet)
#' @param meta   List from figure_metadata.yml for this figure
#' @param ncol_override  User-chosen ncol (NULL = auto)
#' @return A ggplot object or NULL
build_plot <- function(data, meta, ncol_override = NULL) {
  if (is.null(data) || is.null(meta) || meta$plot_type == "custom") {
    return(NULL)
  }

  x_sym <- rlang::sym(meta$x)
  y_sym <- rlang::sym(meta$y)
  params <- meta$params %||% list()
  y_label <- params$y_label
  ncol_val <- ncol_override

  if (meta$plot_type == "make_bar_obs") {
    make_bar_obs(
      data,
      x = !!x_sym,
      y = !!y_sym,
      y_label = y_label,
      ncol = ncol_val
    )
  } else if (meta$plot_type == "ror_bar_v") {
    ror_bar_v(
      data,
      x = !!x_sym,
      y = !!y_sym,
      y_label = y_label,
      x_angle = params$x_angle %||% 0,
      ncol = ncol_val
    )
  } else if (meta$plot_type == "ror_bar_grouped") {
    fill_sym <- rlang::sym(meta$fill)
    palette <- if (!is.null(params$palette)) {
      unlist(params$palette)
    } else if (!is.null(params$palette_brewer)) {
      # handled below
      NULL
    } else {
      NULL
    }
    facet_flag <- if (is.logical(params$facet)) params$facet else TRUE

    p <- ror_bar_grouped(
      data,
      x = !!x_sym,
      y = !!y_sym,
      fill = !!fill_sym,
      direction = params$direction %||% "vertical",
      x_angle = params$x_angle %||% 45,
      palette = palette,
      facet = facet_flag,
      ncol = ncol_val
    )

    # Handle brewer palette
    if (!is.null(params$palette_brewer)) {
      p <- p + ggplot2::scale_fill_brewer(palette = params$palette_brewer)
    }

    # Handle extra facet (e.g. fig_menaces faceted by Section)
    if (!is.null(params$extra_facet) && params$extra_facet %in% names(data)) {
      p <- p +
        ggplot2::facet_wrap(
          rlang::sym(params$extra_facet),
          scales = "free_y",
          ncol = ncol_val %||% 2
        )
    }
    p
  } else if (meta$plot_type == "ror_bar_stacked") {
    fill_sym <- rlang::sym(meta$fill)
    palette <- if (!is.null(params$palette)) unlist(params$palette) else NULL

    ror_bar_stacked(
      data,
      x = !!x_sym,
      y = !!y_sym,
      fill = !!fill_sym,
      y_label = y_label,
      proportion = if (is.logical(params$proportion)) {
        params$proportion
      } else {
        TRUE
      },
      direction = params$direction %||% "horizontal",
      x_angle = params$x_angle %||% 0,
      palette = palette,
      ncol = ncol_val
    )
  } else {
    NULL
  }
}

# ── 3. UI ───────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("Presentation Figure Calibrator (Cloud)"),
  tags$head(tags$style(HTML(
    "
    .font-row { margin-bottom: 4px; }
    .font-row .form-group { margin-bottom: 2px; }
    .btn-nav { font-size: 13px; }
  "
  ))),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "figure",
        "Figure",
        choices = all_labels,
        selected = all_labels[1]
      ),
      selectInput(
        "profile",
        "Profile",
        choices = PROFILES,
        selected = "marovoay"
      ),
      fluidRow(
        column(
          6,
          actionButton(
            "prev_btn",
            "\u25c0 Prev",
            class = "btn-nav",
            width = "100%"
          )
        ),
        column(
          6,
          actionButton(
            "next_btn",
            "Next \u25b6",
            class = "btn-nav",
            width = "100%"
          )
        )
      ),

      hr(),
      h4("Dimensions (inches)"),
      sliderInput("width", "Width", min = 3, max = 16, value = 6.5, step = 0.5),
      sliderInput("height", "Height", min = 2, max = 20, value = 5, step = 0.5),

      hr(),
      h4("Layout"),
      numericInput(
        "ncol",
        "Facet columns (0 = auto)",
        value = 0,
        min = 0,
        max = 6,
        step = 1
      ),

      hr(),
      h4("Font sizes"),
      checkboxInput(
        "fonts_global",
        "Apply fonts to ALL figures",
        value = FALSE
      ),
      div(
        class = "font-row",
        fluidRow(
          column(
            6,
            numericInput(
              "font_base",
              "Base size",
              14,
              min = 6,
              max = 30,
              step = 1
            )
          ),
          column(
            6,
            numericInput("font_title", "Title", 16, min = 6, max = 36, step = 1)
          )
        )
      ),
      div(
        class = "font-row",
        fluidRow(
          column(
            6,
            numericInput(
              "font_axis_text",
              "Axis tick labels",
              12,
              min = 4,
              max = 24,
              step = 1
            )
          ),
          column(
            6,
            numericInput(
              "font_axis_title",
              "Axis titles",
              13,
              min = 4,
              max = 24,
              step = 1
            )
          )
        )
      ),
      div(
        class = "font-row",
        fluidRow(
          column(
            6,
            numericInput(
              "font_legend_text",
              "Legend text",
              12,
              min = 4,
              max = 24,
              step = 1
            )
          ),
          column(
            6,
            numericInput(
              "font_legend_title",
              "Legend title",
              13,
              min = 4,
              max = 24,
              step = 1
            )
          )
        )
      ),
      div(
        class = "font-row",
        fluidRow(
          column(
            6,
            numericInput(
              "font_strip",
              "Facet labels",
              12,
              min = 4,
              max = 24,
              step = 1
            )
          ),
          column(
            6,
            numericInput(
              "font_geom",
              "Bar labels",
              4.5,
              min = 1,
              max = 12,
              step = 0.5
            )
          )
        )
      ),

      hr(),
      fluidRow(
        column(
          4,
          actionButton(
            "save_btn",
            "\U0001f4be Save",
            class = "btn-primary",
            width = "100%"
          )
        ),
        column(
          4,
          actionButton("reset_fonts_btn", "\u21ba Reset", width = "100%")
        ),
        column(4, downloadButton("dl_png", "\U0001f4f7 PNG"))
      ),
      hr(),
      verbatimTextOutput("status"),
      hr(),
      h5("YAML preview"),
      verbatimTextOutput("yaml_preview")
    ),

    mainPanel(
      width = 9,
      plotOutput("fig_preview", height = "auto"),
      hr(),
      verbatimTextOutput("render_log")
    )
  )
)

# ── 4. Server ───────────────────────────────────────────────

server <- function(input, output, session) {
  # --- Presentation sizes (reactive, saveable) ---
  pres <- reactiveVal(load_pres_sizes())

  # --- Effective fonts ---
  get_fonts_for <- function(label) {
    s <- pres()
    global <- s[["_defaults"]]$fonts %||% DEFAULT_FONTS
    fig_f <- s[[label]]$fonts
    if (is.null(fig_f)) {
      return(global)
    }
    merged <- global
    for (k in names(fig_f)) {
      merged[[k]] <- fig_f[[k]]
    }
    merged
  }

  # --- Update controls on figure / profile change ---
  observeEvent(list(input$figure, input$profile), {
    req(input$figure, input$profile)
    s <- pres()

    # Dimensions: prefer saved, fall back to 6.5 × 5
    dim_entry <- s[[input$figure]][[input$profile]]
    w <- dim_entry$width %||% 6.5
    h <- dim_entry$height %||% 5
    if (input$profile == "alaotra" && is.null(dim_entry$height)) {
      h <- h * 1.25
    }

    updateSliderInput(session, "width", value = w)
    updateSliderInput(session, "height", value = h)

    # Fonts
    f <- get_fonts_for(input$figure)
    updateNumericInput(session, "font_base", value = f$base_size)
    updateNumericInput(session, "font_title", value = f$title)
    updateNumericInput(session, "font_axis_text", value = f$axis_text)
    updateNumericInput(session, "font_axis_title", value = f$axis_title)
    updateNumericInput(session, "font_legend_text", value = f$legend_text)
    updateNumericInput(session, "font_legend_title", value = f$legend_title)
    updateNumericInput(session, "font_strip", value = f$strip_text)
    updateNumericInput(session, "font_geom", value = f$geom_text)

    # Reset ncol to 0 (auto)
    updateNumericInput(session, "ncol", value = 0)
  })

  # --- Current fonts from UI ---
  current_fonts <- reactive({
    list(
      base_size = input$font_base %||% 14,
      title = input$font_title %||% 16,
      axis_text = input$font_axis_text %||% 12,
      axis_title = input$font_axis_title %||% 13,
      legend_text = input$font_legend_text %||% 12,
      legend_title = input$font_legend_title %||% 13,
      strip_text = input$font_strip %||% 12,
      geom_text = input$font_geom %||% 4.5
    )
  })

  # --- Effective ncol ---
  effective_ncol <- reactive({
    n <- input$ncol %||% 0
    if (n == 0) NULL else as.integer(n)
  })

  # --- Build plot (reactive) ---
  current_plot <- reactive({
    req(input$figure, input$profile)
    meta <- fig_meta[[input$figure]]
    if (is.null(meta)) {
      return(NULL)
    }

    # Trim trailing space from sheet name (fig_croisement has one)
    sheet_name <- trimws(input$figure)
    data <- xlsx_data[[input$profile]][[sheet_name]]
    if (is.null(data)) {
      # Try with original name
      data <- xlsx_data[[input$profile]][[input$figure]]
    }
    if (is.null(data)) {
      return(NULL)
    }

    p <- build_plot(data, meta, ncol_override = effective_ncol())
    if (!is.null(p)) {
      p <- apply_presentation_fonts(p, current_fonts())
    }
    p
  })

  # --- Figure preview ---
  output$fig_preview <- renderPlot(
    {
      p <- current_plot()
      if (is.null(p)) {
        plot.new()
        text(
          0.5,
          0.5,
          "No data or unsupported plot type",
          cex = 1.2,
          col = "grey50"
        )
      } else {
        p
      }
    },
    height = function() input$height * 96,
    width = function() input$width * 96,
    res = 96
  )

  # --- Render info ---
  output$render_log <- renderPrint({
    req(input$figure)
    f <- current_fonts()
    meta <- fig_meta[[input$figure]]
    cat(sprintf(
      "Label: %s\nProfile: %s\nDimensions: %.1f \u00d7 %.1f in\n",
      input$figure,
      input$profile,
      input$width,
      input$height
    ))
    cat(sprintf("Plot type: %s\n", meta$plot_type %||% "unknown"))
    cat(sprintf(
      "Facet ncol: %s\n\n",
      if (is.null(effective_ncol())) "auto" else effective_ncol()
    ))
    cat(sprintf(
      paste0(
        "Fonts:  base=%g  title=%g\n",
        "        axis_text=%g  axis_title=%g\n",
        "        legend_text=%g  legend_title=%g\n",
        "        strip=%g  geom_text=%g\n"
      ),
      f$base_size,
      f$title,
      f$axis_text,
      f$axis_title,
      f$legend_text,
      f$legend_title,
      f$strip_text,
      f$geom_text
    ))
  })

  # --- Save ---
  observeEvent(input$save_btn, {
    s <- pres()

    if (is.null(s[[input$figure]])) {
      s[[input$figure]] <- list()
    }
    s[[input$figure]][[input$profile]] <- list(
      width = input$width,
      height = input$height
    )

    fonts <- current_fonts()
    if (isTRUE(input$fonts_global)) {
      s[["_defaults"]]$fonts <- fonts
    } else {
      s[[input$figure]]$fonts <- fonts
    }

    # Save ncol if non-auto
    if (!is.null(effective_ncol())) {
      s[[input$figure]]$ncol <- effective_ncol()
    } else {
      s[[input$figure]]$ncol <- NULL
    }

    pres(s)
    yaml::write_yaml(s, PRES_YML_PATH)
    showNotification(
      sprintf(
        "Saved %s / %s + fonts (%s)",
        input$figure,
        input$profile,
        if (isTRUE(input$fonts_global)) "global" else "per-figure"
      ),
      type = "message"
    )
  })

  # --- Reset fonts ---
  observeEvent(input$reset_fonts_btn, {
    updateNumericInput(session, "font_base", value = DEFAULT_FONTS$base_size)
    updateNumericInput(session, "font_title", value = DEFAULT_FONTS$title)
    updateNumericInput(
      session,
      "font_axis_text",
      value = DEFAULT_FONTS$axis_text
    )
    updateNumericInput(
      session,
      "font_axis_title",
      value = DEFAULT_FONTS$axis_title
    )
    updateNumericInput(
      session,
      "font_legend_text",
      value = DEFAULT_FONTS$legend_text
    )
    updateNumericInput(
      session,
      "font_legend_title",
      value = DEFAULT_FONTS$legend_title
    )
    updateNumericInput(session, "font_strip", value = DEFAULT_FONTS$strip_text)
    updateNumericInput(session, "font_geom", value = DEFAULT_FONTS$geom_text)
  })

  # --- PNG download ---
  output$dl_png <- downloadHandler(
    filename = function() {
      paste0(input$figure, "_", input$profile, ".png")
    },
    content = function(file) {
      p <- current_plot()
      if (!is.null(p)) {
        ggplot2::ggsave(
          file,
          plot = p,
          width = input$width,
          height = input$height,
          dpi = 96,
          bg = "white"
        )
      }
    }
  )

  # --- Navigation ---
  observeEvent(input$prev_btn, {
    pos <- which(all_labels == input$figure)
    if (length(pos) > 0 && pos[1] > 1) {
      updateSelectInput(session, "figure", selected = all_labels[pos[1] - 1])
    }
  })

  observeEvent(input$next_btn, {
    pos <- which(all_labels == input$figure)
    if (length(pos) > 0 && pos[1] < length(all_labels)) {
      updateSelectInput(session, "figure", selected = all_labels[pos[1] + 1])
    }
  })

  # --- Status ---
  output$status <- renderText({
    pos <- which(all_labels == input$figure)
    sprintf("Figure %d / %d", pos[1], length(all_labels))
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
}

shinyApp(ui, server)
