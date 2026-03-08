# utils/downloadable_output.R
# Wrappers for generating pre-computed site-level downloadable variants
# ----------------------------------------------------------

#' Sanitise a string for use in filenames
#' @param x Character string
#' @return Lowercase string with non-alphanumeric chars replaced by underscores
sanitize_filename <- function(x) {
  x |>
    tolower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_remove("^_|_$")
}

#' Build download filename
#' @param id   Output identifier (e.g. "fig_age_obs")
#' @param obs  Observatory name
#' @param site Site name (or "Tous")
#' @param ext  File extension without dot
#' @return Sanitised filename string
make_dl_filename <- function(id, obs, site, ext) {
  paste0(
    id,
    "_",
    sanitize_filename(obs),
    "_",
    sanitize_filename(site),
    ".",
    ext
  )
}

# ----------------------------------------------------------
# Data preparation
# ----------------------------------------------------------

#' Prepare combined data for a site variant
#'
#' Overwrites the Observatory column with site labels so that
#' existing plot_fn / tbl_fn that group/facet by Observatory produce
#' the correct output.
#'
#' For "Tous":  obs aggregate ("{Obs} - Tous") + one slice per site.
#' For a site:  obs aggregate ("{Obs} - Tous") + that site.
#'
#' @param data Full data with Observatory and Site columns
#' @param obs  Observatory name (e.g. "Marovoay")
#' @param site Site name or "Tous"
#' @return Combined data frame with Observatory overwritten by site labels
prepare_variant_data <- function(data, obs, site) {
  obs_data <- dplyr::filter(data, .data$Observatory == .env$obs)
  obs_agg <- obs_data |> dplyr::mutate(Observatory = paste(.env$obs, "- Tous"))

  if (site == "Tous") {
    site_parts <- purrr::map_dfr(
      sort(unique(obs_data$Site)),
      function(s) {
        obs_data |>
          dplyr::filter(.data$Site == s) |>
          dplyr::mutate(Observatory = paste(.env$obs, "-", s))
      }
    )
    dplyr::bind_rows(obs_agg, site_parts)
  } else {
    site_data <- obs_data |>
      dplyr::filter(.data$Site == .env$site) |>
      dplyr::mutate(Observatory = paste(.env$obs, "-", .env$site))
    dplyr::bind_rows(obs_agg, site_data)
  }
}

# ----------------------------------------------------------
# Variant generators
# ----------------------------------------------------------

#' Generate site-level PNG variants for a figure and emit download dropdown
#'
#' Call in a chunk with `#| echo: false` and `#| results: asis`.
#' Site-level variants with very small sample sizes may produce
#' unreliable graphs; a footnote is added automatically.
#'
#' @param id       Unique output identifier (e.g. "fig_age_obs")
#' @param chapter  Chapter identifier for sub-folder (e.g. "04")
#' @param plot_fn  Function(data) -> ggplot object
#' @param data     Data frame with Observatory and Site columns
#' @param width    PNG width in inches (default 10)
#' @param height   PNG height in inches (default 6)
#' @param dpi      PNG resolution (default 150)
#' @param min_cell Minimum site sample size to include (default 10)
dl_fig_variants <- function(
  id,
  chapter,
  plot_fn,
  data,
  width = 10,
  height = 6,
  dpi = 150,
  min_cell = 10
) {
  disk_dir <- file.path("docs", "downloads", chapter)
  dir.create(disk_dir, recursive = TRUE, showWarnings = FALSE)

  combos <- get_site_combos(data)
  links <- list()

  for (i in seq_len(nrow(combos))) {
    obs <- combos$Observatory[i]
    site <- combos$Site[i]

    variant <- prepare_variant_data(data, obs, site)
    if (nrow(variant) == 0) {
      next
    }

    # Skip individual-site figures when the site is too small
    if (site != "Tous") {
      site_n <- sum(data$Observatory == obs & data$Site == site)
      if (site_n < min_cell) next
    }

    fname <- make_dl_filename(id, obs, site, "png")
    disk_path <- file.path(disk_dir, fname)
    web_path <- file.path("downloads", chapter, fname)

    tryCatch(
      {
        p <- plot_fn(variant)
        ggplot2::ggsave(
          disk_path,
          p,
          width = width,
          height = height,
          dpi = dpi,
          bg = "white"
        )
        links[[length(links) + 1]] <- list(
          label = site,
          obs = obs,
          href = web_path,
          ext = "png"
        )
      },
      error = function(e) NULL
    )
  }

  emit_dropdown_html(links)
  invisible(NULL)
}

#' Generate site-level XLSX variants for a table and emit download dropdown
#'
#' Call in a chunk with `#| echo: false` and `#| results: asis`.
#' Cells with counts below `min_cell` (default 5) are automatically
#' suppressed for statistical confidentiality.
#'
#' @param id       Unique output identifier (e.g. "tbl_men_obs")
#' @param chapter  Chapter identifier for sub-folder (e.g. "04")
#' @param tbl_fn   Function(data) -> data.frame suitable for XLSX export
#' @param data     Data frame with Observatory and Site columns
#' @param min_cell Minimum cell count for confidentiality (default 5)
dl_tbl_variants <- function(id, chapter, tbl_fn, data, min_cell = 5) {
  disk_dir <- file.path("docs", "downloads", chapter)
  dir.create(disk_dir, recursive = TRUE, showWarnings = FALSE)

  combos <- get_site_combos(data)
  links <- list()

  for (i in seq_len(nrow(combos))) {
    obs <- combos$Observatory[i]
    site <- combos$Site[i]

    variant <- prepare_variant_data(data, obs, site)
    if (nrow(variant) == 0) {
      next
    }

    fname <- make_dl_filename(id, obs, site, "xlsx")
    disk_path <- file.path(disk_dir, fname)
    web_path <- file.path("downloads", chapter, fname)

    tryCatch(
      {
        tbl_df <- tbl_fn(variant)
        tbl_df <- suppress_for_export(tbl_df, threshold = min_cell)
        writexl::write_xlsx(tbl_df, disk_path)
        links[[length(links) + 1]] <- list(
          label = site,
          obs = obs,
          href = web_path,
          ext = "xlsx"
        )
      },
      error = function(e) NULL
    )
  }

  emit_dropdown_html(links)
  invisible(NULL)
}

# ----------------------------------------------------------
# HTML dropdown emitter
# ----------------------------------------------------------

#' Emit an HTML \code{<details>} dropdown with grouped download links
#'
#' Groups links by observatory (Marovoay first, then Alaotra).
#' @param links List of lists, each with elements: label, obs, href, ext
emit_dropdown_html <- function(links) {
  if (length(links) == 0) {
    return(invisible(NULL))
  }

  obs_groups <- split(links, vapply(links, \(x) x$obs, character(1)))

  html <- '<details class="dl-variants">\n'
  html <- paste0(
    html,
    '<summary>\U{1F4E5} T\u00e9l\u00e9charger par site</summary>\n'
  )
  html <- paste0(html, '<div class="dl-grid">\n')

  for (obs in intersect(c("Marovoay", "Alaotra"), names(obs_groups))) {
    items <- obs_groups[[obs]]
    html <- paste0(html, '<div class="dl-group">\n')
    html <- paste0(html, '<h4>', obs, '</h4>\n<ul>\n')
    for (item in items) {
      ext_lbl <- toupper(item$ext)
      html <- paste0(
        html,
        '<li><a href="',
        item$href,
        '" download>',
        item$label,
        ' (',
        ext_lbl,
        ')</a></li>\n'
      )
    }
    html <- paste0(html, '</ul>\n</div>\n')
  }

  html <- paste0(html, '</div>\n</details>\n')
  cat(html)
}
