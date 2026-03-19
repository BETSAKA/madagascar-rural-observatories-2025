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
#' Overwrites the Observatory column with **short** site labels so that
#' existing plot_fn / tbl_fn that group/facet by Observatory produce
#' the correct output.  Using short names directly avoids the need for
#' a separate Facet_label column that would be dropped by count() or
#' summarise().
#'
#' For "Tous":  obs aggregate ("Tous") + one slice per site (site name).
#' For a site:  obs aggregate ("Tous") + that site (site name).
#'
#' @param data Full data with Observatory and Site columns
#' @param obs  Observatory name (e.g. "Marovoay")
#' @param site Site name or "Tous"
#' @return Combined data frame with Observatory set to short labels (ordered factor)
prepare_variant_data <- function(data, obs, site) {
  obs_data <- dplyr::filter(data, .data$Observatory == .env$obs)
  obs_agg <- obs_data |> dplyr::mutate(Observatory = "Tous")

  if (site == "Tous") {
    site_parts <- purrr::map_dfr(
      sort(unique(obs_data$Site)),
      function(s) {
        obs_data |>
          dplyr::filter(.data$Site == s) |>
          dplyr::mutate(Observatory = s)
      }
    )
    result <- dplyr::bind_rows(obs_agg, site_parts)
  } else {
    site_data <- obs_data |>
      dplyr::filter(.data$Site == .env$site) |>
      dplyr::mutate(Observatory = .env$site)
    result <- dplyr::bind_rows(obs_agg, site_data)
  }
  # Order Observatory: sites alphabetically, "Tous" last
  site_levels <- sort(unique(result$Observatory[result$Observatory != "Tous"]))
  result$Observatory <- factor(
    result$Observatory,
    levels = c(site_levels, "Tous")
  )
  result
}

#' Expand data with site-level breakdowns for observatory-specific reports
#'
#' In consolidated mode the data is returned unchanged (facets show the
#' two observatories).
#' In observatory-specific mode (marovoay / alaotra), the data is expanded
#' so that the Observatory column contains short labels: "Tous" (aggregate)
#' plus one entry per site (e.g. "Bepako", "Ampijoroa").
#' This allows plots that `facet_wrap(~Observatory)` to display per-site
#' results alongside the observatory total.
#'
#' @param data Data frame with Observatory and Site columns
#' @return Expanded (or unchanged) data frame
expand_sites_for_profile <- function(data) {
  mode <- get_report_mode()
  if (mode$is_consolidated) {
    return(data)
  }
  prepare_variant_data(data, mode$observatory, "Tous")
}

# ----------------------------------------------------------
# Variant generators
# ----------------------------------------------------------

#' Generate a single aggregate PNG for a figure and emit a download link
#'
#' Call in a chunk with `#| echo: false` and `#| results: asis`.
#' Generates one PNG per observatory showing all sites ("Tous" variant).
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
  if (!knitr::is_html_output()) {
    return(invisible(NULL))
  }

  disk_dir <- file.path("docs", "downloads", chapter)
  dir.create(disk_dir, recursive = TRUE, showWarnings = FALSE)

  obs_list <- intersect(c("Marovoay", "Alaotra"), unique(data$Observatory))
  links <- list()

  for (obs in obs_list) {
    variant <- prepare_variant_data(data, obs, "Tous")
    if (nrow(variant) == 0) {
      next
    }

    fname <- make_dl_filename(id, obs, "tous", "png")
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
          label = obs,
          obs = obs,
          href = web_path,
          ext = "png"
        )
      },
      error = function(e) NULL
    )
  }

  emit_download_links(links)
  invisible(NULL)
}

#' Generate a single aggregate XLSX for a table and emit a download link
#'
#' Call in a chunk with `#| echo: false` and `#| results: asis`.
#' Generates one XLSX per observatory showing the "Tous" aggregate.
#'
#' @param id       Unique output identifier (e.g. "tbl_men_obs")
#' @param chapter  Chapter identifier for sub-folder (e.g. "04")
#' @param tbl_fn   Function(data) -> data.frame suitable for XLSX export
#' @param data     Data frame with Observatory and Site columns
#' @param min_cell Minimum cell count for confidentiality (default 5)
dl_tbl_variants <- function(id, chapter, tbl_fn, data, min_cell = 5) {
  if (!knitr::is_html_output()) {
    return(invisible(NULL))
  }

  disk_dir <- file.path("docs", "downloads", chapter)
  dir.create(disk_dir, recursive = TRUE, showWarnings = FALSE)

  obs_list <- intersect(c("Marovoay", "Alaotra"), unique(data$Observatory))
  links <- list()

  for (obs in obs_list) {
    variant <- prepare_variant_data(data, obs, "Tous")
    if (nrow(variant) == 0) {
      next
    }

    fname <- make_dl_filename(id, obs, "tous", "xlsx")
    disk_path <- file.path(disk_dir, fname)
    web_path <- file.path("downloads", chapter, fname)

    tryCatch(
      {
        tbl_df <- tbl_fn(variant)
        tbl_df <- suppress_for_export(tbl_df, threshold = min_cell)
        writexl::write_xlsx(tbl_df, disk_path)
        links[[length(links) + 1]] <- list(
          label = obs,
          obs = obs,
          href = web_path,
          ext = "xlsx"
        )
      },
      error = function(e) NULL
    )
  }

  emit_download_links(links)
  invisible(NULL)
}

# ----------------------------------------------------------
# HTML download link emitter
# ----------------------------------------------------------

#' Emit simple download links (one per observatory)
#'
#' @param links List of lists, each with elements: label, obs, href, ext
emit_download_links <- function(links) {
  if (length(links) == 0) {
    return(invisible(NULL))
  }

  parts <- vapply(
    links,
    function(item) {
      ext_lbl <- toupper(item$ext)
      sprintf(
        '<a href="%s" download class="dl-link">\U{1F4E5} %s (%s)</a>',
        item$href,
        item$label,
        ext_lbl
      )
    },
    character(1)
  )

  html <- paste0(
    '<div class="dl-downloads">\n',
    paste(parts, collapse = " &nbsp; "),
    '\n</div>\n'
  )
  cat(html)
}
