# utils/sites.R
# Site definitions for the ROR observatory report
# ----------------------------------------------------------

#' Named list of sites by observatory
SITES <- list(
  Marovoay = c("Ampijoroa", "Bepako", "Madiromiongana", "Maroala"),
  Alaotra = c(
    "Ambatoharanana",
    "Ambatomanga",
    "Ambodivoara",
    "Ambohidrony",
    "Analamiranga",
    "Avaradrano",
    "Feramanga Atsimo",
    "Mangabe"
  )
)

#' Add a Site column to a data frame
#'
#' For Marovoay, groups sub-hamlets using fix_hameau().
#' For Alaotra, each hamlet is already its own site.
#' Requires Observatory and j4 columns in the data.
#' @param data Data frame with Observatory and j4 columns
#' @return Data frame with added Site column
assign_site <- function(data) {
  data |> dplyr::mutate(Site = fix_hameau(j4))
}

#' Get all Observatory x Site combinations present in the data
#'
#' Returns a tibble with one row per variant to generate.
#' Each observatory gets a "Tous" entry plus one entry per site.
#' Marovoay is listed before Alaotra.
#' @param data Data frame with Observatory and Site columns
#' @return Tibble with Observatory and Site columns
get_site_combos <- function(data) {
  obs_list <- intersect(c("Marovoay", "Alaotra"), unique(data$Observatory))
  purrr::map_dfr(obs_list, function(obs) {
    sites_present <- sort(unique(data$Site[data$Observatory == obs]))
    tibble::tibble(
      Observatory = obs,
      Site = c("Tous", sites_present)
    )
  })
}
