#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
library(tidyverse)
library(haven)
library(labelled)
library(gt)
library(scales)

source("utils/helpers_report.R")
source("utils/sites.R")
source("utils/downloadable_output.R")

res_deb <- read_dta("data/ROS_MDG_microdata/2025/res_deb.dta") |>
  mutate(j5 = as.character(j5)) |>
  add_obs() |>
  assign_site() |>
  filter_for_profile()

res_h <- read_dta("data/ROS_MDG_microdata/2025/res_h.dta") |> mutate(j5 = as.character(j5))
res_v <- read_dta("data/ROS_MDG_microdata/2025/res_v.dta") |> mutate(j5 = as.character(j5))
res_e <- read_dta("data/ROS_MDG_microdata/2025/res_E.dta") |> mutate(j5 = as.character(j5))

obs <- res_deb |> select(j5, Observatory, Site)
res_deb_site <- res_deb |> expand_sites_for_profile()
obs_site <- res_deb_site |> select(j5, Observatory, Site)
#
#
#
#| include: false
# --- Statistiques clés pour le texte narratif ---
h_obs <- res_h |> left_join(obs, by = "j5")
n_men_h <- n_distinct(h_obs$j5)

# Statut d'occupation
statut_s <- h_obs |> mutate(S = decode_labelled(h0)) |>
  filter(!is.na(S)) |>
  count(Observatory, S) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory)
top_statut_a <- statut_s |> filter(Observatory == "Alaotra") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(S)
pct_statut_a <- statut_s |> filter(Observatory == "Alaotra") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(pct)
top_statut_m <- statut_s |> filter(Observatory == "Marovoay") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(S)
pct_statut_m <- statut_s |> filter(Observatory == "Marovoay") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(pct)

# Latrine
latrine_s <- h_obs |> mutate(S = decode_labelled(h5)) |>
  filter(!is.na(S)) |>
  count(Observatory, S) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory)
top_lat_a <- latrine_s |> filter(Observatory == "Alaotra") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(S)
pct_lat_a <- latrine_s |> filter(Observatory == "Alaotra") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(pct)
top_lat_m <- latrine_s |> filter(Observatory == "Marovoay") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(S)
pct_lat_m <- latrine_s |> filter(Observatory == "Marovoay") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(pct)

# Eau
water_s <- h_obs |> mutate(S = decode_labelled(h4)) |>
  filter(!is.na(S)) |>
  count(Observatory, S) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory)
top_eau_a <- water_s |> filter(Observatory == "Alaotra") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(S)
pct_eau_a <- water_s |> filter(Observatory == "Alaotra") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(pct)
top_eau_m <- water_s |> filter(Observatory == "Marovoay") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(S)
pct_eau_m <- water_s |> filter(Observatory == "Marovoay") |>
  slice_max(pct, n = 1, with_ties = FALSE) |> pull(pct)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
house <- res_h |>
  left_join(obs_site, by = "j5") |>
  mutate(Statut = decode_labelled(h0)) |>
  filter(!is.na(Statut)) |>
  count(Observatory, Statut) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory)

house |>
  obs_gt() |>
  tab_header(title = obs_title("Statut d'occupation du logement")) |>
  cols_label(Statut = "Statut", pct = "%") |>
  cols_hide(n) |>
  fmt_number(columns = pct, decimals = 1) |>
  style_table()
#
#
#
#| label: fig-statut-logement
#| fig-cap: "Statut d'occupation du logement"
#| fig-height: !expr fig_h("fig-statut-logement")
#| fig-width: !expr fig_w("fig-statut-logement")
house |> make_bar_obs(x = Statut, title = "Statut d'occupation du logement")
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_house_status", "05", function(d) {
  d |> mutate(Statut = decode_labelled(h0)) |>
    filter(!is.na(Statut)) |>
    count(Observatory, Statut) |>
    mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory) |>
    make_bar_obs(x = Statut, title = "Statut d'occupation du logement")
}, res_h |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
latrine <- res_h |>
  left_join(obs_site, by = "j5") |>
  mutate(Type = decode_labelled(h5)) |>
  filter(!is.na(Type)) |>
  count(Observatory, Type) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory)

latrine |>
  obs_gt() |>
  tab_header(title = obs_title("Types de latrine")) |>
  cols_label(Type = "Type de latrine", pct = "%") |>
  cols_hide(n) |>
  fmt_number(columns = pct, decimals = 1) |>
  style_table()
#
#
#
#| label: fig-latrine
#| fig-cap: "Types de latrine"
#| fig-height: !expr fig_h("fig-latrine")
#| fig-width: !expr fig_w("fig-latrine")
latrine |> make_bar_obs(title = obs_title("Types de latrine"))
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_latrine", "05", function(d) {
  d |> mutate(Type = decode_labelled(h5)) |>
    filter(!is.na(Type)) |>
    count(Observatory, Type) |>
    mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory) |>
    make_bar_obs(title = "Types de latrine par observatoire")
}, res_h |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
water <- res_h |>
  left_join(obs_site, by = "j5") |>
  mutate(Type = str_remove(decode_labelled(h4), "_+$")) |>
  filter(!is.na(Type)) |>
  count(Observatory, Type) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory)

water |>
  obs_gt() |>
  tab_header(title = obs_title("Mode d'approvisionnement en eau")) |>
  cols_label(Type = "Source", pct = "%") |>
  cols_hide(n) |>
  fmt_number(columns = pct, decimals = 1) |>
  style_table()
#
#
#
#| label: fig-eau
#| fig-cap: "Mode d'approvisionnement en eau"
#| fig-height: !expr fig_h("fig-eau")
#| fig-width: !expr fig_w("fig-eau")
water |> make_bar_obs(title = obs_title("Mode d'approvisionnement en eau"))
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_water", "05", function(d) {
  d |> mutate(Type = str_remove(decode_labelled(h4), "_+$")) |>
    filter(!is.na(Type)) |>
    count(Observatory, Type) |>
    mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory) |>
    make_bar_obs(title = "Mode d'approvisionnement en eau")
}, res_h |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
light <- res_h |>
  left_join(obs_site, by = "j5") |>
  tabulate_binary_set("h7", exclude_pattern = "h7c[12]|h7i_autre")

light |>
  obs_gt() |>
  tab_header(title = obs_title("Modes d'éclairage des ménages")) |>
  cols_label(Type = "Type d'eclairage", pct = "%") |>
  cols_hide(n) |>
  fmt_number(columns = pct, decimals = 1) |>
  style_table()
#
#
#
#| label: fig-eclairage
#| fig-cap: "Modes d'éclairage"
#| fig-height: !expr fig_h("fig-eclairage")
#| fig-width: !expr fig_w("fig-eclairage")
light |> make_bar_obs(title = obs_title("Modes d'éclairage"))
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_lighting", "05", function(d) {
  d |> tabulate_binary_set("h7", exclude_pattern = "h7c[12]|h7i_autre") |>
    make_bar_obs(title = "Modes d'eclairage par observatoire")
}, res_h |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
solar_users <- res_h |>
  left_join(obs_site, by = "j5") |>
  filter(h7c == 1, !is.na(h7c1))

solar_status <- solar_users |>
  count(Observatory, En_marche = if_else(h7c1 == 1, "Oui", "Non")) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory) |>
  mutate(Valeur = paste0(pct, "%")) |>
  select(Observatory, En_marche, Valeur) |>
  pivot_wider(names_from = Observatory, values_from = Valeur, values_fill = "-")

gt(solar_status) |>
  tab_header(title = "Panneau solaire en état de marche (parmi les utilisateurs du solaire)") |>
  cols_label(En_marche = "En marche") |>
  style_table()
#
#
#
#| include: false
dl_tbl_variants("tbl_lighting", "05", function(d) {
  d |> tabulate_binary_set("h7", exclude_pattern = "h7c[12]|h7i_autre") |>
    rename(Site = Observatory, Effectif = n, `%` = pct)
}, res_h |> left_join(obs, by = "j5"))
dl_fig_variants("fig_lighting", "05", function(d) {
  d |> tabulate_binary_set("h7", exclude_pattern = "h7c[12]|h7i_autre") |>
    make_bar_obs(title = "Modes d'eclairage par observatoire")
}, res_h |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
cooking <- res_h |>
  left_join(obs_site, by = "j5") |>
  tabulate_binary_set("h6", exclude_pattern = "h6a1|h6e_autre") |>
  mutate(Type = case_when(
    str_detect(Type, "(?i)r\u00e9sin") ~ "Bois de chauffe",
    str_detect(Type, "(?i)autre.*pr") ~ "Autre",
    TRUE ~ Type
  ))

cooking |>
  obs_gt() |>
  tab_header(title = obs_title("Modes de cuisson des ménages")) |>
  cols_label(Type = "Source d'energie", pct = "%") |>
  cols_hide(n) |>
  fmt_number(columns = pct, decimals = 1) |>
  style_table()
#
#
#
#| fig-height: !expr fig_h("fig-cooking")
cooking |> make_bar_obs(title = obs_title("Modes de cuisson"))
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_cooking", "05", function(d) {
  d |> tabulate_binary_set("h6", exclude_pattern = "h6a1|h6e_autre") |>
    mutate(Type = case_when(
      str_detect(Type, "(?i)r\u00e9sin") ~ "Bois de chauffe",
      str_detect(Type, "(?i)autre.*pr") ~ "Autre",
      TRUE ~ Type
    )) |>
    make_bar_obs(title = "Modes de cuisson par observatoire")
}, res_h |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
comfort <- res_v |>
  left_join(obs_site, by = "j5") |>
  tabulate_binary_set("v", count_mode = TRUE) |>
  mutate(Type = if_else(str_detect(Type, "(?i)autre.*pr"), "Autre", Type))

comfort |>
  obs_gt() |>
  tab_header(title = obs_title("Taux de possession de biens de confort courants")) |>
  cols_label(Type = "Bien", pct = "%") |>
  cols_hide(n) |>
  fmt_number(columns = pct, decimals = 1) |>
  style_table()
#
#
#
#| fig-height: !expr fig_h("fig-comfort")
comfort |> make_bar_obs(title = obs_title("Biens de confort courants"))
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_comfort", "05", function(d) {
  d |> tabulate_binary_set("v", count_mode = TRUE) |>
    mutate(Type = if_else(str_detect(Type, "(?i)autre.*pr"), "Autre", Type)) |>
    make_bar_obs(title = "Biens de confort courants par observatoire")
}, res_v |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
# Keep common hand tools + mechanisation/traction items
equip_keep <- c(
  "Angady", "Faucille/Coupe-coupe", "Hache", "Arrosoir/Sceau", "Couteau",
  "R\u00e2teau, fourche", "Houe", "Pioche",
  "Charrue", "Herse/Pulv\u00e9riseur/Rotovat", "Sarcleuse",
  "Charrette attel\u00e9e", "Motoculteur, type Kubota", "Tracteur",
  "Pulv\u00e9risateur", "Pompe hydraulique"
)

equip_data <- res_e |>
  left_join(obs_site, by = "j5") |>
  mutate(Equipement = decode_labelled(e11)) |>
  filter(!is.na(Equipement), Equipement != "", str_detect(Equipement, paste(equip_keep, collapse = "|")))

tools <- equip_data |> count(Observatory, Equipement, name = "N_outils")
households <- equip_data |>
  distinct(j5, Observatory, Equipement) |>
  count(Observatory, Equipement, name = "N_menages")
total_hh <- obs_site |> count(Observatory, name = "total_hh")

table_equip <- tools |>
  left_join(households, by = c("Observatory", "Equipement")) |>
  left_join(total_hh, by = "Observatory") |>
  mutate(pct_menages = round(N_menages / total_hh * 100, 1)) |>
  select(Observatory, Equipement, N_menages, pct_menages, N_outils) |>
  arrange(Observatory, desc(pct_menages))

table_equip |>
  obs_gt() |>
  tab_header(title = obs_title("Taux de possession d'équipements agricoles")) |>
  cols_label(
    Equipement = "Equipement",
    pct_menages = "Ménages (%)", N_outils = "Nb outils"
  ) |>
  cols_hide(N_menages) |>
  fmt_number(columns = N_outils, decimals = 0) |>
  fmt_number(columns = pct_menages, decimals = 1) |>
  style_table()
#
#
#
#| fig-height: !expr fig_h("fig-table-equip")
table_equip |>
  make_bar_obs(x = Equipement, y = pct_menages,
          title = obs_title("Équipements agricoles"), ncol = 2)
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_agri_equip", "05", function(d) {
  equip <- d |> mutate(Equipement = decode_labelled(e11)) |>
    filter(!is.na(Equipement), Equipement != "", str_detect(Equipement, paste(equip_keep, collapse = "|"))) |>
    distinct(j5, Observatory, Equipement) |>
    count(Observatory, Equipement, name = "N_menages") |>
    left_join(d |> distinct(j5, Observatory) |> count(Observatory, name = "total"),
              by = "Observatory") |>
    mutate(pct_menages = round(N_menages / total * 100, 1))
  equip |>
    make_bar_obs(x = Equipement, y = pct_menages,
            title = "Equipements agricoles par observatoire", ncol = 1)
}, res_e |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
comm_vars <- c("v5", "v8", "v9", "v20", "v11")
comm_labels <- tibble(
  v_var = comm_vars,
  Type = vapply(res_v[comm_vars], function(x) {
    lab <- iconv(var_label(x), from = "latin1", to = "UTF-8")
    if (grepl(":", lab)) lab <- sub(".*?:\\s*", "", lab)
    str_squish(str_to_title(lab))
  }, character(1))
)

comm <- res_v |>
  select(j5, all_of(comm_vars)) |>
  left_join(obs_site, by = "j5") |>
  pivot_longer(all_of(comm_vars), names_to = "v_var", values_to = "value") |>
  filter(value == 1) |>
  left_join(comm_labels, by = "v_var") |>
  count(Observatory, Type) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory)

comm |>
  obs_gt() |>
  tab_header(title = obs_title("Matériels de communication audiovisuelle et télécommunication")) |>
  cols_label(Type = "Equipement", pct = "%") |>
  cols_hide(n) |>
  fmt_number(columns = pct, decimals = 1) |>
  style_table()
#
#
#
#| fig-height: !expr fig_h("fig-comm")
comm |> make_bar_obs(title = obs_title("Matériels de communication"))
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_communication", "05", function(d) {
  comm_vars <- c("v5", "v8", "v9", "v20", "v11")
  comm_labels <- tibble(
    v_var = comm_vars,
    Type = vapply(d[comm_vars], function(x) {
      lab <- iconv(var_label(x), from = "latin1", to = "UTF-8")
      if (grepl(":", lab)) lab <- sub(".*?:\\s*", "", lab)
      str_squish(str_to_title(lab))
    }, character(1)))
  d |> select(j5, Observatory, all_of(comm_vars)) |>
    pivot_longer(all_of(comm_vars), names_to = "v_var", values_to = "value") |>
    filter(value == 1) |>
    left_join(comm_labels, by = "v_var") |>
    count(Observatory, Type) |>
    mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory) |>
    make_bar_obs(title = "Materiels de communication par observatoire")
}, res_v |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
res_t2 <- read_dta("data/ROS_MDG_microdata/2025/res_t2.dta") |> mutate(j5 = as.character(j5))

type_transf_labs <- c(
  "1" = "Argent", "2" = "Riz", "3" = "Autres produits", "99" = "Autre"
)

res_t2_obs <- res_t2 |>
  left_join(obs, by = "j5") |>
  mutate(
    Type = if_else(
      as.character(as.integer(t21a)) %in% names(type_transf_labs),
      type_transf_labs[as.character(as.integer(t21a))],
      "En nature autre que le riz"
    )
  )
#
#
#
val_par_type <- res_t2_obs |>
  expand_sites_for_profile() |>
  filter(!is.na(t21d), t21d > 0) |>
  summarise(
    n = n(),
    val_moy = round(mean(t21d, na.rm = TRUE)),
    val_med = round(median(t21d, na.rm = TRUE)),
    .by = c(Observatory, Type)
  ) |>
  filter(n >= 5)
#
#
#
#| fig-cap: "Valeur moyenne des transferts sortants par observatoire et type"
#| fig-width: !expr fig_w("fig-transferts-sortants")
#| fig-height: !expr fig_h("fig-transferts-sortants")
val_par_type |>
  mutate(val_moy_k = val_moy / 1000) |>
  make_bar_obs(x = Type, y = val_moy_k,
          y_label = "Valeur moyenne (milliers Ar)", show_pct = FALSE)
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_transferts", "05", function(d) {
  val <- d |> filter(!is.na(t21d), t21d > 0) |>
    mutate(Type = if_else(
      as.character(as.integer(t21a)) %in% names(type_transf_labs),
      type_transf_labs[as.character(as.integer(t21a))], "Non precise")) |>
    summarise(val_moy = round(mean(t21d, na.rm = TRUE)), n = n(),
              .by = c(Observatory, Type)) |> filter(n >= 5)
  val |>
    mutate(val_moy_k = val_moy / 1000) |>
    ror_bar_grouped(x = Type, y = val_moy_k, fill = Observatory,
                    y_label = "Valeur moyenne (milliers Ar)",
                    direction = "horizontal", facet = FALSE,
                    show_pct = FALSE)
}, res_t2 |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
##| tbl-cap: "Transferts sortants : resume par observatoire"

# n_t2 <- res_t2_obs |>
#   summarise(n_envoyeurs = n_distinct(j5), .by = Observatory) |>
#   left_join(res_deb |> count(Observatory, name = "n_total"), by = "Observatory") |>
#   mutate(pct = round(n_envoyeurs / n_total * 100, 1))
# 
# n_t2 |>
#   gt() |>
#   tab_header(title = "Menages envoyant des transferts") |>
#   cols_label(n_envoyeurs = "Nb envoyeurs", n_total = "Nb menages total", pct = "% envoyeurs") |>
#   style_table()
#
#
#
##| echo: false
##| results: asis
# dl_tbl_variants("tbl_transferts", "05", function(d) {
#   t2_ids <- unique(as.character(res_t2$j5))
#   d |>
#     mutate(envoyeur = j5 %in% t2_ids) |>
#     summarise(Envoyeurs = sum(envoyeur), Total = n(), .by = Observatory) |>
#     mutate(`%` = round(Envoyeurs / Total * 100, 1)) |>
#     rename(Site = Observatory)
# }, res_deb)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
dest_labs <- c(
  "1" = "Meme commune", "2" = "Autre commune, meme region",
  "3" = "Autre region", "4" = "Etranger"
)

dest_data <- res_t2_obs |>
  expand_sites_for_profile() |>
  filter(!is.na(t41)) |>
  mutate(
    Destination = if_else(
      as.character(as.integer(t41)) %in% names(dest_labs),
      dest_labs[as.character(as.integer(t41))],
      "Non precise"
    )
  ) |>
  count(Observatory, Destination) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory)
#
#
#
#| fig-cap: "Lieu de residence des destinataires de transferts"
#| fig-width: !expr fig_w("fig-destination-transferts")
#| fig-height: !expr fig_h("fig-destination-transferts")
dest_data |>
  make_bar_obs(x = Destination, y_label = "% des transferts")
#
#
#
#| echo: false
#| results: asis
dl_fig_variants("fig_destination", "05", function(d) {
  dest_labs <- c("1" = "Meme commune", "2" = "Autre commune, meme region",
                 "3" = "Autre region", "4" = "Etranger")
  d |> filter(!is.na(t41)) |>
    mutate(Destination = if_else(
      as.character(as.integer(t41)) %in% names(dest_labs),
      dest_labs[as.character(as.integer(t41))], "Non precise")) |>
    count(Observatory, Destination) |>
    mutate(pct = round(n / sum(n) * 100, 1), .by = Observatory) |>
    ror_bar_grouped(x = Destination, fill = Observatory,
                    y_label = "% des transferts", direction = "horizontal",
                    facet = FALSE)
}, res_t2 |> left_join(obs, by = "j5"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
source("utils/calc_incomes_all_years.R")

# Calcul des échelles d'équivalence (OCDE modifiée)
# 1,0 pour le premier adulte du ménage ;
# 0,5 pour les autres personnes de 14 ans ou plus ;
# 0,3 pour les enfants de moins de 14 ans.
weights <- read_dta("data/ROS_MDG_microdata/2025/res_m_a.dta") |>
  group_by(j5) |>
  summarise(
    hh_size = n(),
    eq_oecd = sum(case_when(
      row_number() == 1 ~ 1.0,
      m5 >= 14 ~ 0.5,
      m5 < 14 ~ 0.3,
      TRUE ~ 0.5
    )),
    .groups = "drop"
  ) |>
  mutate(j5 = as.character(j5))

inc <- compute_income_year(2025) |>
  mutate(j5 = as.character(j5)) |>
  left_join(obs, by = "j5") |>
  left_join(weights, by = "j5") |>
  mutate(
    revtot_pc = revtot / hh_size,
    revtot_eq = revtot / eq_oecd
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
inc |>
  summarise(
    `Nb ménages` = n(),
    Moyenne = round(mean(revtot, na.rm = TRUE)),
    Médiane = round(median(revtot, na.rm = TRUE)),
    `Par tête (moy.)` = round(mean(revtot_pc, na.rm = TRUE)),
    `Par équivalent (moy.)` = round(mean(revtot_eq, na.rm = TRUE)),
    .by = Observatory
  ) |>
  gt() |>
  tab_header(
    title = "Revenu total annuel (Ar/an)",
    subtitle = "Comparaison par ménage, par tête et par équivalent (OCDE modifiée)"
  ) |>
  fmt_number(columns = Moyenne:`Par équivalent (moy.)`, use_seps = TRUE, decimals = 0) |>
  style_table()
#
#
#
#
#
inc |>
  summarise(
    `Nb ménages` = n(),
    Moyenne = round(mean(revtot, na.rm = TRUE)),
    Médiane = round(median(revtot, na.rm = TRUE)),
    `Écart-type` = round(sd(revtot, na.rm = TRUE)),
    Minimum = round(min(revtot, na.rm = TRUE)),
    Maximum = round(max(revtot, na.rm = TRUE)),
    .by = Observatory
  ) |>
  gt() |>
  tab_header(
    title = "Statistiques descriptives du revenu total par ménage",
    subtitle = "Campagne 2025"
  ) |>
  fmt_number(columns = Moyenne:Maximum, use_seps = TRUE, decimals = 0) |>
  style_table()
#
#
#
#| echo: false
#| results: asis
dl_tbl_variants("tbl_revtot", "05", function(d) {
  d |> summarise(`Nb menages` = n(),
    Moyenne = round(mean(revtot, na.rm = TRUE)),
    Mediane = round(median(revtot, na.rm = TRUE)),
    `Ecart-type` = round(sd(revtot, na.rm = TRUE)),
    .by = Observatory) |>
    rename(Site = Observatory)
}, inc)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
decomp <- inc |>
  summarise(
    `Salaires agricoles` = mean(revsec, na.rm = TRUE),
    `Salaires non-agricoles` = mean(revppal, na.rm = TRUE),
    `Revenus independants` = mean(rev_indep, na.rm = TRUE),
    `Revenu net rizicole` = mean(rev_riz, na.rm = TRUE),
    `Revenu net autres cultures` = mean(rev_cu, na.rm = TRUE),
    Elevage = mean(revel, na.rm = TRUE),
    Peche = mean(revpeche, na.rm = TRUE),
    .by = Observatory
  ) |>
  pivot_longer(-Observatory, names_to = "Composante", values_to = "Montant") |>
  pivot_wider(names_from = Observatory, values_from = Montant)

obs_cols <- setdiff(names(decomp), "Composante")
totaux <- colSums(decomp[obs_cols])
decomp_pct <- decomp |>
  mutate(across(all_of(obs_cols), ~ . / totaux[cur_column()] * 100))

total_montants <- as.list(totaux)
total_pct <- as.list(setNames(rep(100, length(obs_cols)), obs_cols))

decomp_display <- bind_cols(
  decomp,
  decomp_pct |> select(all_of(obs_cols)) |> rename_with(~ paste0(., " (%)"))
)
total_display <- tibble(
  Composante = "Total revenu courant",
  !!!total_montants,
  !!!setNames(total_pct, paste0(names(total_pct), " (%)"))
)

bind_rows(decomp_display, total_display) |>
  gt() |>
  tab_header(
    title = "Decomposition du revenu courant moyen par menage (Ar/an)",
    subtitle = "Campagne 2025"
  ) |>
  fmt_number(columns = all_of(obs_cols), use_seps = TRUE, decimals = 0) |>
  fmt_number(columns = ends_with("(%)"), decimals = 1) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Composante == "Total revenu courant")
  ) |>
  style_table()
#
#
#
#| fig-cap: "Part des composantes dans le revenu courant moyen par observatoire"
#| fig-width: !expr fig_w("fig_revcou_shares")
#| fig-height: !expr fig_h("fig_revcou_shares")

inc |>
  summarise(
    `Salaires agricoles` = mean(revsec, na.rm = TRUE),
    `Salaires non-agricoles` = mean(revppal, na.rm = TRUE),
    `Revenus independants` = mean(rev_indep, na.rm = TRUE),
    `Revenu net rizicole` = mean(rev_riz, na.rm = TRUE),
    `Revenu net autres cultures` = mean(rev_cu, na.rm = TRUE),
    Elevage = mean(revel, na.rm = TRUE),
    .by = Observatory
  ) |>
  pivot_longer(-Observatory, names_to = "Composante", values_to = "Montant") |>
  mutate(pct = Montant / sum(Montant), .by = Observatory) |>
  ggplot(aes(x = Observatory, y = Montant, fill = Composante)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = ifelse(pct > 0.04, paste0(round(pct * 100), "%"), "")),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = "Part du revenu courant", fill = NULL) +
  theme_ror()
#
#
#
#| echo: false
#| results: asis
dl_tbl_variants("tbl_revcou_decomp", "05", function(d) {
  d |> summarise(
    `Salaires agricoles` = round(mean(revsec, na.rm = TRUE)),
    `Salaires non-agricoles` = round(mean(revppal, na.rm = TRUE)),
    `Revenus independants` = round(mean(rev_indep, na.rm = TRUE)),
    `Revenu net rizicole` = round(mean(rev_riz, na.rm = TRUE)),
    `Revenu net autres cultures` = round(mean(rev_cu, na.rm = TRUE)),
    Elevage = round(mean(revel, na.rm = TRUE)),
    Peche = round(mean(revpeche, na.rm = TRUE)),
    .by = Observatory) |>
    pivot_longer(-Observatory, names_to = "Composante", values_to = "Montant") |>
    rename(Site = Observatory)
}, inc)
dl_fig_variants("fig_revcou_shares", "05", function(d) {
  d |> summarise(
    `Salaires agricoles` = mean(revsec, na.rm = TRUE),
    `Salaires non-agricoles` = mean(revppal, na.rm = TRUE),
    `Revenus independants` = mean(rev_indep, na.rm = TRUE),
    `Revenu net rizicole` = mean(rev_riz, na.rm = TRUE),
    `Revenu net autres cultures` = mean(rev_cu, na.rm = TRUE),
    Elevage = mean(revel, na.rm = TRUE),
    .by = Observatory) |>
    pivot_longer(-Observatory, names_to = "Composante", values_to = "Montant") |>
    mutate(pct = Montant / sum(Montant), .by = Observatory) |>
    ggplot(aes(x = Observatory, y = Montant, fill = Composante)) +
    geom_col(position = "fill") +
    geom_text(
      aes(label = ifelse(pct > 0.04, paste0(round(pct * 100), "%"), "")),
      position = position_fill(vjust = 0.5),
      color = "white",
      size = 3
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = "Part du revenu courant", fill = NULL) +
    theme_ror()
}, inc)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
inc |>
  summarise(
    `Revenu courant` = round(mean(revcou, na.rm = TRUE)),
    `Revenu exceptionnel` = round(mean(revexcept, na.rm = TRUE)),
    `Revenu total` = round(mean(revtot, na.rm = TRUE)),
    `Part courant (%)` = round(mean(revcou, na.rm = TRUE) / mean(revtot, na.rm = TRUE) * 100, 1),
    .by = Observatory
  ) |>
  gt() |>
  tab_header(
    title = "Revenu courant et revenu exceptionnel moyens par menage (Ar/an)",
    subtitle = "Campagne 2025"
  ) |>
  fmt_number(
    columns = c(`Revenu courant`, `Revenu exceptionnel`, `Revenu total`),
    use_seps = TRUE, decimals = 0
  ) |>
  style_table()
#
#
#
#| echo: false
#| results: asis
dl_tbl_variants("tbl_revcou_revexcept", "05", function(d) {
  d |>
    summarise(
      `Revenu courant` = round(mean(revcou, na.rm = TRUE)),
      `Revenu exceptionnel` = round(mean(revexcept, na.rm = TRUE)),
      `Revenu total` = round(mean(revtot, na.rm = TRUE)),
      `Part courant (%)` = round(mean(revcou, na.rm = TRUE) /
                                   mean(revtot, na.rm = TRUE) * 100, 1),
      .by = Observatory
    ) |>
    rename(Site = Observatory)
}, inc)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false


# Function to get household scales for a specific year and observatory
# We use the 2025 definition for OECD equivalent scales (m5 is age)
# For older years, variables might differ, but we try to be consistent.
get_scales_all_years <- function(yr) {
  f <- paste0("data/ROS_MDG_microdata/", yr, "/res_m_a.dta")
  if (!file.exists(f)) return(tibble())
  
  d <- read_dta(f) |> mutate(j5 = as.character(j5), year = yr)
  
  # Standardizing age variable name if needed (usually m5)
  age_col <- if ("m5" %in% names(d)) "m5" else if ("am5" %in% names(d)) "am5" else NULL
  
  if (is.null(age_col)) {
    # If age is missing, we approximate hh_size and assume adults
    return(d |> group_by(j5, year) |> summarise(hh_size = n(), eq_oecd = 1 + (n()-1)*0.5, .groups = "drop"))
  }
  
  d |>
    group_by(j5, year) |>
    summarise(
      hh_size = n(),
      eq_oecd = sum(case_when(
        row_number() == 1 ~ 1.0,
        !!sym(age_col) >= 14 ~ 0.5,
        !!sym(age_col) < 14 ~ 0.3,
        TRUE ~ 0.5
      )),
      .groups = "drop"
    )
}

trends_file <- "output/inc_trends_obs.rds"

if (file.exists(trends_file)) {
  trends <- readRDS(trends_file)
} else {
  all_inc <- compute_income_all_years(c(1995:2015, 2025))

  obs_lookup <- map_dfr(c(1995:2015, 2025), function(yr) {
    f <- paste0("data/ROS_MDG_microdata/", yr, "/res_deb.dta")
    if (!file.exists(f)) return(tibble())
    read_dta(f) |>
      mutate(j5 = as.character(j5), year = yr) |>
      select(j5, year, j0) |>
      distinct(j5, year, .keep_all = TRUE)
  })

  scales_lookup <- map_dfr(c(1995:2015, 2025), get_scales_all_years)

  all_obs <- all_inc |>
    left_join(scales_lookup, by = c("j5", "year")) |>
    left_join(obs_lookup, by = c("j5", "year")) |>
    filter(j0 %in% c(3, 21)) |>
    mutate(Observatory = if_else(j0 == 3, "Marovoay", "Alaotra"))

  trends <- all_obs |>
    summarise(
      n = n(),
      hh_size_mean   = mean(hh_size, na.rm = TRUE),
      eq_oecd_mean   = mean(eq_oecd, na.rm = TRUE),
      revcou_mean    = mean(revcou, na.rm = TRUE),
      revcou_median  = median(revcou, na.rm = TRUE),
      revexcept_mean = mean(revexcept, na.rm = TRUE),
      revtot_mean    = mean(revtot, na.rm = TRUE),
      revtot_median  = median(revtot, na.rm = TRUE),
      revppal_mean   = mean(revppal, na.rm = TRUE),
      revsec_mean    = mean(revsec, na.rm = TRUE),
      rev_indep_mean = mean(rev_indep, na.rm = TRUE),
      rev_riz_mean   = mean(rev_riz, na.rm = TRUE),
      rev_cu_mean    = mean(rev_cu, na.rm = TRUE),
      revel_mean     = mean(revel, na.rm = TRUE),
      revpeche_mean  = mean(revpeche, na.rm = TRUE),
      .by = c(year, Observatory)
    )

  # Prix median du paddy par observatoire
  paddy_prices <- map_dfr(c(1995:2015, 2025), function(yr) {
    f <- paste0("data/ROS_MDG_microdata/", yr, "/res_dc21.dta")
    if (!file.exists(f)) return(tibble())
    d <- read_dta(f) |> mutate(j5 = as.character(j5), year = yr)
    if (!"dc24" %in% names(d)) return(tibble())
    d |>
      left_join(obs_lookup |> filter(year == yr), by = c("j5", "year")) |>
      filter(j0 %in% c(3, 21), !is.na(dc24), dc24 > 0) |>
      mutate(Observatory = if_else(j0 == 3, "Marovoay", "Alaotra")) |>
      summarise(prix_paddy_median = median(dc24, na.rm = TRUE),
                .by = c(year, Observatory))
  })

  trends <- trends |> left_join(paddy_prices, by = c("year", "Observatory"))

  # IPC Banque Mondiale (base 2010 = 100)
  library(jsonlite)
  wb_raw <- fromJSON(
    "https://api.worldbank.org/v2/country/MDG/indicator/FP.CPI.TOTL?date=1995:2025&format=json&per_page=50"
  )
  cpi_wb <- tibble(
    year = as.integer(wb_raw[[2]]$date),
    cpi  = as.numeric(wb_raw[[2]]$value)
  ) |> filter(!is.na(cpi)) |> arrange(year)

  cpi_full <- bind_rows(
    cpi_wb,
    tibble(year = 2024L, cpi = cpi_wb$cpi[cpi_wb$year == 2023] * 1.09),
    tibble(year = 2025L, cpi = cpi_wb$cpi[cpi_wb$year == 2023] * 1.09^2)
  )

  trends <- trends |>
    left_join(cpi_full, by = "year") |>
    mutate(
      deflator       = cpi / 100,
      revtot_real_pc = (revtot_mean / hh_size_mean) / deflator,
      revtot_real_eq = (revtot_mean / eq_oecd_mean) / deflator,
      revcou_real    = revcou_mean / deflator,
      revtot_real    = revtot_mean / deflator,
      revexcept_real = revexcept_mean / deflator,
      revppal_real   = revppal_mean / deflator,
      revsec_real    = revsec_mean / deflator,
      rev_indep_real = rev_indep_mean / deflator,
      rev_riz_real   = rev_riz_mean / deflator,
      rev_cu_real    = rev_cu_mean / deflator,
      revel_real     = revel_mean / deflator,
      revpeche_real  = revpeche_mean / deflator
    )

  saveRDS(trends, trends_file)
}

# Deflation par le prix du paddy
paddy_base_2010 <- trends |>
  filter(year == 2010) |>
  select(Observatory, paddy_base = prix_paddy_median)

trends <- trends |>
  left_join(paddy_base_2010, by = "Observatory") |>
  mutate(
    deflator_paddy  = prix_paddy_median / paddy_base,
    revtot_paddy    = revtot_mean / deflator_paddy,
    revcou_paddy    = revcou_mean / deflator_paddy,
    revexcept_paddy = revexcept_mean / deflator_paddy
  ) |>
  select(-paddy_base)
#
#
#
#| fig-cap: "Evolution du revenu total moyen par menage (Ar courants)"
#| fig-width: !expr fig_w("fig_trends_revtot")
#| fig-height: !expr fig_h("fig_trends_revtot")

trends |> make_trend_plot(revtot_mean, "Revenu total moyen (Ar/menage/an)")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Evolution du revenu courant moyen par menage (Ar courants)"
#| fig-width: !expr fig_w("fig_trends_revcou")
#| fig-height: !expr fig_h("fig_trends_revcou")

trends |> make_trend_plot(revcou_mean, "Revenu courant moyen (Ar/menage/an)")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Evolution des composantes du revenu courant moyen (Ar courants)"
#| fig-width: !expr fig_w("fig_trends_decomp")
#| fig-height: !expr fig_h("fig_trends_decomp")

# Note: certaines composantes (rev_cu en 2002-2004) sont negatives en raison
# de charges superieures aux recettes. On utilise geom_col qui gere
# correctement les valeurs negatives (barres sous l'axe zero).
trends |>
  select(year, Observatory,
         `Salaires agricoles` = revsec_mean,
         `Salaires non-agricoles` = revppal_mean,
         `Revenus independants` = rev_indep_mean,
         `Revenu net rizicole` = rev_riz_mean,
         `Revenu net autres cultures` = rev_cu_mean,
         Elevage = revel_mean,
         Peche = revpeche_mean) |>
  pivot_longer(-c(year, Observatory), names_to = "Composante", values_to = "Montant") |>
  ggplot(aes(x = factor(year), y = Montant, fill = Composante)) +
  geom_col(position = "stack") +
  facet_wrap(~Observatory) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = NULL, y = "Revenu courant moyen (Ar/menage/an)", fill = NULL) +
  theme_ror() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "bottom")
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Evolution de l'IPC national Madagascar (base 2010 = 100)"
#| fig-width: !expr fig_w("fig_ipc_national")
#| fig-height: !expr fig_h("fig_ipc_national")

cpi_series <- trends |>
  distinct(year, cpi) |>
  filter(!is.na(cpi)) |>
  arrange(year)

cpi_series |>
  ggplot(aes(x = year, y = cpi)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  annotate("text", x = 2011, y = 105, label = "Base 2010 = 100",
           hjust = 0, size = 3, colour = "grey40") +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  labs(x = NULL, y = "IPC national (base 2010 = 100)") +
  theme_ror()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Evolution du prix median du paddy (Ar/kg)"
#| fig-width: !expr fig_w("fig_trends_paddy")
#| fig-height: !expr fig_h("fig_trends_paddy")

trends |> make_trend_plot(prix_paddy_median, "Prix median du paddy (Ar/kg)")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Comparaison de l'IPC national et de l'indice du prix du paddy (base 100 en 2010)"
#| fig-width: !expr fig_w("fig_ipc_vs_paddy")
#| fig-height: !expr fig_h("fig_ipc_vs_paddy")

paddy_2010 <- trends |>
  filter(year == 2010) |>
  select(Observatory, paddy_base = prix_paddy_median)

index_paddy <- trends |>
  left_join(paddy_2010, by = "Observatory") |>
  mutate(indice_paddy = prix_paddy_median / paddy_base * 100) |>
  select(year, Observatory, indice_paddy)

index_cpi <- cpi_series |>
  mutate(Indicateur = "IPC national") |>
  rename(indice = cpi)

index_all <- index_paddy |>
  mutate(Indicateur = paste0("Prix paddy ", Observatory)) |>
  rename(indice = indice_paddy) |>
  select(year, Indicateur, indice) |>
  bind_rows(index_cpi)

index_all |>
  ggplot(aes(x = year, y = indice, colour = Indicateur)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  labs(x = NULL, y = "Indice (base 100 en 2010)", colour = NULL) +
  theme_ror()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Evolution du revenu total reel moyen par menage (Ar constants 2010, IPC)"
#| fig-width: !expr fig_w("fig_trends_revtot_real")
#| fig-height: !expr fig_h("fig_trends_revtot_real")

trends |> make_trend_plot(revtot_real, "Revenu total reel moyen (Ar constants 2010)")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Evolution du revenu courant reel moyen par menage (Ar constants 2010, IPC)"
#| fig-width: !expr fig_w("fig_trends_revcou_real")
#| fig-height: !expr fig_h("fig_trends_revcou_real")

trends |> make_trend_plot(revcou_real, "Revenu courant reel moyen (Ar constants 2010)")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Evolution du revenu total reel moyen par menage (Ar constants paddy 2010)"
#| fig-width: !expr fig_w("fig_trends_revtot_paddy")
#| fig-height: !expr fig_h("fig_trends_revtot_paddy")

trends |> make_trend_plot(revtot_paddy, "Revenu total reel moyen (Ar constants paddy 2010)")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Evolution du revenu courant reel moyen par menage (Ar constants paddy 2010)"
#| fig-width: !expr fig_w("fig_trends_revcou_paddy")
#| fig-height: !expr fig_h("fig_trends_revcou_paddy")

trends |> make_trend_plot(revcou_paddy, "Revenu courant reel moyen (Ar constants paddy 2010)")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false

habitat_file <- "output/habitat_trends_obs.rds"

if (file.exists(habitat_file)) {
  habitat_trends <- readRDS(habitat_file)
} else {
  years_dir <- "data/ROS_MDG_microdata"
  all_years <- c(1995, 1997:2015, 2025)

  habitat_trends <- map_dfr(all_years, function(yr) {
    f_deb <- file.path(years_dir, yr, "res_deb.dta")
    f_h   <- file.path(years_dir, yr, "res_h.dta")
    if (!file.exists(f_deb) || !file.exists(f_h)) return(tibble())

    deb <- read_dta(f_deb) |>
      mutate(j5 = as.character(j5)) |>
      filter(j0 %in% c(3, 21)) |>
      mutate(Observatory = if_else(j0 == 3, "Marovoay", "Alaotra"))

    h <- read_dta(f_h) |> mutate(j5 = as.character(j5))
    d <- h |>
      select(-any_of("year")) |>
      inner_join(deb |> select(j5, Observatory), by = "j5") |>
      mutate(year = yr)

    has_h7  <- "h7" %in% names(d)
    has_h7a <- "h7a" %in% names(d)
    pct_elec <- if (has_h7) {
      d |> summarise(pct_elec = round(mean(h7 == 1, na.rm = TRUE) * 100, 1),
                     .by = c(year, Observatory))
    } else if (has_h7a) {
      d |> summarise(pct_elec = round(mean(h7a == 1, na.rm = TRUE) * 100, 1),
                     .by = c(year, Observatory))
    } else tibble()

    has_h4 <- "h4" %in% names(d)
    pct_water <- if (has_h4) {
      d |> summarise(
        pct_eau_amelioree = round(mean(h4 %in% c(1, 2, 3, 4), na.rm = TRUE) * 100, 1),
        .by = c(year, Observatory)
      )
    } else tibble()

    out <- tibble(year = yr, Observatory = unique(deb$Observatory))
    if (nrow(pct_elec) > 0)  out <- out |> left_join(pct_elec, by = c("year", "Observatory"))
    if (nrow(pct_water) > 0) out <- out |> left_join(pct_water, by = c("year", "Observatory"))
    out
  })

  saveRDS(habitat_trends, habitat_file)
}

hab_solid <- habitat_trends |> filter(year <= 2015)
hab_gap   <- habitat_trends |> filter(year %in% c(2015, 2025))
#
#
#
#| fig-cap: "Evolution de l'acces a l'electricite (1997-2025)"
#| fig-width: !expr fig_w("fig_habitat_elec")
#| fig-height: !expr fig_h("fig_habitat_elec")

if ("pct_elec" %in% names(habitat_trends)) {
  ggplot(mapping = aes(x = year, y = pct_elec, colour = Observatory)) +
    geom_line(data = hab_solid |> filter(!is.na(pct_elec)), linewidth = 0.8) +
    geom_line(data = hab_gap |> filter(!is.na(pct_elec)), linewidth = 0.8, linetype = "31") +
    geom_point(data = habitat_trends |> filter(!is.na(pct_elec)), size = 2) +
    scale_x_continuous(breaks = seq(1995, 2025, 5)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = NULL, y = "% de menages avec electricite", colour = "Observatoire") +
    theme_ror()
}
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Evolution de l'acces a une source d'eau amelioree (1997-2025)"
#| fig-width: !expr fig_w("fig_habitat_water")
#| fig-height: !expr fig_h("fig_habitat_water")

if ("pct_eau_amelioree" %in% names(habitat_trends)) {
  ggplot(mapping = aes(x = year, y = pct_eau_amelioree, colour = Observatory)) +
    geom_line(data = hab_solid |> filter(!is.na(pct_eau_amelioree)), linewidth = 0.8) +
    geom_line(data = hab_gap |> filter(!is.na(pct_eau_amelioree)), linewidth = 0.8, linetype = "31") +
    geom_point(data = habitat_trends |> filter(!is.na(pct_eau_amelioree)), size = 2) +
    scale_x_continuous(breaks = seq(1995, 2025, 5)) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = NULL, y = "% de menages avec eau amelioree", colour = "Observatoire") +
    theme_ror()
}
#
#
#
#
