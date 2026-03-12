setwd(
  "C:/Users/fbede/Documents/Statistiques/madagascar-rural-observatories-2025"
)
library(tidyverse)
library(haven)
library(fixest)
source("utils/helpers_report.R")
source("utils/sites.R")
source("utils/calc_incomes_all_years.R")

MARO_TREATED <- c("031", "032")
MARO_CONTROL <- c("033", "034")
MARO_ALL <- c(MARO_TREATED, MARO_CONTROL)
ALAOTRA_ALL <- c("211", "212", "213")
ALL_SITES <- c(MARO_ALL, ALAOTRA_ALL)
TREAT_YEAR <- 2003
PRE_YEARS <- 1999:2002
HIST_YEARS <- 1999:2014

hc <- read_rds("data/household_consolidated.rds")
mk_income <- function(df) {
  df |>
    mutate(
      rev_riz = coalesce(rev_riz, 0),
      rev_cu = coalesce(rev_cu, 0),
      revel = coalesce(revel, 0),
      revpeche = coalesce(revpeche, 0),
      revppal = coalesce(revppal, 0),
      revsec = coalesce(revsec, 0),
      revcou = coalesce(
        revcou,
        revppal + revsec + rev_riz + rev_cu + revel + revpeche
      ),
      revtot = coalesce(revtot, revcou),
      rev_agri = rev_riz + rev_cu + revel + revpeche,
      rev_nonagri = revppal + revsec
    ) |>
    mutate(across(
      c(revtot, revcou, rev_agri, rev_nonagri),
      ~ pmin(.x, quantile(.x, 0.99, na.rm = TRUE)),
      .names = "{.col}_w"
    )) |>
    mutate(across(ends_with("_w"), ~ log(.x + 1), .names = "ln_{.col}"))
}

# Historical Marovoay
panel_hist_maro <- hc |>
  filter(obs == "03", site_id %in% MARO_ALL, year %in% HIST_YEARS) |>
  mk_income() |>
  mutate(
    treated_unit = site_id %in% MARO_TREATED,
    post = year >= TREAT_YEAR,
    treatment = as.integer(treated_unit & post)
  ) |>
  distinct(j5, year, .keep_all = TRUE)

# Historical Alaotra
panel_hist_alaotra <- hc |>
  filter(obs == "21", site_id %in% ALAOTRA_ALL, year %in% HIST_YEARS) |>
  mk_income() |>
  mutate(treated_unit = FALSE, post = year >= TREAT_YEAR, treatment = 0L) |>
  distinct(j5, year, .keep_all = TRUE)

# 2025 Marovoay
res_deb_25 <- read_dta("data/ROS_MDG_microdata/2025/res_deb.dta") |>
  mutate(j0 = as.character(as.numeric(j0)), j5 = as.character(j5))

site_map_25_maro <- res_deb_25 |>
  filter(j0 == "3") |>
  mutate(
    obs = "03",
    site_id = case_when(
      str_starts(str_to_lower(j4), "bepako") ~ "031",
      str_starts(str_to_lower(j4), "madiromiongana") ~ "032",
      str_starts(str_to_lower(j4), "ampijoroa") ~ "033",
      str_starts(str_to_lower(j4), "maroala") ~ "034",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(site_id)) |>
  select(j5, obs, site_id)

rev_2025 <- compute_income_year(2025) |> mutate(j5 = as.character(j5))

panel_2025_maro <- site_map_25_maro |>
  left_join(rev_2025, by = "j5") |>
  mutate(year = 2025) |>
  mk_income() |>
  mutate(
    treated_unit = site_id %in% MARO_TREATED,
    post = TRUE,
    treatment = as.integer(treated_unit)
  )

# Alaotra 2025
site_map_25_al <- res_deb_25 |>
  filter(j0 == "21") |>
  mutate(
    obs = "21",
    site_id = case_when(
      j4 %in% c("Avaradrano", "Feramanga Atsimo", "Mangabe") ~ "211",
      j4 %in% c("Ambatoharanana", "Ambodivoara", "Analamiranga") ~ "212",
      j4 %in% c("Ambatomanga", "Ambohidrony") ~ "213",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(site_id)) |>
  select(j5, obs, site_id)

panel_2025_alaotra <- site_map_25_al |>
  left_join(rev_2025, by = "j5") |>
  mutate(year = 2025) |>
  mk_income() |>
  mutate(treated_unit = FALSE, post = TRUE, treatment = 0L)

# Long-run panels
panel_lr_full <- bind_rows(
  panel_hist_maro |> filter(year %in% PRE_YEARS),
  panel_hist_alaotra |> filter(year %in% PRE_YEARS),
  panel_2025_maro,
  panel_2025_alaotra
) |>
  mutate(
    post_lr = as.integer(year == 2025),
    did_lr = as.integer(treated_unit & year == 2025)
  )

panel_lr2 <- bind_rows(
  panel_hist_maro |> filter(year %in% PRE_YEARS),
  panel_2025_maro
) |>
  mutate(
    post_lr = as.integer(year == 2025),
    did_lr = as.integer(treated_unit & year == 2025)
  )

# Aggregate to site-year
agg_lr <- function(df) {
  df |>
    group_by(site_id, year, treated_unit, did_lr, post_lr) |>
    summarise(
      across(starts_with("ln_"), ~ mean(.x, na.rm = TRUE)),
      n_hh = n(),
      .groups = "drop"
    )
}

ps_lr_full <- agg_lr(panel_lr_full)
ps_lr_maro <- agg_lr(panel_lr2)

# Regressions
fit_maro <- feols(
  ln_revtot_w ~ did_lr | site_id + post_lr,
  data = ps_lr_maro,
  warn = FALSE,
  notes = FALSE
)
fit_maro_ag <- feols(
  ln_rev_agri_w ~ did_lr | site_id + post_lr,
  data = ps_lr_maro,
  warn = FALSE,
  notes = FALSE
)
fit_maro_nag <- feols(
  ln_rev_nonagri_w ~ did_lr | site_id + post_lr,
  data = ps_lr_maro,
  warn = FALSE,
  notes = FALSE
)

fit_full <- feols(
  ln_revtot_w ~ did_lr | site_id + post_lr,
  data = ps_lr_full,
  warn = FALSE,
  notes = FALSE
)
fit_full_ag <- feols(
  ln_rev_agri_w ~ did_lr | site_id + post_lr,
  data = ps_lr_full,
  warn = FALSE,
  notes = FALSE
)
fit_full_nag <- feols(
  ln_rev_nonagri_w ~ did_lr | site_id + post_lr,
  data = ps_lr_full,
  warn = FALSE,
  notes = FALSE
)

cat("=== LONG-RUN DiD RESULTS ===\n\n")
cat("--- Marovoay only (4 sites) ---\n")
cat(
  "Total income ATT:",
  round(coef(fit_maro)[["did_lr"]], 3),
  "  SE:",
  round(se(fit_maro)[["did_lr"]], 3),
  "\n"
)
cat(
  "Agri revenue ATT:",
  round(coef(fit_maro_ag)[["did_lr"]], 3),
  "  SE:",
  round(se(fit_maro_ag)[["did_lr"]], 3),
  "\n"
)
cat(
  "Non-agri ATT:    ",
  round(coef(fit_maro_nag)[["did_lr"]], 3),
  "  SE:",
  round(se(fit_maro_nag)[["did_lr"]], 3),
  "\n"
)

cat("\n--- Marovoay + Alaotra (7 sites) ---\n")
cat(
  "Total income ATT:",
  round(coef(fit_full)[["did_lr"]], 3),
  "  SE:",
  round(se(fit_full)[["did_lr"]], 3),
  "\n"
)
cat(
  "Agri revenue ATT:",
  round(coef(fit_full_ag)[["did_lr"]], 3),
  "  SE:",
  round(se(fit_full_ag)[["did_lr"]], 3),
  "\n"
)
cat(
  "Non-agri ATT:    ",
  round(coef(fit_full_nag)[["did_lr"]], 3),
  "  SE:",
  round(se(fit_full_nag)[["did_lr"]], 3),
  "\n"
)

cat(
  "\n--- Permutation exact p-values (long-run, Marovoay only, 6 allocations) ---\n"
)
combs2 <- combn(MARO_ALL, 2, simplify = FALSE)
perm_atts2 <- map_dbl(combs2, function(tr) {
  ps_lr_maro |>
    mutate(did_p = as.integer(site_id %in% tr & year == 2025)) |>
    {
      function(d) {
        tryCatch(
          coef(feols(
            ln_revtot_w ~ did_p | site_id + post_lr,
            data = d,
            warn = FALSE,
            notes = FALSE
          ))[["did_p"]],
          error = function(e) NA_real_
        )
      }
    }()
})
actual2 <- coef(fit_maro)[["did_lr"]]
p2 <- mean(abs(perm_atts2) >= abs(actual2), na.rm = TRUE)
cat(
  "p =",
  round(p2, 3),
  "(",
  sum(abs(perm_atts2) >= abs(actual2), na.rm = TRUE),
  "of",
  length(perm_atts2),
  "allocations )\n"
)

cat(
  "\n--- Permutation exact p-values (long-run extended, 21 allocations) ---\n"
)
combs7 <- combn(ALL_SITES, 2, simplify = FALSE)
perm_atts7 <- map_dbl(combs7, function(tr) {
  ps_lr_full |>
    mutate(did_p = as.integer(site_id %in% tr & year == 2025)) |>
    {
      function(d) {
        tryCatch(
          coef(feols(
            ln_revtot_w ~ did_p | site_id + post_lr,
            data = d,
            warn = FALSE,
            notes = FALSE
          ))[["did_p"]],
          error = function(e) NA_real_
        )
      }
    }()
})
actual7 <- coef(fit_full)[["did_lr"]]
p7 <- mean(abs(perm_atts7) >= abs(actual7), na.rm = TRUE)
cat(
  "p =",
  round(p7, 3),
  "(",
  sum(abs(perm_atts7) >= abs(actual7), na.rm = TRUE),
  "of",
  length(perm_atts7),
  "allocations )\n"
)

cat("\n=== Site counts ===\n")
cat(
  "Maro-only panel HH:",
  nrow(panel_lr2),
  "  Site-years:",
  nrow(ps_lr_maro),
  "\n"
)
cat(
  "Full panel HH:",
  nrow(panel_lr_full),
  "  Site-years:",
  nrow(ps_lr_full),
  "\n"
)
