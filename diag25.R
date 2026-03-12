setwd(
  "C:/Users/fbede/Documents/Statistiques/madagascar-rural-observatories-2025"
)
library(tidyverse)
library(haven)
source("utils/calc_incomes_all_years.R")

res_deb_25 <- read_dta("data/ROS_MDG_microdata/2025/res_deb.dta") |>
  mutate(j0 = as.character(as.numeric(j0)), j5 = as.character(j5))

rev_2025 <- compute_income_year(2025) |> mutate(j5 = as.character(j5))
cat("rev_2025 rows:", nrow(rev_2025), "\n")

# Check j0 distribution
cat("\nj0 distribution:\n")
print(table(res_deb_25$j0))

# Check j4 for Marovoay
cat("\nj4 for j0=3 (Marovoay):\n")
maro_j4 <- res_deb_25 |>
  filter(j0 == "3") |>
  mutate(j4c = as.character(as_factor(j4))) |>
  count(j4c) |>
  arrange(j4c)
print(maro_j4, n = 30)

# Check join
cat(
  "\nJoin rows (all):",
  nrow(inner_join(res_deb_25, rev_2025, by = "j5")),
  "\n"
)
cat(
  "Join rows (j0=3):",
  nrow(inner_join(res_deb_25 |> filter(j0 == "3"), rev_2025, by = "j5")),
  "\n"
)
