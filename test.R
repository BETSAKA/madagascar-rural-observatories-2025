library(tidyverse)
library(haven)
setwd(
  "C:/Users/fbede/Documents/Statistiques/madagascar-rural-observatories-2025"
)

# The 2025 income must be reconstructed from raw DTA files like the historical pipeline
# Let's check what res_respg / res_respg0 contain (head of household data)
res_respg0 <- read_dta("data/res_respg0.dta")
cat(
  "res_respg0 years:",
  paste(sort(unique(res_respg0$year)), collapse = ", "),
  "\n"
)
cat("res_respg0 cols:", paste(names(res_respg0), collapse = ", "), "\n")
cat(
  "j5 prefix:",
  paste(
    sort(unique(substr(as.character(res_respg0$j5), 1, 2))),
    collapse = ", "
  ),
  "\n"
)

# Check res_pp0 and res_mp0 which are likely income components
res_pp0 <- read_dta("data/res_pp0.dta")
cat(
  "\nres_pp0 years:",
  paste(sort(unique(res_pp0$year)), collapse = ", "),
  "\n"
)
cat("res_pp0 cols:", paste(names(res_pp0), collapse = ", "), "\n")

res_mp0 <- read_dta("data/res_mp0.dta")
cat(
  "\nres_mp0 years:",
  paste(sort(unique(res_mp0$year)), collapse = ", "),
  "\n"
)
cat("res_mp0 cols:", paste(names(res_mp0), collapse = ", "), "\n")

# Check the main income pipeline file (inspect.R or something)
agro_files <- list.files(
  "data",
  pattern = "inspect|income|rev",
  ignore.case = TRUE
)
cat("Income-related files:", paste(agro_files, collapse = ", "), "\n")

# Check the utils folder for income construction
util_files <- list.files("utils", full.names = FALSE)
cat("Utils files:", paste(util_files, collapse = ", "), "\n")
