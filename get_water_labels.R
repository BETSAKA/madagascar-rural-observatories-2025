library(tidyverse)
# Try to find recent data if available
# This usually loads environments or databases
# We just need to know the labels of h4
# Let us check the data/ folder for RDS files
f <- list.files("data", pattern = "\.rds$", full.names = TRUE, recursive = TRUE)
if (length(f) > 0) {
  # Try to read one and see h4
  d <- readRDS(f[1])
  if ("h4" %in% names(d)) {
    print(sort(unique(as.character(haven::as_factor(d$h4)))))
  } else {
    cat("h4 not in", f[1], "\n")
  }
}
