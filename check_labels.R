yml <- yaml::read_yaml("data/figure_sizes.yml")
yml_labels <- names(yml)

qmd_files <- list.files(".", pattern = "\\.qmd$", full.names = TRUE)
all_text <- unlist(lapply(qmd_files, readLines, warn = FALSE))

fig_h_calls <- regmatches(all_text, gregexpr('fig_h\\("([^"]+)"\\)', all_text))
fig_h_labels <- unique(unlist(lapply(fig_h_calls, function(x) gsub('fig_h\\("([^"]+)"\\)', "\\1", x))))

fig_w_calls <- regmatches(all_text, gregexpr('fig_w\\("([^"]+)"\\)', all_text))
fig_w_labels <- unique(unlist(lapply(fig_w_calls, function(x) gsub('fig_w\\("([^"]+)"\\)', "\\1", x))))

all_labels <- unique(c(fig_h_labels, fig_w_labels))

missing <- setdiff(all_labels, yml_labels)
if (length(missing) > 0) {
  cat("MISSING from YAML:\n")
  cat(paste(" ", missing, collapse = "\n"), "\n")
} else {
  cat("All", length(all_labels), "labels found in YAML\n")
}

extra <- setdiff(yml_labels, all_labels)
if (length(extra) > 0) {
  cat("In YAML but not in .qmd files:\n")
  cat(paste(" ", extra, collapse = "\n"), "\n")
}
