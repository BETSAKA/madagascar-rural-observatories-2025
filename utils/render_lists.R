# utils/render_lists.R
# Helpers to generate book-level lists of figures and tables from Quarto sources.

read_book_chapters <- function(config = "_quarto-consolidated.yml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required.")
  }

  config_path <- normalizePath(config, winslash = "/", mustWork = FALSE)
  if (!file.exists(config_path)) {
    root <- tryCatch(
      rprojroot::find_root(rprojroot::has_file("_quarto.yaml")),
      error = function(e) getwd()
    )
    config_path <- file.path(root, config)
  }
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config)
  }

  cfg <- yaml::read_yaml(config_path)
  chapters <- cfg$book$chapters

  flatten_chapters <- function(x) {
    out <- character(0)
    for (item in x) {
      if (is.character(item)) {
        out <- c(out, item)
      } else if (is.list(item)) {
        if (
          !is.null(item$part) &&
            is.character(item$part) &&
            grepl("\\.qmd$", item$part)
        ) {
          out <- c(out, item$part)
        }
        if (!is.null(item$chapters)) {
          out <- c(out, flatten_chapters(item$chapters))
        }
      }
    }
    out
  }

  flatten_chapters(chapters)
}

extract_crossrefs_from_qmd <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")

  is_commented <- function(x) {
    grepl("^\\s*#\\s*#\\|", x) || grepl("^\\s*<!--", x)
  }

  get_value <- function(line, key) {
    sub(
      paste0("^\\s*#\\|\\s*", key, ":\\s*\"?(.*?)\"?\\s*$"),
      "\\1",
      line,
      perl = TRUE
    )
  }

  res <- list()
  i <- 1L
  while (i <= length(lines)) {
    line <- lines[[i]]
    if (
      grepl("^\\s*#\\|\\s*label:\\s*(fig|tbl)-", line) && !is_commented(line)
    ) {
      label <- get_value(line, "label")
      kind <- if (startsWith(label, "fig-")) "figure" else "table"
      cap_key <- if (kind == "figure") "fig-cap" else "tbl-cap"
      caption <- NA_character_

      j <- i - 1L
      while (j >= 1L) {
        prev <- lines[[j]]
        if (grepl("^\\s*#\\|", prev) && !is_commented(prev)) {
          key <- sub("^\\s*#\\|\\s*([a-zA-Z-]+):.*$", "\\1", prev)
          if (identical(key, cap_key)) {
            caption <- get_value(prev, key)
            break
          }
          j <- j - 1L
          next
        }
        if (grepl("^\\s*```", prev) || grepl("^\\s*$", prev)) {
          j <- j - 1L
          next
        }
        break
      }

      if (is.na(caption) || !nzchar(caption)) {
        j <- i + 1L
        while (j <= length(lines)) {
          nxt <- lines[[j]]
          if (grepl("^\\s*#\\|", nxt) && !is_commented(nxt)) {
            key <- sub("^\\s*#\\|\\s*([a-zA-Z-]+):.*$", "\\1", nxt)
            if (identical(key, cap_key)) {
              caption <- get_value(nxt, key)
              break
            }
            j <- j + 1L
            next
          }
          if (grepl("^\\s*```", nxt) || grepl("^\\s*$", nxt)) {
            j <- j + 1L
            next
          }
          break
        }
      }

      if (!is.na(caption) && nzchar(caption)) {
        caption <- gsub("<br>.*$", "", caption)
        caption <- gsub("\\s+", " ", caption)
        res[[length(res) + 1L]] <- data.frame(
          file = file,
          href = paste0(sub("\\.qmd$", ".html", basename(file)), "#", label),
          label = label,
          kind = kind,
          caption = trimws(caption),
          stringsAsFactors = FALSE
        )
      }
    }
    i <- i + 1L
  }

  if (length(res) == 0L) {
    data.frame(
      file = character(),
      href = character(),
      label = character(),
      kind = character(),
      caption = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, res)
  }
}

collect_book_crossrefs <- function(config = "_quarto-consolidated.yml") {
  config_path <- normalizePath(config, winslash = "/", mustWork = FALSE)
  if (!file.exists(config_path)) {
    root <- tryCatch(
      rprojroot::find_root(rprojroot::has_file("_quarto.yaml")),
      error = function(e) getwd()
    )
    config_path <- file.path(root, config)
  }
  root_dir <- dirname(config_path)

  chapters <- read_book_chapters(config)
  chapter_paths <- file.path(root_dir, chapters)
  chapter_paths <- chapter_paths[file.exists(chapter_paths)]
  refs <- lapply(chapter_paths, extract_crossrefs_from_qmd)
  refs <- do.call(rbind, refs)
  if (is.null(refs) || nrow(refs) == 0L) {
    return(data.frame(
      file = character(),
      label = character(),
      kind = character(),
      caption = character(),
      stringsAsFactors = FALSE
    ))
  }
  refs$row_id <- seq_len(nrow(refs))
  refs
}

render_book_lists_markdown <- function(config = "_quarto-consolidated.yml") {
  refs <- collect_book_crossrefs(config)
  figs <- refs[refs$kind == "figure", , drop = FALSE]
  tbls <- refs[refs$kind == "table", , drop = FALSE]

  fmt_items <- function(df, prefix) {
    if (nrow(df) == 0L) {
      return("Aucun élément.")
    }
    paste0(
      "1. <span id=\"",
      prefix,
      "-",
      seq_len(nrow(df)),
      "\"></span>",
      "[",
      df$caption,
      "](",
      df$href,
      ")",
      collapse = "\n"
    )
  }

  paste(
    "# Liste des figures {.unnumbered}",
    "",
    fmt_items(figs, "lof-fig"),
    "",
    "# Liste des tableaux {.unnumbered}",
    "",
    fmt_items(tbls, "lot-tbl"),
    sep = "\n"
  )
}
