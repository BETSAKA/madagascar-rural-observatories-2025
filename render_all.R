# render_all.R
# Renders all report variants (consolidated, Marovoay, Alaotra)
# and assembles download files so the consolidated HTML can link to them.
#
# Usage:  source("render_all.R")
# Or:     Rscript render_all.R
# ------------------------------------------------------------------

render_errors <- character(0)

run_render <- function(label, cmd) {
  cat(paste0("\n=== ", label, " ===\n"))
  rc <- system(cmd, intern = FALSE)
  if (rc != 0) {
    msg <- paste0("FAILED (exit code ", rc, "): ", cmd)
    cat("  *** ", msg, "\n")
    render_errors <<- c(render_errors, msg)
  }
  invisible(rc)
}

## Pre-cleanup: remove top-level stale "*_files" artifact dirs that can
## cause Quarto to refuse to remove them on Windows. Only remove
## directories at repo root that end with "_files".
clean_stale_files <- function() {
  dirs <- list.dirs(path = ".", recursive = FALSE, full.names = TRUE)
  stale <- dirs[grepl("_files$", dirs)]
  if (length(stale) == 0) {
    return(invisible(NULL))
  }
  for (d in stale) {
    # Do not touch anything inside docs/ or docs-*/
    if (grepl("^\\./docs", d)) {
      next
    }
    cat("Removing stale artifact directory:", d, "\n")
    tryCatch(
      {
        unlink(d, recursive = TRUE, force = TRUE)
      },
      warning = function(w) {
        cat("Warning removing", d, ":", conditionMessage(w), "\n")
      },
      error = function(e) {
        cat("Error removing", d, ":", conditionMessage(e), "\n")
      }
    )
  }
  invisible(NULL)
}

## Rescue output files that Quarto leaves at the project root instead of
## placing in the output-dir (happens when the cleanup step fails on Windows).
rescue_outputs <- function(output_dir, output_stem) {
  for (ext in c(".pdf", ".docx")) {
    root_file <- paste0(output_stem, ext)
    if (file.exists(root_file)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      dest <- file.path(output_dir, root_file)
      file.copy(root_file, dest, overwrite = TRUE)
      file.remove(root_file)
      cat("  Rescued:", root_file, "->", dest, "\n")
    }
  }
}

## Also remove stale .rmarkdown files left at root by knitr
clean_stale_rmarkdown <- function() {
  rmd <- list.files(".", pattern = "\\.rmarkdown$", full.names = TRUE)
  if (length(rmd) > 0) {
    file.remove(rmd)
    cat("Removed stale .rmarkdown files:", paste(rmd, collapse = ", "), "\n")
  }
}

# --- 1/3: Consolidated (HTML + PDF + DOCX) ---
# Render HTML first (fast, no xelatex cleanup issues).
# First render cleans output-dir; subsequent passes use --no-clean
# so they don't wipe previously rendered formats.
clean_stale_files()
run_render("1a/3  Consolidated — HTML", "quarto render --to html")
clean_stale_files()
clean_stale_rmarkdown()

# Render PDF separately so a cleanup failure doesn't block DOCX
run_render("1b/3  Consolidated — PDF", "quarto render --to pdf --no-clean")
clean_stale_files()
clean_stale_rmarkdown()
rescue_outputs("docs", "rapport-consolide")

# Render DOCX separately
run_render("1c/3  Consolidated — DOCX", "quarto render --to docx --no-clean")
clean_stale_files()
clean_stale_rmarkdown()
rescue_outputs("docs", "rapport-consolide")

# --- 2/3: Marovoay (PDF + DOCX) ---
clean_stale_files()
run_render("2a/3  Marovoay — PDF", "quarto render --profile marovoay --to pdf")
clean_stale_files()
clean_stale_rmarkdown()
rescue_outputs("docs-marovoay", "rapport-marovoay")

run_render(
  "2b/3  Marovoay — DOCX",
  "quarto render --profile marovoay --to docx --no-clean"
)
clean_stale_files()
clean_stale_rmarkdown()
rescue_outputs("docs-marovoay", "rapport-marovoay")

# --- 3/3: Alaotra (PDF + DOCX) ---
clean_stale_files()
run_render("3a/3  Alaotra — PDF", "quarto render --profile alaotra --to pdf")
clean_stale_files()
clean_stale_rmarkdown()
rescue_outputs("docs-alaotra", "rapport-alaotra")

run_render(
  "3b/3  Alaotra — DOCX",
  "quarto render --profile alaotra --to docx --no-clean"
)
clean_stale_files()
clean_stale_rmarkdown()
rescue_outputs("docs-alaotra", "rapport-alaotra")

# Copy per-observatory outputs into docs/downloads/ so the HTML links work
cat("\n=== Assembling download files ===\n")
dir.create("docs/downloads", showWarnings = FALSE, recursive = TRUE)

files_to_copy <- c(
  "docs-marovoay/rapport-marovoay.pdf",
  "docs-marovoay/rapport-marovoay.docx",
  "docs-alaotra/rapport-alaotra.pdf",
  "docs-alaotra/rapport-alaotra.docx"
)

for (f in files_to_copy) {
  if (file.exists(f)) {
    file.copy(f, "docs/downloads/", overwrite = TRUE)
    cat("  Copied:", f, "-> docs/downloads/\n")
  } else {
    cat("  WARNING: not found:", f, "\n")
  }
}

# --- Verify expected outputs ---
# Note: render_errors may include "FAILED" entries due to Quarto's
# safeRemoveDirSync bug on Windows, even though the outputs were produced.
# We check actual file existence rather than relying solely on exit codes.
cat("\n=== Checking expected outputs ===\n")
expected <- c(
  "docs/index.html",
  "docs/rapport-consolide.pdf",
  "docs/rapport-consolide.docx",
  "docs/downloads/rapport-marovoay.pdf",
  "docs/downloads/rapport-marovoay.docx",
  "docs/downloads/rapport-alaotra.pdf",
  "docs/downloads/rapport-alaotra.docx"
)

missing <- expected[!file.exists(expected)]

# Filter render_errors: if a render "failed" but all its outputs exist,
# it was likely just the cleanup bug
real_errors <- character(0)
for (e in render_errors) {
  # Consolidated renders (any format)
  if (
    grepl("quarto render", e) &&
      !grepl("--profile", e) &&
      all(file.exists(c(
        "docs/index.html",
        "docs/rapport-consolide.pdf",
        "docs/rapport-consolide.docx"
      )))
  ) {
    cat("  Note:", e, "(outputs exist — cleanup bug only)\n")
  } else if (
    grepl("--profile marovoay", e) &&
      all(file.exists(c(
        "docs-marovoay/rapport-marovoay.pdf",
        "docs-marovoay/rapport-marovoay.docx"
      )))
  ) {
    cat("  Note:", e, "(outputs exist — cleanup bug only)\n")
  } else if (
    grepl("--profile alaotra", e) &&
      all(file.exists(c(
        "docs-alaotra/rapport-alaotra.pdf",
        "docs-alaotra/rapport-alaotra.docx"
      )))
  ) {
    cat("  Note:", e, "(outputs exist — cleanup bug only)\n")
  } else {
    real_errors <- c(real_errors, e)
  }
}

if (length(missing) == 0 && length(real_errors) == 0) {
  cat("\nAll 7 outputs produced successfully:\n")
  for (f in expected) {
    cat("  OK:", f, "\n")
  }
} else {
  if (length(real_errors) > 0) {
    cat("\nRender errors:\n")
    for (e in real_errors) {
      cat("  ", e, "\n")
    }
  }
  if (length(missing) > 0) {
    cat("\nMissing outputs:\n")
    for (m in missing) {
      cat("  MISSING:", m, "\n")
    }
  }
  cat("\nSome outputs were not produced. Check the render logs above.\n")
}
