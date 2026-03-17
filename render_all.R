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
  if (length(stale) == 0) return(invisible(NULL))
  for (d in stale) {
    # Do not touch anything inside docs/ or docs-*/
    if (grepl("^\\./docs", d)) next
    cat("Removing stale artifact directory:", d, "\n")
    tryCatch({
      unlink(d, recursive = TRUE, force = TRUE)
    }, warning = function(w) {
      cat("Warning removing", d, ":", conditionMessage(w), "\n")
    }, error = function(e) {
      cat("Error removing", d, ":", conditionMessage(e), "\n")
    })
  }
  invisible(NULL)
}

clean_stale_files()

run_render("1/3  Rendering consolidated (HTML + PDF + DOCX)", "quarto render")
run_render(
  "2/3  Rendering Marovoay (PDF + DOCX)",
  "quarto render --profile marovoay"
)
run_render(
  "3/3  Rendering Alaotra (PDF + DOCX)",
  "quarto render --profile alaotra"
)

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

if (length(missing) == 0 && length(render_errors) == 0) {
  cat("\nAll 7 outputs produced successfully:\n")
  for (f in expected) {
    cat("  OK:", f, "\n")
  }
} else {
  if (length(render_errors) > 0) {
    cat("\nRender errors:\n")
    for (e in render_errors) {
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
