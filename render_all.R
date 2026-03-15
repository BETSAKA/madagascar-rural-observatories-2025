# render_all.R
# Renders all report variants (consolidated, Marovoay, Alaotra)
# and assembles download files so the consolidated HTML can link to them.
#
# Usage:  source("render_all.R")
# Or:     Rscript render_all.R
# ------------------------------------------------------------------

cat("=== 1/3  Rendering consolidated (HTML + PDF + DOCX) ===\n")
system("quarto render", intern = FALSE)

cat("\n=== 2/3  Rendering Marovoay (PDF + DOCX) ===\n")
system("quarto render --profile marovoay", intern = FALSE)

cat("\n=== 3/3  Rendering Alaotra (PDF + DOCX) ===\n")
system("quarto render --profile alaotra", intern = FALSE)

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

cat("\nDone. All reports available in docs/\n")
cat("  HTML:  docs/index.html\n")
cat("  PDF:   docs/rapport-consolide.pdf\n")
cat("  DOCX:  docs/rapport-consolide.docx\n")
cat("  Marovoay PDF/DOCX: docs/downloads/rapport-marovoay.*\n")
cat("  Alaotra PDF/DOCX:  docs/downloads/rapport-alaotra.*\n")
