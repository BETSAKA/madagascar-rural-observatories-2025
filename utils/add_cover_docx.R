#!/usr/bin/env Rscript
# utils/add_cover_docx.R
# Quarto post-render script: inserts cover image after the title block
# (title, subtitle, authors, date) and before the TOC in DOCX output.
#
# Pandoc/Quarto cannot insert content before the TOC in DOCX format, so we
# post-process: unzip the DOCX, splice cover-image XML into document.xml,
# add the media file, and re-zip.
#
# Usage (called automatically by Quarto via post-render):
#   Rscript utils/add_cover_docx.R <output-file1> [<output-file2> ...]
# -----------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Main entry point (runs when called via Rscript, not when source()'d)
# ---------------------------------------------------------------------------
.add_cover_main <- function() {
  # Quarto passes output files via env var, not command-line args
  out_files_raw <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES", "")
  all_files <- strsplit(out_files_raw, "\n")[[1]]
  docx_files <- all_files[grepl("\\.docx$", all_files, ignore.case = TRUE)]

  if (length(docx_files) == 0) {
    message("add_cover_docx: No DOCX files in output \u2014 skipping.")
    return(invisible())
  }

  cover_img <- "images/cover.jpg"
  if (!file.exists(cover_img)) {
    message("add_cover_docx: Cover image not found: ", cover_img)
    return(invisible())
  }

  for (f in docx_files) {
    message("add_cover_docx: Processing ", f)
    tryCatch(
      add_cover_to_docx(f, cover_img),
      error = function(e) message("  ERROR: ", conditionMessage(e))
    )
  }
}

# ---------------------------------------------------------------------------
add_cover_to_docx <- function(docx_path, cover_img) {
  if (!file.exists(docx_path)) {
    message("  Not found: ", docx_path, " — skipping.")
    return(invisible(FALSE))
  }

  saved_wd <- getwd()
  on.exit(setwd(saved_wd), add = TRUE)

  # --- Image dimensions (EMU: 1 inch = 914 400) ---------------------------
  img_dim <- dim(jpeg::readJPEG(cover_img))
  target_cx <- 5943000 # ~6.5 in

  target_cy <- round(target_cx * img_dim[1] / img_dim[2])

  # --- Unzip ---------------------------------------------------------------
  tmp <- tempfile("docx_cover_")
  dir.create(tmp)
  unzip(docx_path, exdir = tmp)

  # --- Check idempotency (skip if cover already inserted) -----------------
  media_dir <- file.path(tmp, "word", "media")
  if (file.exists(file.path(media_dir, "cover_page.jpg"))) {
    message("  Cover already present \u2014 skipping.")
    unlink(tmp, recursive = TRUE)
    return(invisible(TRUE))
  }

  # --- Copy cover image to word/media/ -------------------------------------
  if (!dir.exists(media_dir)) {
    dir.create(media_dir, recursive = TRUE)
  }
  file.copy(
    normalizePath(cover_img),
    file.path(media_dir, "cover_page.jpg"),
    overwrite = TRUE
  )

  # --- Add image relationship ----------------------------------------------
  rels_path <- file.path(tmp, "word", "_rels", "document.xml.rels")
  rels_xml <- xml2::read_xml(rels_path)
  ids <- xml2::xml_attr(xml2::xml_children(rels_xml), "Id")
  max_id <- max(as.integer(gsub("rId", "", ids)))
  new_rid <- paste0("rId", max_id + 1L)

  xml2::xml_add_child(
    rels_xml,
    "Relationship",
    Id = new_rid,
    Type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image",
    Target = "media/cover_page.jpg"
  )
  xml2::write_xml(rels_xml, rels_path)

  # --- Build cover-image paragraph XML -------------------------------------
  cover_xml <- sprintf(
    paste0(
      '<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"',
      ' xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"',
      ' xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"',
      ' xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"',
      ' xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">',
      '<w:pPr><w:jc w:val="center"/><w:spacing w:before="1440"/></w:pPr>',
      '<w:r><w:drawing>',
      '<wp:inline distT="0" distB="0" distL="0" distR="0">',
      '<wp:extent cx="%d" cy="%d"/>',
      '<wp:effectExtent b="0" l="0" r="0" t="0"/>',
      '<wp:docPr id="9999" name="CoverImage" descr="Photo de couverture"/>',
      '<a:graphic>',
      '<a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">',
      '<pic:pic>',
      '<pic:nvPicPr>',
      '<pic:cNvPr id="9998" name="CoverImage"/>',
      '<pic:cNvPicPr><a:picLocks noChangeAspect="1"/></pic:cNvPicPr>',
      '</pic:nvPicPr>',
      '<pic:blipFill>',
      '<a:blip r:embed="%s"/>',
      '<a:stretch><a:fillRect/></a:stretch>',
      '</pic:blipFill>',
      '<pic:spPr>',
      '<a:xfrm><a:off x="0" y="0"/><a:ext cx="%d" cy="%d"/></a:xfrm>',
      '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom>',
      '</pic:spPr>',
      '</pic:pic>',
      '</a:graphicData></a:graphic>',
      '</wp:inline>',
      '</w:drawing></w:r></w:p>'
    ),
    target_cx,
    target_cy,
    new_rid,
    target_cx,
    target_cy
  )

  credit_text <- paste0(
    "Rizi\u00e8re dans la r\u00e9gion d\u2019Alaotra Mangoro. ",
    "Photo\u00a0: Leja Mitarika / NJProduction, ",
    "CC\u00a0BY-SA\u00a04.0, via Wikimedia Commons."
  )

  credit_xml <- paste0(
    '<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:pPr><w:jc w:val="center"/><w:spacing w:before="120"/></w:pPr>',
    '<w:r>',
    '<w:rPr><w:i/><w:sz w:val="18"/><w:szCs w:val="18"/></w:rPr>',
    '<w:t xml:space="preserve">',
    credit_text,
    '</w:t>',
    '</w:r></w:p>'
  )

  break_xml <- paste0(
    '<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:r><w:br w:type="page"/></w:r></w:p>'
  )

  # --- Insert into document.xml body (after title block, before TOC) ------
  doc_path <- file.path(tmp, "word", "document.xml")
  doc_xml <- xml2::read_xml(doc_path)
  ns <- xml2::xml_ns(doc_xml)
  body <- xml2::xml_find_first(doc_xml, "//w:body", ns)

  # Walk body children to find the last title-block paragraph.

  # Style names depend on locale — English templates use "Title" etc.,

  # French reference-doc uses "Titre", "Sous-titre", etc.
  title_styles <- c(
    "Title",
    "Subtitle",
    "Author",
    "Date", # English
    "Titre",
    "Sous-titre" # French
  )
  body_children <- xml2::xml_children(body)
  last_title_idx <- 0L
  for (i in seq_along(body_children)) {
    style_node <- xml2::xml_find_first(
      body_children[[i]],
      ".//w:pPr/w:pStyle",
      ns
    )
    if (inherits(style_node, "xml_node")) {
      sval <- xml2::xml_attr(style_node, "val")
      if (sval %in% title_styles) {
        last_title_idx <- i
        next
      }
    }
    # Skip empty/unstyled paragraphs only while we haven't found a
    # title-block element yet (template padding at top of page)
    if (
      last_title_idx == 0L && nchar(xml2::xml_text(body_children[[i]])) == 0L
    ) {
      next
    }
    # Non-title, non-empty paragraph after the title block → stop
    if (last_title_idx > 0L) break
  }

  if (last_title_idx > 0L) {
    anchor <- body_children[[last_title_idx]]
    # Insert in reverse order ("after" inserts immediately after anchor,
    # so last inserted ends up closest to anchor)
    # Final order: ... Title block | cover | credit | page-break | TOC ...
    xml2::xml_add_sibling(anchor, xml2::read_xml(break_xml), .where = "after")
    xml2::xml_add_sibling(anchor, xml2::read_xml(credit_xml), .where = "after")
    xml2::xml_add_sibling(anchor, xml2::read_xml(cover_xml), .where = "after")
    # Final order: ... Title block | cover | page-break | TOC ...
  } else {
    # Fallback: insert before first child if no title block found
    first_child <- xml2::xml_child(body, 1)
    xml2::xml_add_sibling(
      first_child,
      xml2::read_xml(break_xml),
      .where = "before"
    )
    xml2::xml_add_sibling(
      first_child,
      xml2::read_xml(credit_xml),
      .where = "before"
    )
    xml2::xml_add_sibling(
      first_child,
      xml2::read_xml(cover_xml),
      .where = "before"
    )
  }

  xml2::write_xml(doc_xml, doc_path)

  # --- Re-zip (preserving directory structure) -----------------------------
  docx_abs <- normalizePath(docx_path, mustWork = TRUE)
  file.remove(docx_abs)
  setwd(tmp)
  all_files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  utils::zip(docx_abs, files = all_files)
  setwd(saved_wd)

  unlink(tmp, recursive = TRUE)
  message("  Cover added to ", docx_path)
  invisible(TRUE)
}

# Run main block only when invoked as a script (not when source()'d)
if (sys.nframe() == 0L) {
  .add_cover_main()
}
