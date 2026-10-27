# =============================================================================
# TRUSTED CODE BANK :: Deterministic provenance audit of a QC script
# -----------------------------------------------------------------------------
# Status   : APPROVED (v1.0.0, 2026-10-23)  -- green-shield code
# Owner    : Statistical Programming Standards
# Purpose  : Mirror the {commons} trust model for scripts drafted by the Posit
#            Assistant inside Positron. The *script* never labels itself; this
#            function labels each top-level expression from what the code does:
#
#   TRUSTED (green shield)  calls a function from the trusted code bank
#   CITED   (blue)          preceded by a `# cite:` comment whose target is
#                           verified: a file + line range that exists in this
#                           repo, or a URL on the approved-reference allowlist
#   REVIEW  (yellow)        novel code with no trusted backing -> deep manual review
#   SETUP   (grey)          library()/source()/read/write/run/print only
# =============================================================================

qc_approved_reference_domains <- c(
  "psiaims.github.io/CAMIS",
  "pharmaverse.github.io",
  "github.com/pharmaverse",
  "cdisc.org"
)

# Setup and I/O plumbing: loading, reading, writing, running and printing.
# These never change a result, so they are neither trusted nor flagged.
qc_setup_calls <- c(
  "library", "require", "requireNamespace", "source", "file.path", "dir.create",
  "set.seed", "options", "Sys.setenv", "invisible", "c", "here", "list.files",
  "readRDS", "saveRDS", "read.csv", "write.csv", "capture.output", "print", "message",
  "paste", "list", "attr", "interrogate", "export_report", "gtsave", "image_read"
)

#' Names of all functions defined in the trusted code bank
#' @noRd
qc_trusted_functions <- function(trusted_dir = "trusted") {
  files <- list.files(trusted_dir, pattern = "[.]R$", full.names = TRUE)
  fns <- unlist(lapply(files, \(f) {
    exprs <- parse(f, keep.source = FALSE)
    vapply(exprs, \(e) {
      if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") &&
          is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
        as.character(e[[2]])
      } else NA_character_
    }, character(1))
  }))
  sort(unique(stats::na.omit(fns)))
}

#' Verify a `# cite:` target deterministically
#' @noRd
qc_verify_citation <- function(cite) {
  cite <- trimws(cite)
  if (grepl("^https?://", cite)) {
    ok <- any(vapply(qc_approved_reference_domains, \(d) grepl(d, cite, fixed = TRUE), logical(1)))
    return(list(ok = ok, why = if (ok) "approved reference URL" else "URL not on approved-reference allowlist"))
  }
  path <- sub("#.*$", "", cite)
  if (!file.exists(path)) return(list(ok = FALSE, why = "cited file not found"))
  m <- regmatches(cite, regexec("#L(\\d+)(?:-L(\\d+))?$", cite, perl = TRUE))[[1]]
  if (length(m)) {
    n_lines <- length(readLines(path, warn = FALSE))
    end <- if (nzchar(m[3])) as.integer(m[3]) else as.integer(m[2])
    if (end > n_lines) return(list(ok = FALSE, why = "cited line range out of bounds"))
  }
  list(ok = TRUE, why = "cited file/lines verified")
}

#' Audit a script: one row per top-level expression with a provenance label
#' @noRd
qc_audit_provenance <- function(script, trusted_dir = "trusted") {
  trusted <- qc_trusted_functions(trusted_dir)
  src <- readLines(script, warn = FALSE)
  exprs <- parse(script, keep.source = TRUE)
  srcrefs <- attr(exprs, "srcref")
  pd <- utils::getParseData(parse(script, keep.source = TRUE))

  rows <- lapply(seq_along(exprs), \(i) {
    sr <- srcrefs[[i]]
    l1 <- sr[[1]]
    l2 <- sr[[3]]
    calls <- unique(pd$text[pd$token == "SYMBOL_FUNCTION_CALL" & pd$line1 >= l1 & pd$line2 <= l2])

    # Contiguous comment block directly above the expression
    j <- l1 - 1
    above <- character()
    while (j >= 1 && grepl("^\\s*#", src[j])) {
      above <- c(src[j], above)
      j <- j - 1
    }
    cite_line <- grep("#\\s*cite:", above, value = TRUE)
    cite <- if (length(cite_line)) sub(".*#\\s*cite:\\s*", "", cite_line[1]) else NA_character_

    trusted_used <- intersect(calls, trusted)
    label <- if (length(trusted_used)) {
      "TRUSTED"
    } else if (!is.na(cite) && qc_verify_citation(cite)$ok) {
      "CITED"
    } else if (all(calls %in% qc_setup_calls)) {
      "SETUP"
    } else {
      "REVIEW"
    }
    detail <- switch(label,
      TRUSTED = paste("Trusted:", paste0(trusted_used, "()", collapse = ", ")),
      CITED = paste("Cited:", cite),
      SETUP = "Setup / I/O only",
      REVIEW = if (!is.na(cite)) paste("Citation rejected:", qc_verify_citation(cite)$why)
               else "AI-drafted code with no trusted backing"
    )
    tibble::tibble(
      lines = if (l1 == l2) as.character(l1) else paste0(l1, "-", l2),
      n_lines = l2 - l1 + 1,
      code = trimws(src[l1]),
      label = label,
      detail = detail
    )
  })
  out <- dplyr::bind_rows(rows)
  attr(out, "script") <- script
  out
}

#' Render a provenance audit as a gt table with green / blue / yellow markers
#' @noRd
qc_gt_provenance <- function(audit) {
  pill <- function(bg, fg, icon, text) {
    sprintf(
      "<span style='display:inline-block;white-space:nowrap;background:%s;color:%s;padding:2px 10px;border-radius:10px;font-weight:600;font-size:0.85em;'>%s&nbsp;%s</span>",
      bg, fg, if (is.null(icon)) "" else as.character(fontawesome::fa(icon, fill = fg, height = "0.9em")), text
    )
  }
  badge <- c(
    TRUSTED = pill("#198754", "white", "shield-halved", "TRUSTED"),
    CITED   = pill("#0d6efd", "white", "link", "CITED"),
    REVIEW  = pill("#ffc107", "black", "triangle-exclamation", "REVIEW"),
    SETUP   = pill("#e9ecef", "#495057", NULL, "setup / I/O")
  )
  lines_by <- tapply(audit$n_lines, audit$label, sum)
  share <- function(l) if (is.na(lines_by[l])) 0 else round(100 * lines_by[l] / sum(audit$n_lines[audit$label != "SETUP"]))
  subtitle <- sprintf("Non-setup lines: %d%% trusted  |  %d%% cited  |  %d%% needs review",
                      share("TRUSTED"), share("CITED"), share("REVIEW"))
  audit |>
    dplyr::mutate(marker = unname(badge[label])) |>
    dplyr::select(marker, lines, code, detail) |>
    gt::gt() |>
    gt::fmt_passthrough(columns = marker, escape = FALSE) |>
    gt::cols_width(marker ~ gt::px(130), lines ~ gt::px(70)) |>
    gt::tab_options(table.font.size = gt::px(13), data_row.padding = gt::px(3)) |>
    gt::tab_header(
      title = gt::md(sprintf("Provenance audit: `%s`", basename(attr(audit, "script")))),
      subtitle = subtitle
    ) |>
    gt::cols_label(marker = "", lines = "Lines", code = "Expression (first line)", detail = "Why") |>
    gt::tab_style(gt::cell_text(font = "monospace", size = "small"), gt::cells_body(columns = code)) |>
    gt::tab_source_note("Labels are assigned deterministically from the code's call graph and verified citations, never by the AI that wrote the script (same principle as {commons}).")
}
