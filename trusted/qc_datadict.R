# =============================================================================
# TRUSTED CODE BANK :: define.xml -> data-dict.yaml (polyglot data contract)
# -----------------------------------------------------------------------------
# Status   : APPROVED (v1.0.0, 2026-10-23)  -- green-shield code
# Owner    : Statistical Programming Standards
# Purpose  : Translate the study define.xml (via {metacore}) into a
#            data-dict.yaml (https://data-dict.tidyverse.org) so the same
#            spec can be validated by the language-agnostic data-dict CLI,
#            rendered as HTML documentation, translated into R / Python / SQL,
#            and read by {commons} agents (commons uses the data-dict format).
#
# Mapping  : define.xml                   -> data-dict
#            text + CodeListRef           -> enum, values = codelist codes
#            integer/float + CodeListRef  -> number + assert X IN (codes)
#            DisplayFormat DATE9.         -> date (range = observed)
#            other text / numeric         -> string / number (examples = observed)
#            key variables                -> primary_key / required
#            MethodDef derivations        -> table-level assert (see qc_datadict_rules)
# Note     : `examples` and `range` are descriptive in data-dict (never
#            validated), so drawing them from the data does not weaken QC.
#            Every enforceable rule comes from the spec.
# =============================================================================

#' Derivation rules transcribed from define.xml MethodDefs (data-dict expressions)
#' Each carries the define.xml line range it was transcribed from.
#' TRTDURD (TRTEDT - TRTSDT + 1) is NOT expressible: data-dict does not
#' support date - date. It remains a pointblank rule in qc_adsl_spec.R.
#' @noRd
qc_datadict_rules <- list(
  ADSL = list(
    list(assert = "ARM <> 'Screen Failure'",
         description = "Structure: Screen Failures are excluded (define.xml L182)"),
    list(assert = "AGEGR1 = CASE WHEN AGE < 65 THEN '<65' WHEN AGE <= 80 THEN '65-80' ELSE '>80' END",
         description = "AGEGR1 derived from AGE, MT.ADSL.AGEGR1N (define.xml L4926-L4931)"),
    list(assert = "SAFFL = CASE WHEN ITTFL = 'Y' AND TRTSDT IS NOT NULL THEN 'Y' ELSE 'N' END",
         description = "SAFFL = Y if ITTFL = Y and TRTSDT not missing (define.xml L5108)")
  )
)

#' Build one data-dict table entry from a metacore dataset spec + data
#' @noRd
qc_datadict_table <- function(data, spec, table, parquet, origin = NULL,
                              description = NULL, rules = qc_datadict_rules[["ADSL"]]) {
  st <- qc_spec_table(spec)
  st <- st[st$variable %in% names(data), ]
  keys <- st$variable[!is.na(st$key_seq)]

  observed <- function(x, n = 5) {
    u <- sort(unique(x[!is.na(x)]))
    if (!length(u)) return(NULL)
    u[unique(round(seq(1, length(u), length.out = min(n, length(u)))))]
  }

  columns <- lapply(seq_len(nrow(st)), \(i) {
    v <- st$variable[i]
    x <- data[[v]]
    codes <- st$codes[[i]]
    is_date <- !is.na(st$format[i]) && grepl("^DATE", st$format[i])
    col <- list(name = v, label = st$label[i])

    if (is_date) {
      col$type <- "date"
      rng <- range(x, na.rm = TRUE)
      col$range <- as.character(rng)
    } else if (st$type[i] == "text" && !is.null(codes)) {
      col$type <- "enum"
      col$values <- as.list(as.character(codes))
      col$description <- sprintf("Controlled terminology %s (define.xml).", st$code_id[i])
    } else if (st$type[i] == "text") {
      col$type <- "string"
      ex <- observed(as.character(x))
      col$examples <- as.list(if (is.null(ex)) "(none observed)" else ex)
    } else {
      col$type <- "number"
      ex <- if (!is.null(codes)) as.numeric(codes) else observed(as.numeric(x))
      col$examples <- as.list(if (is.null(ex)) 0 else ex)
      if (!is.null(codes)) {
        col$description <- sprintf("Controlled terminology %s (define.xml).", st$code_id[i])
      }
    }

    cons <- list()
    if (v %in% keys) cons <- c(cons, list("primary_key"))
    if (v == "STUDYID") cons <- c(cons, list("required"))
    if (st$type[i] == "text" && !is.na(st$length[i]) && !is_date) {
      cons <- c(cons, list(list(
        assert = sprintf("LENGTH(%s) <= %d", v, st$length[i]),
        description = sprintf("SAS length %d (define.xml)", st$length[i])
      )))
    }
    if (st$type[i] != "text" && !is.null(codes) && !is_date) {
      cons <- c(cons, list(list(
        assert = sprintf("%s IN (%s)", v, paste(as.numeric(codes), collapse = ", ")),
        description = sprintf("Codelist %s (define.xml)", st$code_id[i])
      )))
    }
    if (length(cons)) col$constraints <- cons
    col
  })

  out <- list(
    name = table,
    label = spec$ds_spec$label[1],
    description = description %||% spec$ds_spec$structure[1],
    source = list(parquet = parquet)
  )
  if (!is.null(origin)) out$origin <- origin
  out$columns <- columns
  if (length(rules)) out$constraints <- rules
  out
}

#' Write a data-dict.yaml for one or more datasets built from the define.xml
#' @param tables list of qc_datadict_table() results
#' @noRd
qc_write_datadict <- function(tables, path, name = "cdiscpilot01_adam",
                              description = "CDISC pilot 3 ADaM data contract generated from the study define.xml.") {
  dict <- list(
    `$version` = "0.1.0",
    `$learn_more` = "https://data-dict.tidyverse.org/",
    name = name,
    description = description,
    details = paste(
      "Generated by trusted/qc_datadict.R from metadata/ADaM_define_CDISC_pilot3.xml.",
      "Do not hand-edit: regenerate from the define.xml."
    ),
    tables = tables
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(yaml::as.yaml(dict, indent.mapping.sequence = TRUE, line.sep = "\n"), path)
  invisible(path)
}

#' Write a data frame to Parquet (attributes stripped, factors -> character)
#' Uses {nanoparquet}: it writes Date columns with the Parquet *logical* DATE
#' type, which the data-dict reader requires. (DuckDB's writer only sets the
#' legacy converted type, so data-dict 0.0.3 reads those dates as numbers.)
#' @noRd
qc_write_parquet <- function(data, path) {
  data <- as.data.frame(data)
  data[] <- lapply(data, \(x) {
    if (is.factor(x)) x <- as.character(x)
    attributes(x) <- attributes(x)[intersect(names(attributes(x)), "class")]
    x
  })
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  nanoparquet::write_parquet(data, path)
  invisible(path)
}

#' Run a data-dict CLI subcommand and capture its output
#' @noRd
qc_datadict_cli <- function(...) {
  bin <- Sys.which("data-dict")
  if (!nzchar(bin)) bin <- path.expand("~/.local/bin/data-dict")
  if (!file.exists(bin)) stop("data-dict CLI not found. Install with: uv tool install data-dict-yaml")
  out <- suppressWarnings(system2(bin, shQuote(c(...)), stdout = TRUE, stderr = TRUE))
  structure(out, status = attr(out, "status") %||% 0L)
}
