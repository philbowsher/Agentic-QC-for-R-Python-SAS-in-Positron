# =============================================================================
# TRUSTED CODE BANK :: Spec-driven dataset QC (define.xml -> metacore -> pointblank)
# -----------------------------------------------------------------------------
# Status   : APPROVED (v1.0.0, 2026-10-23)  -- green-shield code
# Owner    : Statistical Programming Standards
# Lineage  : Spec handling follows pharmaverse/examples adam/adsl.R
#            (metacore + metatools check_ct_data / check_variables), re-expressed
#            as a pointblank agent so every rule becomes an auditable step.
# Note     : Functions tagged @measure are also exposed to the commons agent
#            (commons_app/). Helpers without @measure are internal.
# =============================================================================

#' Locate the study define.xml
#'
#' Project copy first (metadata/), falling back to the CDISC pilot 3 define
#' shipped with {metacore}.
#' @noRd
qc_define_path <- function() {
  local <- file.path("metadata", "ADaM_define_CDISC_pilot3.xml")
  if (file.exists(local)) {
    return(local)
  }
  metacore::metacore_example("ADaM_define_CDISC_pilot3.xml")
}

#' Read the define.xml into a dataset-level metacore object
#' @noRd
qc_read_spec <- function(dataset = "ADSL", path = qc_define_path()) {
  mc <- suppressWarnings(suppressMessages(
    metacore::define_to_metacore(path, verbose = "silent")
  ))
  suppressWarnings(suppressMessages(
    metacore::select_dataset(mc, dataset, verbose = "silent")
  ))
}

#' Variable-level spec as a flat table: type, length, format, codelist
#' @noRd
qc_spec_table <- function(mc) {
  ct <- mc$codelist |>
    dplyr::filter(type %in% c("code_decode", "permitted_val")) |>
    dplyr::mutate(codes = purrr::map(codes, \(x) unique(as.character(x$code)))) |>
    dplyr::select(code_id, codes)

  mc$ds_vars |>
    dplyr::select(variable, order, key_seq) |>
    dplyr::left_join(
      mc$var_spec |> dplyr::select(variable, label, type, length, format),
      by = "variable"
    ) |>
    dplyr::left_join(
      mc$value_spec |>
        dplyr::distinct(variable, code_id, derivation_id),
      by = "variable"
    ) |>
    dplyr::left_join(ct, by = "code_id") |>
    dplyr::arrange(order)
}

#' Build a pointblank agent from the define.xml for one dataset
#'
#' Every rule is derived from the spec, not hand-written:
#'   * keys            -> col_vals_not_null() + rows_distinct()
#'   * data type       -> col_is_character() / col_is_numeric() / col_is_date()
#'   * SAS length      -> col_vals_expr(nchar(x) <= length)
#'   * controlled term -> col_vals_in_set(set = codelist codes)
#' Variables in the spec but absent from the data are reported via
#' attr(agent, "spec_vars_missing") rather than failing the agent, so a
#' partially-built dataset can still be QC'd.
#' @noRd
qc_spec_agent <- function(data,
                          dataset = "ADSL",
                          spec = qc_read_spec(dataset),
                          tbl_name = tolower(dataset),
                          label = paste(dataset, "vs define.xml (CDISC pilot 3)"),
                          warn_at = 1) {
  st <- qc_spec_table(spec)
  in_data <- st$variable[st$variable %in% names(data)]
  st_in <- st[st$variable %in% in_data, ]

  agent <- pointblank::create_agent(
    tbl = data,
    tbl_name = tbl_name,
    label = label,
    actions = pointblank::action_levels(warn_at = warn_at)
  )

  # Keys: required, non-missing, unique
  keys <- st$variable[!is.na(st$key_seq)]
  keys <- union(intersect(c("STUDYID", "USUBJID"), in_data), intersect(keys, in_data))
  for (v in keys) {
    agent <- pointblank::col_vals_not_null(
      agent, columns = dplyr::all_of(v),
      brief = sprintf("Key %s must be populated", v)
    )
  }
  agent <- pointblank::rows_distinct(
    agent, columns = dplyr::all_of(keys),
    brief = sprintf("One record per %s (define.xml structure)", paste(keys, collapse = "/"))
  )

  # Data types
  for (i in seq_len(nrow(st_in))) {
    v <- st_in$variable[i]
    is_date <- !is.na(st_in$format[i]) && grepl("^DATE", st_in$format[i])
    agent <- if (is_date) {
      pointblank::col_is_date(agent, columns = dplyr::all_of(v),
                              brief = sprintf("%s is a date (%s)", v, st_in$format[i]))
    } else if (st_in$type[i] == "text") {
      pointblank::col_is_character(agent, columns = dplyr::all_of(v),
                                   brief = sprintf("%s is character (define type: text)", v))
    } else {
      pointblank::col_is_numeric(agent, columns = dplyr::all_of(v),
                                 brief = sprintf("%s is numeric (define type: %s)", v, st_in$type[i]))
    }
  }

  # SAS lengths for character variables (Pinnacle 21-style check)
  for (i in which(st_in$type == "text" & !is.na(st_in$length))) {
    v <- st_in$variable[i]
    len <- st_in$length[i]
    if (is.character(data[[v]])) {
      agent <- pointblank::col_vals_expr(
        agent,
        expr = rlang::expr(is.na(!!rlang::sym(v)) | nchar(!!rlang::sym(v)) <= !!len),
        brief = sprintf("%s length <= %d (define.xml)", v, len)
      )
    }
  }

  # Controlled terminology
  for (i in which(!vapply(st_in$codes, is.null, logical(1)))) {
    v <- st_in$variable[i]
    set <- st_in$codes[[i]]
    if (is.numeric(data[[v]])) set <- suppressWarnings(as.numeric(set))
    agent <- pointblank::col_vals_in_set(
      agent, columns = dplyr::all_of(v), set = set,
      brief = sprintf("%s values in codelist %s", v, st_in$code_id[i])
    )
  }

  attr(agent, "spec_vars_missing") <- setdiff(st$variable, names(data))
  attr(agent, "data_vars_not_in_spec") <- setdiff(names(data), st$variable)
  agent
}

#' One-row-per-step summary of an interrogated agent (plain tibble)
#' @noRd
qc_agent_summary <- function(agent) {
  x <- agent$validation_set
  tibble::tibble(
    step = x$i,
    check = x$assertion_type,
    column = vapply(x$column, \(c) paste(unlist(c), collapse = ", "), character(1)),
    rule = x$brief,
    n = x$n,
    n_failed = x$n_failed,
    status = dplyr::case_when(
      x$eval_error ~ "ERROR",
      x$all_passed ~ "PASS",
      TRUE ~ "FAIL"
    )
  )
}

#' ADSL conformance to the define.xml
#'
#' Runs the approved spec-driven pointblank check of the ADSL dataset against
#' the CDISC pilot 3 ADaM define.xml: keys, data types, SAS lengths and
#' controlled terminology. Returns the pointblank agent report.
#'
#' @param which `enum[primary, qc]` Which ADSL to check: the primary
#'   programmer's dataset or the independent QC dataset.
#' @return A text summary of every failing rule (with offending values) for
#'   the model, and the full pointblank agent report for the user.
#' @provenance https://github.com/pharmaverse/examples/blob/d35207a5b50ab21be4b8aea6e39de5f6c3a22b28/adam/adsl.R#L317-L329
#' @measure
adsl_spec_check <- function(which = "primary", adam) {
  tbl <- if (which == "qc") "qc_adsl" else "adsl"
  data <- DBI::dbReadTable(adam, tbl) |> qc_restore_dates()
  agent <- qc_spec_agent(data, tbl_name = tbl) |> pointblank::interrogate(progress = FALSE)
  report <- pointblank::get_agent_report(agent, title = sprintf("ADSL (%s) vs define.xml", which))

  vs <- agent$validation_set
  summ <- qc_agent_summary(agent)
  failing <- which(summ$status != "PASS")
  lines <- vapply(failing, \(i) {
    offending <- ""
    if (vs$assertion_type[i] == "col_vals_in_set") {
      col <- summ$column[i]
      bad <- setdiff(unique(data[[col]]), unlist(vs$values[[i]]))
      offending <- sprintf(" Offending values: %s.", paste(ifelse(is.na(bad), "NA", bad), collapse = ", "))
    }
    sprintf("- FAIL step %d: %s -- %d of %d records fail.%s",
            summ$step[i], summ$rule[i], summ$n_failed[i], summ$n[i], offending)
  }, character(1))
  value <- paste0(
    sprintf("ADSL (%s) vs define.xml: %d rules checked, %d passed, %d failed. ",
            which, nrow(summ), sum(summ$status == "PASS"), length(failing)),
    "A failing rule is a conformance finding against the study spec, not expected behaviour. ",
    "The define.xml states ADSL has one record per subject and Screen Failures are excluded.\n",
    paste(lines, collapse = "\n"),
    sprintf("\nSpec variables not yet derived in this dataset (scope, not failures): %s.",
            paste(attr(agent, "spec_vars_missing"), collapse = ", "))
  )
  ellmer::ContentToolResult(
    value = value,
    extra = list(display = shinychat::tool_result_display(
      html = as.character(gt::as_raw_html(report, inline_css = TRUE))
    ))
  )
}

#' Restore Date columns after a DuckDB/CSV round trip
#' @noRd
qc_restore_dates <- function(data) {
  dt <- grep("DT$", names(data), value = TRUE)
  for (v in dt) {
    if (!inherits(data[[v]], "Date")) data[[v]] <- as.Date(data[[v]])
  }
  data
}
