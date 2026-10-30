# =============================================================================
# TRUSTED CODE BANK :: Double-programming comparisons (datasets and ARDs)
# -----------------------------------------------------------------------------
# Status   : APPROVED (v1.0.0, 2026-10-23)  -- green-shield code
# Owner    : Statistical Programming Standards
# Lineage  : Dataset compare = {diffdf} (PROC COMPARE analogue).
#            ARD compare     = {cards} ARD structure (pharmaverse/examples
#            tlg/demographic.R) + CAMIS-aligned tolerance policy below.
# =============================================================================

# ---- Company tolerance policy (CAMIS-aligned) -------------------------------
# CAMIS documents *why* SAS/R/Python results differ; it does not prescribe a
# numeric tolerance. The thresholds below are this company's policy and are
# deliberately explicit so a reviewer can see and challenge them.
qc_tolerance_policy <- list(
  # Counts and denominators must match exactly
  exact_stats = c("n", "N", "N_obs", "N_miss", "N_nonmiss", "n_cum"),
  # Absolute tolerance for every other numeric statistic (proportions on the
  # 0-1 scale, means, SDs). 1e-3 absorbs storage/precision artifacts such as
  # 16.11111 vs 16.112 (|diff| = 8.9e-4) but not real differences.
  abs_tol = 1e-3,
  # Statistics with documented cross-language method differences (CAMIS).
  # A difference here is NOT auto-passed: it is labelled so a human can
  # confirm the method, then accept or reject.
  camis_methods = list(
    p25 = "Quantile definition: SAS default PCTLDEF=5 = R type 2; R default is type 7",
    p75 = "Quantile definition: SAS default PCTLDEF=5 = R type 2; R default is type 7"
  ),
  camis_urls = list(
    p25 = "https://psiaims.github.io/CAMIS/Comp/r-sas-summary-stats.html",
    p75 = "https://psiaims.github.io/CAMIS/Comp/r-sas-summary-stats.html"
  )
)

#' Flatten a {cards} ARD (or an ARD-shaped data frame) to one row per statistic
#' @noRd
qc_flatten_ard <- function(ard) {
  ard <- as.data.frame(ard)
  to_chr <- function(x) {
    if (is.list(x)) vapply(x, \(v) if (length(v) == 0 || is.null(v)) NA_character_ else as.character(v[[1]]), character(1))
    else as.character(x)
  }
  to_num <- function(x) {
    if (is.list(x)) vapply(x, \(v) if (length(v) == 0 || is.null(v) || !is.numeric(v[[1]])) NA_real_ else as.numeric(v[[1]]), numeric(1))
    else suppressWarnings(as.numeric(x))
  }
  out <- tibble::tibble(
    group1 = if ("group1" %in% names(ard)) to_chr(ard$group1) else NA_character_,
    group1_level = if ("group1_level" %in% names(ard)) to_chr(ard$group1_level) else NA_character_,
    variable = to_chr(ard$variable),
    variable_level = if ("variable_level" %in% names(ard)) to_chr(ard$variable_level) else NA_character_,
    stat_name = to_chr(ard$stat_name),
    stat = to_num(ard$stat)
  )
  out[!is.na(out$stat), ]
}

#' Compare a primary and a QC ARD under the CAMIS-aligned tolerance policy
#'
#' Returns one row per statistic with a `status`:
#'   MATCH            identical (|diff| < 1e-12)
#'   PASS_PRECISION   within policy$abs_tol -- floating point / storage artifact
#'   CAMIS_METHOD     differs, statistic has a documented SAS/R method difference
#'   FAIL             true discrepancy (exact stat differs, or beyond tolerance)
#'   MISSING_QC / MISSING_PRIMARY   statistic only produced on one side
#' @noRd
qc_compare_ard <- function(primary, qc, policy = qc_tolerance_policy) {
  keys <- c("group1", "group1_level", "variable", "variable_level", "stat_name")
  p <- qc_flatten_ard(primary) |> dplyr::rename(primary = stat)
  q <- qc_flatten_ard(qc) |> dplyr::rename(qc = stat)

  dplyr::full_join(p, q, by = keys) |>
    dplyr::mutate(
      diff = qc - primary,
      abs_diff = abs(diff),
      is_exact_stat = stat_name %in% policy$exact_stats,
      is_camis = stat_name %in% names(policy$camis_methods),
      status = dplyr::case_when(
        is.na(qc) ~ "MISSING_QC",
        is.na(primary) ~ "MISSING_PRIMARY",
        abs_diff < 1e-12 ~ "MATCH",
        is_exact_stat ~ "FAIL",
        dplyr::near(primary, qc, tol = policy$abs_tol) ~ "PASS_PRECISION",
        is_camis ~ "CAMIS_METHOD",
        TRUE ~ "FAIL"
      ),
      note = dplyr::case_when(
        status == "PASS_PRECISION" ~ sprintf("|diff| = %.1e <= tol %.0e", abs_diff, policy$abs_tol),
        status == "CAMIS_METHOD" ~ vapply(stat_name, \(s) policy$camis_methods[[s]] %||% NA_character_, character(1)),
        status == "FAIL" & is_exact_stat ~ "Counts/denominators must match exactly",
        status == "FAIL" ~ sprintf("|diff| = %.3g > tol %.0e", abs_diff, policy$abs_tol),
        TRUE ~ NA_character_
      ),
      reference = dplyr::if_else(
        status == "CAMIS_METHOD",
        vapply(stat_name, \(s) policy$camis_urls[[s]] %||% NA_character_, character(1)),
        NA_character_
      )
    ) |>
    dplyr::select(-is_exact_stat, -is_camis) |>
    dplyr::mutate(status = factor(status, levels = c(
      "FAIL", "MISSING_QC", "MISSING_PRIMARY", "CAMIS_METHOD", "PASS_PRECISION", "MATCH"
    ))) |>
    dplyr::arrange(status, variable, variable_level, group1_level, stat_name)
}

#' Render an ARD comparison as a gt table
#' @param show "review" (FAIL / MISSING / CAMIS_METHOD only -- what a human
#'   must act on), "issues" (everything except MATCH) or "all"
#' @noRd
qc_gt_ard_comparison <- function(cmp, show = c("review", "issues", "all"),
                                 title = "ARD double-programming comparison",
                                 subtitle = NULL) {
  show <- match.arg(show)
  counts <- table(cmp$status)
  if (is.null(subtitle)) {
    subtitle <- paste(sprintf("%s: %d", names(counts), as.integer(counts))[counts > 0], collapse = "  |  ")
  }
  dat <- switch(show,
    review = dplyr::filter(cmp, status %in% c("FAIL", "MISSING_QC", "MISSING_PRIMARY", "CAMIS_METHOD")),
    issues = dplyr::filter(cmp, status != "MATCH"),
    all = cmp
  )
  group_labels <- c(
    FAIL = "FAIL -- true discrepancy, investigate",
    MISSING_QC = "MISSING IN QC -- statistic not reproduced",
    MISSING_PRIMARY = "MISSING IN PRIMARY -- statistic not produced",
    CAMIS_METHOD = "CAMIS METHOD DIFFERENCE -- confirm method, then sign off",
    PASS_PRECISION = "PASS -- floating-point / storage precision artifact",
    MATCH = "MATCH"
  )
  dat <- dplyr::mutate(dat,
    variable_level = dplyr::coalesce(variable_level, ""),
    group = factor(group_labels[as.character(status)], levels = group_labels),
    status = as.character(status)
  ) |>
    dplyr::arrange(group)

  palette <- c(
    FAIL = "#F8D7DA", MISSING_QC = "#F8D7DA", MISSING_PRIMARY = "#F8D7DA",
    CAMIS_METHOD = "#FFF3CD", PASS_PRECISION = "#D1E7DD", MATCH = "#FFFFFF"
  )

  tbl <- dat |>
    dplyr::select(group, status, group1_level, variable, variable_level, stat_name,
                  primary, qc, diff, note, reference) |>
    gt::gt(groupname_col = "group") |>
    gt::cols_hide(status) |>
    gt::tab_header(title = title, subtitle = subtitle) |>
    gt::cols_label(
      group1_level = "Treatment", variable = "Variable",
      variable_level = "Level", stat_name = "Stat", primary = "Primary",
      qc = "QC", diff = "Diff", note = "Reason", reference = ""
    ) |>
    gt::fmt_number(columns = c(primary, qc), decimals = 5, drop_trailing_zeros = TRUE) |>
    gt::fmt_number(columns = diff, n_sigfig = 3) |>
    gt::fmt_url(columns = reference, label = "CAMIS") |>
    gt::sub_missing(missing_text = "") |>
    gt::cols_width(note ~ gt::px(300), group1_level ~ gt::px(120)) |>
    gt::tab_source_note(sprintf(
      "Policy: counts exact; other statistics |diff| <= %g; documented SAS/R method differences flagged for human sign-off (CAMIS, psiaims.github.io/CAMIS).",
      qc_tolerance_policy$abs_tol
    )) |>
    gt::tab_style(gt::cell_text(weight = "bold"), gt::cells_row_groups()) |>
    gt::tab_options(table.font.size = gt::px(13), data_row.padding = gt::px(3))

  for (s in names(palette)) {
    rows <- which(dat$status == s)
    if (length(rows)) {
      tbl <- gt::tab_style(tbl, gt::cell_fill(color = palette[[s]]), gt::cells_body(rows = rows))
    }
  }
  tbl
}

#' Dataset double-programming compare (PROC COMPARE analogue via {diffdf})
#'
#' Returns a list: `diffdf` (the raw object), `summary` (one row per variable
#' with a difference count) and `rows` (subject-level value differences).
#' @noRd
qc_compare_datasets <- function(base, compare, keys = "USUBJID") {
  common <- intersect(names(base), names(compare))

  # Structure checks (PROC COMPARE-style), done before attributes are stripped
  type_of <- function(x) paste(class(x), collapse = "/")
  label_of <- function(x) { l <- attr(x, "label"); if (is.null(l)) NA_character_ else as.character(l) }
  str_cmp <- tibble::tibble(
    VARIABLE = common,
    type_base = vapply(base[common], type_of, ""),
    type_compare = vapply(compare[common], type_of, ""),
    label_base = vapply(base[common], label_of, ""),
    label_compare = vapply(compare[common], label_of, "")
  )
  structure <- list(
    n_obs = c(base = nrow(base), compare = nrow(compare)),
    n_vars = c(base = ncol(base), compare = ncol(compare)),
    vars_only_in_base = setdiff(names(base), names(compare)),
    vars_only_in_compare = setdiff(names(compare), names(base)),
    type_diffs = dplyr::filter(str_cmp, type_base != type_compare) |>
      dplyr::select(VARIABLE, type_base, type_compare),
    label_diffs = dplyr::filter(str_cmp, dplyr::coalesce(label_base, "") != dplyr::coalesce(label_compare, "")) |>
      dplyr::select(VARIABLE, label_base, label_compare)
  )

  base <- as.data.frame(base)[common]
  compare <- as.data.frame(compare)[common]
  # Strip attributes (labels/formats) so only values are compared
  base[] <- lapply(base, \(x) { attributes(x) <- attributes(x)[intersect(names(attributes(x)), "class")]; x })
  compare[] <- lapply(compare, \(x) { attributes(x) <- attributes(x)[intersect(names(attributes(x)), "class")]; x })

  dd <- diffdf::diffdf(base, compare, keys = keys, suppress_warnings = TRUE)
  var_diffs <- dd[grepl("^VarDiff_", names(dd))]
  rows <- if (length(var_diffs)) {
    dplyr::bind_rows(lapply(var_diffs, \(x) {
      x <- as.data.frame(x)
      x$BASE <- as.character(x$BASE)
      x$COMPARE <- as.character(x$COMPARE)
      x
    }))
  } else {
    tibble::tibble()
  }
  summary <- if (nrow(rows)) {
    dplyr::count(rows, VARIABLE, name = "n_diff") |> dplyr::arrange(dplyr::desc(n_diff))
  } else {
    tibble::tibble(VARIABLE = character(), n_diff = integer())
  }
  list(
    diffdf = dd,
    structure = structure,
    summary = summary,
    rows = tibble::as_tibble(rows),
    extra_rows_base = if (!is.null(dd$ExtRowsBase)) nrow(dd$ExtRowsBase) else 0L,
    extra_rows_compare = if (!is.null(dd$ExtRowsComp)) nrow(dd$ExtRowsComp) else 0L,
    identical = length(dd) == 0
  )
}

#' Demographics ARD: primary vs independent QC
#'
#' Compares the primary programmer's demographics Analysis Results Dataset
#' (built with {cards}) against the independently programmed QC ARD, using the
#' company CAMIS-aligned tolerance policy: counts exact, other statistics
#' within an absolute tolerance of 0.001, documented SAS/R method differences
#' (e.g. quantile definitions) flagged for human sign-off.
#'
#' @param show `enum[issues, all]` Show only non-matching statistics, or all.
#' @return A gt table, one row per statistic, colour-coded by status.
#' @provenance https://github.com/pharmaverse/examples/blob/d35207a5b50ab21be4b8aea6e39de5f6c3a22b28/tlg/demographic.R#L24-L37
#' @provenance https://psiaims.github.io/CAMIS/Comp/r-sas-summary-stats.html (retrieved 2026-10-23)
#' @measure
dm_ard_compare <- function(show = "issues", adam) {
  primary <- DBI::dbReadTable(adam, "ard_dm_primary")
  qc <- DBI::dbReadTable(adam, "ard_dm_qc")
  qc_compare_ard(primary, qc) |> qc_gt_ard_comparison(show = show)
}

#' ADSL double-programming compare
#'
#' Compares the primary ADSL with the independently programmed QC ADSL,
#' matched on USUBJID, using {diffdf}. Reports subjects present in only one
#' dataset and every variable whose values differ.
#'
#' @return A gt table summarising differences per variable.
#' @provenance https://github.com/pharmaverse/examples/blob/d35207a5b50ab21be4b8aea6e39de5f6c3a22b28/adam/adsl.R#L1-L330
#' @measure
adsl_double_programming_compare <- function(adam) {
  primary <- DBI::dbReadTable(adam, "adsl")
  qc <- DBI::dbReadTable(adam, "qc_adsl")
  res <- qc_compare_datasets(primary, qc)
  dplyr::bind_rows(
    tibble::tibble(VARIABLE = "(subjects only in primary)", n_diff = res$extra_rows_base),
    tibble::tibble(VARIABLE = "(subjects only in QC)", n_diff = res$extra_rows_compare),
    res$summary
  ) |>
    gt::gt() |>
    gt::tab_header("ADSL: primary vs independent QC (diffdf)") |>
    gt::cols_label(VARIABLE = "Variable", n_diff = "Records differing")
}
