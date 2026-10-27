# =============================================================================
# Program   : programs/00_setup.R
# Purpose   : Agentic QC technical stack -- install (if missing), load, and
#             report versions of every package the QC skills depend on.
# Output    : gt table of the stack (Viewer)
# =============================================================================

stack <- tibble::tribble(
  ~package,          ~role,
  "pharmaversesdtm", "Data source: CDISC pilot SDTM",
  "pharmaverseadam", "Data source: CDISC pilot ADaM (ADAE, ADTTE)",
  "metacore",        "Metadata: parses ADaM_define_CDISC_pilot3.xml (ships in metacore/extdata)",
  "metatools",       "Metadata: codelist / variable helpers",
  "admiral",         "Primary derivations (pharmaverse)",
  "cards",           "Analysis Results Datasets (ARDs)",
  "pointblank",      "Validation: spec-driven dataset interrogation + HTML report",
  "gt",              "Validation: ARD comparison and provenance tables",
  "diffdf",          "Validation: dataset double-programming compare",
  "magick",          "TLF graphics: bitmap-to-bitmap figure comparison",
  "ggsurvfit",       "TLF graphics: standard KM figure",
  "commons",         "Provenance: trusted-code agent (green / blue / yellow markers)",
  "ellmer",          "LLM client for commons (AWS Bedrock)",
  "logrx",           "Audit trail: clinical-grade execution logs",
  "IPDfromKM",       "km-digitizer skill: IPD reconstruction",
  "callr",           "Clean-process execution for logged runs"
)

missing <- setdiff(stack$package, rownames(installed.packages()))
if (length(missing)) install.packages(missing)

invisible(lapply(stack$package, \(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))

stack_tbl <- stack |>
  dplyr::mutate(version = vapply(package, \(p) as.character(packageVersion(p)), "")) |>
  gt::gt() |>
  gt::tab_header("Agentic QC technical stack", paste(R.version.string, "| Positron")) |>
  gt::cols_label(package = "Package", role = "Role", version = "Version") |>
  gt::tab_style(gt::cell_text(font = "monospace", weight = "bold"), gt::cells_body(columns = package))
print(stack_tbl)
