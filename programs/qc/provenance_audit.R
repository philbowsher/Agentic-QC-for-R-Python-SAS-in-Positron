# =============================================================================
# Program   : programs/qc/provenance_audit.R
# Purpose   : ACCOUNTABILITY -- deterministic provenance audit of every
#             Assistant-drafted QC script: which expressions run trusted code
#             (green), which are backed by a verified citation (blue), and which
#             are novel AI-drafted code needing deep manual review (yellow).
# Input     : programs/qc/*.R, trusted/*.R
# Output    : output/reports/provenance_<script>.html
# =============================================================================

source("trusted/qc_provenance.R")

qc_scripts <- c(
  "programs/qc/qc_adsl_spec.R",
  "programs/qc/qc_adsl.R",
  "programs/qc/qc_adsl_datadict.R",
  "programs/qc/qc_t_dm.R",
  "programs/qc/qc_f_km_ttde.R"
)

audits <- lapply(qc_scripts, qc_audit_provenance)
names(audits) <- basename(qc_scripts)

dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)
for (nm in names(audits)) {
  gt::gtsave(qc_gt_provenance(audits[[nm]]),
             file.path("output/reports", paste0("provenance_", sub("[.]R$", "", nm), ".html")))
}

# Roll-up across scripts (lines of code by label)
provenance_rollup <- dplyr::bind_rows(audits, .id = "script") |>
  dplyr::filter(label != "SETUP") |>
  dplyr::count(script, label, wt = n_lines, name = "lines") |>
  tidyr::pivot_wider(names_from = label, values_from = lines, values_fill = 0)
print(provenance_rollup)

print(qc_gt_provenance(audits[["qc_adsl_spec.R"]]))
