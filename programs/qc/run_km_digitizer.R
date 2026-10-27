# =============================================================================
# Program   : programs/qc/run_km_digitizer.R
# Purpose   : ADVANCED -- run the km-digitizer skill pipeline (Python + R) on the
#             "published" KM figure, from one R script. Demonstrates R and Python
#             working together in one Positron project.
# Requires  : reference/KMPlot/ttde_legacy.png (programs/legacy/f_km_legacy.R)
#             .venv/bin/python with opencv / python-docx (see README)
# Config    : metadata/km_digitizer/config*.json (axis pixels from a pixel scan,
#             calibration points and number-at-risk transcribed from the PNG)
# Output    : output/km_digitizer/ttde_legacy/ (IPD, verification.json, report.docx)
# Note      : Each arm is digitized separately (extract_curve) because both
#             curves start at 100% and the tool's automatic ordering swapped them.
# =============================================================================

# Do not normalizePath(): it follows the venv symlink to the system Python,
# which bypasses the venv and its packages (cv2, docx).
py <- file.path(getwd(), ".venv", "bin", "python")
if (!file.exists(py)) {
  stop("Python env missing. In the Terminal run:\n",
       "  uv venv .venv --python 3.12\n",
       "  uv pip install --python .venv/bin/python opencv-python-headless numpy pandas python-docx matplotlib")
}
rscript <- file.path(R.home("bin"), "Rscript")
skill <- ".posit/assistant/skills/km-digitizer/scripts"
out <- "output/km_digitizer/ttde_legacy"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

run <- function(cmd, ...) {
  res <- suppressWarnings(system2(cmd, shQuote(c(...)), stdout = TRUE, stderr = TRUE))
  status <- attr(res, "status") %||% 0L
  res <- grep("^WARNING: ignoring environment value of R_HOME", res, value = TRUE, invert = TRUE)
  if (status != 0) {
    stop(paste(c(cmd, ..., res), collapse = "\n"))
  }
  res
}

# 1. Python (OpenCV): digitize each arm -> coordinates
for (arm in c("placebo", "high")) {
  cfg <- jsonlite::read_json(sprintf("metadata/km_digitizer/config_%s.json", arm))
  cfg$output_path <- file.path(out, sprintf("km_digitized_%s.json", arm))
  cfg$debug_image <- file.path(out, sprintf("km_debug_%s.png", arm))
  cfg_path <- file.path(out, sprintf("config_%s.json", arm))
  jsonlite::write_json(cfg, cfg_path, auto_unbox = TRUE, pretty = TRUE)
  cat(tail(run(py, file.path(skill, "digitize_km.py"), cfg_path), 2), sep = "\n")
}

# 2. Merge the two arms into one digitized file
p <- jsonlite::read_json(file.path(out, "km_digitized_placebo.json"))
h <- jsonlite::read_json(file.path(out, "km_digitized_high.json"))
p$curves <- c(p$curves, h$curves)
p$number_at_risk <- jsonlite::read_json("metadata/km_digitizer/config.json")$number_at_risk
jsonlite::write_json(p, file.path(out, "km_digitized.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

# 3. R (IPDfromKM): reconstruct patient-level data
cat(tail(run(rscript, file.path(skill, "reconstruct_ipd.R"), file.path(out, "km_digitized.json"), out), 1), sep = "\n")

# 4. R: verification gate (MAE < 3 pp, MaxDev < 5 pp)
verify <- run(rscript, file.path(skill, "verify_reconstruction.R"),
              file.path(out, "km_digitized.json"), file.path(out, "ipd_combined.csv"), out)
cat(grep("PASS|FAIL|MAE|Overall", verify, value = TRUE), sep = "\n")
if (!any(grepl("Overall: PASS", verify))) stop("Verification failed: do not generate the report.")

# 5. Python (python-docx): Word report
cat(tail(run(py, file.path(skill, "generate_report.py"),
             file.path(out, "km_digitized.json"), file.path(out, "ipd_combined.csv"), out), 1), sep = "\n")
