# =============================================================================
# Program   : programs/qc/qc_f_km_ttde.R
# Purpose   : ACTION 2B -- Figure QC. Independent QC of Figure 14.2.1 (KM, time
#             to first dermatologic event) and bitmap-to-bitmap comparison
#             against the primary PNG with {magick}.
# Drafted by: Posit Assistant (agentic-qc skill) -- reviewed by QC programmer
# Input     : data/qc/qc_adsl.rds, pharmaverseadam::adae,
#             output/figures/f_km_ttde_primary.png
# Output    : output/figures/f_km_ttde_qc.png, output/figures/f_km_ttde_diff.png
# =============================================================================

library(dplyr)
source("trusted/qc_figures.R")

# ---- Independent QC ADTTE (dplyr, no admiral) ---------------------------------
qc_adsl <- readRDS("data/qc/qc_adsl.rds") |> filter(SAFFL == "Y", !is.na(TRTEDT))

first_derm <- pharmaverseadam::adae |>
  filter(AESOC == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" | grepl("APPLICATION SITE", AEDECOD)) |>
  group_by(USUBJID) |>
  summarise(EVDT = min(ASTDT), .groups = "drop")

qc_adtte <- qc_adsl |>
  left_join(first_derm, by = "USUBJID") |>
  mutate(
    event = !is.na(EVDT) & EVDT >= TRTSDT & EVDT <= TRTEDT + 30,
    ADT = if_else(event, EVDT, TRTEDT),
    CNSR = if_else(event, 0L, 1L),
    AVAL = as.numeric(ADT - TRTSDT) + 1,
    PARAMCD = "TTDE",
    TRT01A = factor(TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  )

# ---- Render with the SAME trusted figure standard as the primary -------------
f_km_qc <- tlf_km_plot(qc_adtte)
tlf_save_png(f_km_qc, "output/figures/f_km_ttde_qc.png")

# ---- Bitmap-to-bitmap comparison ----------------------------------------------
km_cmp <- qc_compare_figures(
  primary_png = "output/figures/f_km_ttde_primary.png",
  qc_png = "output/figures/f_km_ttde_qc.png",
  out_png = "output/figures/f_km_ttde_diff.png"
)
print(km_cmp[c("status", "pixels_different", "pct_different")])

# ---- Drill-down: which subjects drive the visual difference? -----------------
primary_adtte <- readRDS("data/primary/adtte.rds")
km_subject_diffs <- primary_adtte |>
  select(USUBJID, TRT01A, AVAL_primary = AVAL, CNSR_primary = CNSR) |>
  full_join(select(qc_adtte, USUBJID, AVAL_qc = AVAL, CNSR_qc = CNSR), by = "USUBJID") |>
  filter(is.na(AVAL_primary) | is.na(AVAL_qc) |
           AVAL_primary != AVAL_qc | CNSR_primary != CNSR_qc)
print(km_subject_diffs)

print(magick::image_read(km_cmp$composite))
