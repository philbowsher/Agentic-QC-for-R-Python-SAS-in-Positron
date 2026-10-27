# =============================================================================
# Program   : programs/primary/f_km_ttde.R
# Purpose   : PRIMARY Figure 14.2.1 KM plot, time to first dermatologic event
# Input     : data/primary/adtte.rds
# Output    : output/figures/f_km_ttde_primary.png
# Standard  : trusted/qc_figures.R tlf_km_plot() + tlf_save_png()
# =============================================================================

source("trusted/qc_figures.R")

adtte <- readRDS("data/primary/adtte.rds") |>
  dplyr::filter(PARAMCD == "TTDE", SAFFL == "Y") |>
  dplyr::mutate(TRT01A = factor(TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")))

f_km <- tlf_km_plot(adtte)
tlf_save_png(f_km, "output/figures/f_km_ttde_primary.png")
print(f_km)
