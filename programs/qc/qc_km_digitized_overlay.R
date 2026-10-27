# =============================================================================
# Program   : programs/qc/qc_km_digitized_overlay.R
# Purpose   : ADVANCED -- km-digitizer skill follow-up. Overlay the KM curve
#             reconstructed from a published PNG (IPDfromKM) on the original
#             image, and quantify agreement against the true ADTTE.
# Drafted by: Posit Assistant (agentic-qc + km-digitizer skills) -- reviewed by QC programmer
# Input     : reference/KMPlot/ttde_legacy.png,
#             output/km_digitizer/ttde_legacy/ipd_combined.csv (programs/qc/run_km_digitizer.R),
#             metadata/km_digitizer/config.json,
#             data/primary/adtte.rds (truth; normally unavailable for a legacy figure)
# Output    : output/figures/f_km_digitized_overlay.png
# =============================================================================

library(dplyr)
library(ggplot2)
library(survival)
library(gt)

cfg <- jsonlite::fromJSON("metadata/km_digitizer/config.json")
ipd <- read.csv("output/km_digitizer/ttde_legacy/ipd_combined.csv")

# ---- Original image, cropped to the plot region found by the pixel scan ------
reg <- cfg$plot_region
img <- magick::image_read(cfg$image_path) |>
  magick::image_crop(sprintf("%dx%d+%d+%d", reg$right - reg$left, reg$bottom - reg$top, reg$left, reg$top))

# ---- Reconstructed KM steps ----------------------------------------------------
fit_ipd <- survfit(Surv(time, event) ~ arm, data = ipd)
steps <- broom::tidy(fit_ipd) |>
  mutate(arm = sub("arm=", "", strata)) |>
  bind_rows(data.frame(time = 0, estimate = 1, arm = unique(ipd$arm)))

overlay <- ggplot(steps, aes(time, 100 * estimate, group = arm)) +
  annotation_raster(as.raster(img), xmin = cfg$x_range[1], xmax = cfg$x_range[2],
                    ymin = cfg$y_range[1], ymax = cfg$y_range[2]) +
  geom_step(linewidth = 0.9, linetype = "22", colour = "black") +
  scale_x_continuous(breaks = seq(0, 200, 25)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  coord_cartesian(xlim = cfg$x_range, ylim = cfg$y_range, expand = FALSE) +
  labs(
    title = "Reconstructed IPD (black dashed) over the published figure",
    subtitle = "km-digitizer skill: PNG -> digitized curve -> IPDfromKM -> survfit",
    x = cfg$x_label, y = cfg$y_label
  ) +
  theme_minimal(base_size = 12)

ragg::agg_png("output/figures/f_km_digitized_overlay.png", width = 9, height = 5.5, units = "in", res = 150)
print(overlay)
invisible(dev.off())

# ---- Agreement with the truth --------------------------------------------------
truth <- readRDS("data/primary/adtte.rds") |>
  filter(PARAMCD == "TTDE", TRT01A %in% cfg$curve_names) |>
  transmute(time = AVAL, event = 1 - CNSR, arm = TRT01A)

km_summary <- function(d, source) {
  f <- survfit(Surv(time, event) ~ arm, data = d)
  s <- summary(f, times = c(25, 50, 100), extend = TRUE)
  tibble::tibble(
    source = source,
    arm = sub("arm=", "", as.character(s$strata)),
    day = s$time,
    surv_pct = 100 * s$surv
  ) |>
    left_join(
      d |> group_by(arm) |> summarise(N = n(), events = sum(event), .groups = "drop"),
      by = "arm"
    )
}

km_agreement <- bind_rows(km_summary(truth, "Truth (ADTTE)"), km_summary(ipd, "Reconstructed")) |>
  tidyr::pivot_wider(id_cols = c(arm, day), names_from = source,
                     values_from = c(surv_pct, events)) |>
  mutate(diff_pp = `surv_pct_Reconstructed` - `surv_pct_Truth (ADTTE)`)

km_agreement_tbl <- km_agreement |>
  gt(groupname_col = "arm") |>
  tab_header("Digitized vs true KM estimates", "Event-free % at day 25 / 50 / 100") |>
  fmt_number(columns = starts_with("surv_pct") | diff_pp, decimals = 1) |>
  cols_label(day = "Day", `surv_pct_Truth (ADTTE)` = "Truth %", surv_pct_Reconstructed = "Reconstructed %",
             `events_Truth (ADTTE)` = "Truth events", events_Reconstructed = "Reconstructed events",
             diff_pp = "Diff (pp)")
print(km_agreement_tbl)
magick::image_read("output/figures/f_km_digitized_overlay.png") |> print()
