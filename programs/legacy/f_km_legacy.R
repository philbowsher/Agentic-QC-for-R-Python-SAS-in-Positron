# =============================================================================
# Program   : programs/legacy/f_km_legacy.R
# Purpose   : Create a "legacy / published" KM figure (PNG only) to stand in
#             for a competitor or historical TLF whose patient-level data is
#             NOT available. Used by the km-digitizer skill demo.
# Output    : reference/KMPlot/ttde_legacy.png
# Note      : The underlying data is our own ADTTE, so the digitized
#             reconstruction can be checked against the truth.
# =============================================================================

library(dplyr)
library(ggplot2)
library(survival)

adtte <- readRDS("data/primary/adtte.rds") |>
  filter(PARAMCD == "TTDE", TRT01A %in% c("Placebo", "Xanomeline High Dose")) |>
  mutate(TRT01A = factor(TRT01A, levels = c("Placebo", "Xanomeline High Dose")))

fit <- survfit(Surv(AVAL, 1 - CNSR) ~ TRT01A, data = adtte)
annot_times <- c(25, 50, 100, 150)
s <- summary(fit, times = annot_times, extend = TRUE)
annot <- data.frame(
  time = s$time,
  surv = s$surv,
  arm = sub("TRT01A=", "", as.character(s$strata))
) |>
  mutate(label = sprintf("%.1f%%", 100 * surv))
risk_times <- seq(0, 200, by = 25)
nar <- summary(fit, times = risk_times, extend = TRUE)
nar_df <- data.frame(time = nar$time, n = nar$n.risk, arm = sub("TRT01A=", "", as.character(nar$strata)))

steps <- broom::tidy(fit) |>
  mutate(arm = sub("TRT01A=", "", strata)) |>
  bind_rows(data.frame(time = 0, estimate = 1, arm = levels(adtte$TRT01A))) |>
  arrange(arm, time)

cols <- c("Placebo" = "#0055D4", "Xanomeline High Dose" = "#D40000")

km <- ggplot(steps, aes(time, 100 * estimate, colour = arm)) +
  geom_step(linewidth = 1.1) +
  geom_point(data = annot, aes(time, 100 * surv), size = 2.2, show.legend = FALSE) +
  geom_text(data = annot, aes(time, 100 * surv, label = label),
            vjust = -1, size = 3.6, show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(limits = c(0, 200), breaks = risk_times, expand = expansion(add = c(0, 6))) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  labs(x = "Days since first dose", y = "Event-free (%)", colour = NULL,
       title = "Time to First Dermatologic Event (published figure)") +
  theme_classic(base_size = 13) +
  theme(legend.position = "top", panel.grid.major.y = element_line(colour = "grey90"))

risk <- ggplot(nar_df, aes(time, factor(arm, levels = rev(levels(adtte$TRT01A))), label = n)) +
  geom_text(size = 3.8) +
  scale_x_continuous(limits = c(0, 200), breaks = risk_times, expand = expansion(add = c(0, 6))) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL, title = "No. at risk") +
  theme_void(base_size = 12) +
  theme(axis.text.y = element_text(hjust = 1, margin = margin(r = 18)),
        plot.title = element_text(size = 11))

dir.create("reference/KMPlot", recursive = TRUE, showWarnings = FALSE)
ragg::agg_png("reference/KMPlot/ttde_legacy.png", width = 10, height = 6.5, units = "in", res = 120)
print(patchwork::wrap_plots(km, risk, ncol = 1, heights = c(4, 1)))
invisible(dev.off())
