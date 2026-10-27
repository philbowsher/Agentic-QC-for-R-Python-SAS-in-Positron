# =============================================================================
# Program   : programs/qc/qc_t_dm.R
# Purpose   : ACTION 2A -- TLF QC. Independent QC ARD for Table 14.1.2
#             Demographics, compared to the primary {cards} ARD under the
#             CAMIS-aligned tolerance policy, rendered with {gt}.
# Drafted by: Posit Assistant (agentic-qc skill) -- reviewed by QC programmer
# Input     : data/qc/qc_adsl.rds, data/primary/ard_dm.rds
# Output    : data/qc/ard_dm_qc.csv, output/tables/t_dm_ard_compare.html
# -----------------------------------------------------------------------------
# The QC ARD stands in for a cross-language hand-off (e.g. a SAS PROC MEANS /
# PROC FREQ output dataset): it is computed independently with dplyr and base
# R defaults, and stored at 3 decimal places, as an exported dataset would be.
# =============================================================================

library(dplyr)
library(tidyr)
library(gt)
source("trusted/qc_compare.R")

qc_adsl <- readRDS("data/qc/qc_adsl.rds") |> filter(SAFFL == "Y")

# ---- Independent QC ARD -------------------------------------------------------
qc_cont <- qc_adsl |>
  group_by(group1_level = TRT01A) |>
  summarise(
    N = sum(!is.na(AGE)),
    mean = mean(AGE),
    sd = sd(AGE),
    median = median(AGE),
    p25 = quantile(AGE, 0.25),
    p75 = quantile(AGE, 0.75),
    min = min(AGE),
    max = max(AGE),
    .groups = "drop"
  ) |>
  pivot_longer(-group1_level, names_to = "stat_name", values_to = "stat") |>
  mutate(variable = "AGE", variable_level = NA_character_)

# Zero-count cells are kept (complete()) so every level appears in every arm
qc_cat <- qc_adsl |>
  select(TRT01A, AGEGR1, SEX, RACE) |>
  pivot_longer(-TRT01A, names_to = "variable", values_to = "variable_level") |>
  count(group1_level = TRT01A, variable, variable_level, name = "n") |>
  complete(group1_level, nesting(variable, variable_level), fill = list(n = 0)) |>
  group_by(group1_level, variable) |>
  mutate(N = sum(n), p = n / N) |>
  ungroup() |>
  pivot_longer(c(n, N, p), names_to = "stat_name", values_to = "stat") |>
  mutate(group1 = "TRT01A")

# Treatment-group big N (column headers)
qc_bign <- qc_adsl |>
  count(variable_level = TRT01A, name = "n") |>
  mutate(N = sum(n), p = n / N, variable = "TRT01A", group1 = NA_character_,
         group1_level = NA_character_) |>
  pivot_longer(c(n, N, p), names_to = "stat_name", values_to = "stat")

ard_dm_qc <- bind_rows(mutate(qc_cont, group1 = "TRT01A"), qc_cat, qc_bign) |>
  mutate(stat = round(stat, 3)) |>
  select(group1, group1_level, variable, variable_level, stat_name, stat)

dir.create("data/qc", recursive = TRUE, showWarnings = FALSE)
write.csv(ard_dm_qc, "data/qc/ard_dm_qc.csv", row.names = FALSE, na = "")

# ---- Compare under the CAMIS-aligned tolerance policy ------------------------
ard_dm_primary <- readRDS("data/primary/ard_dm.rds")
ard_dm_qc <- read.csv("data/qc/ard_dm_qc.csv", na.strings = "")

ard_cmp <- qc_compare_ard(primary = ard_dm_primary, qc = ard_dm_qc)
print(count(ard_cmp, status))

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
qc_gt_ard_comparison(ard_cmp, show = "all",
                     title = "Table 14.1.2 Demographics -- primary vs QC ARD (all statistics)") |>
  gtsave("output/tables/t_dm_ard_compare_all.html")

# Review view: only what a human must act on
t_cmp <- qc_gt_ard_comparison(ard_cmp, show = "review",
                              title = "Table 14.1.2 Demographics -- primary vs QC ARD")
gtsave(t_cmp, "output/tables/t_dm_ard_compare.html")
print(t_cmp)
