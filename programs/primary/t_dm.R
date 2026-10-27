# =============================================================================
# Program   : programs/primary/t_dm.R
# Purpose   : PRIMARY Table 14.1.2 Demographics (Safety Population)
#             ARD with {cards}, display with {gt}
# Template  : pharmaverse/examples tlg/demographic.R (cards::ard_stack)
# Input     : data/primary/adsl.rds
# Output    : data/primary/ard_dm.rds, output/tables/t_dm_primary.html
# =============================================================================

library(cards)
library(dplyr)
library(tidyr)
library(gt)

adsl <- readRDS("data/primary/adsl.rds") |>
  filter(SAFFL == "Y") |>
  mutate(
    AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
    TRT01A = factor(TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  )

# ---- Analysis Results Dataset ------------------------------------------------
ard_dm <- ard_stack(
  adsl,
  ard_continuous(
    variables = AGE,
    statistic = ~ continuous_summary_fns(c("N", "mean", "sd", "median", "p25", "p75", "min", "max"))
  ),
  ard_categorical(variables = c(AGEGR1, SEX, RACE)),
  .by = TRT01A
)

dir.create("data/primary", recursive = TRUE, showWarnings = FALSE)
saveRDS(ard_dm, "data/primary/ard_dm.rds")

# ---- Display -----------------------------------------------------------------
stats <- ard_dm |>
  filter(!is.na(group1)) |>
  mutate(
    arm = unlist(group1_level),
    level = vapply(variable_level, \(x) if (length(x)) as.character(x[[1]]) else NA_character_, ""),
    value = unlist(stat)
  ) |>
  select(arm, variable, level, stat_name, value)

cont <- stats |>
  filter(variable == "AGE") |>
  pivot_wider(names_from = stat_name, values_from = value) |>
  transmute(
    arm, variable = "Age (years)",
    `n` = as.character(N),
    `Mean (SD)` = paste0(round(mean, 1), " (", round(sd, 2), ")"),
    `Median` = as.character(round(median, 1)),
    `Q1, Q3` = paste0(round(p25, 1), ", ", round(p75, 1)),
    `Min, Max` = paste0(min, ", ", max)
  ) |>
  pivot_longer(-c(arm, variable), names_to = "label", values_to = "cell")

cat_rows <- stats |>
  filter(variable != "AGE", stat_name %in% c("n", "p")) |>
  pivot_wider(names_from = stat_name, values_from = value) |>
  transmute(
    arm,
    variable = recode_values(variable, "AGEGR1" ~ "Age group (years)", "SEX" ~ "Sex", "RACE" ~ "Race"),
    label = level,
    cell = paste0(n, " (", format(round(p * 100, 1), nsmall = 1), "%)")
  )

big_n <- adsl |> count(TRT01A)
col_labels <- setNames(
  as.list(sprintf("%s<br>(N=%d)", big_n$TRT01A, big_n$n)),
  as.character(big_n$TRT01A)
)

t_dm <- bind_rows(cont, cat_rows) |>
  pivot_wider(names_from = arm, values_from = cell) |>
  gt(groupname_col = "variable", rowname_col = "label") |>
  tab_header(
    title = "Table 14.1.2 Summary of Demographic Characteristics",
    subtitle = "Safety Population"
  ) |>
  cols_label(.list = lapply(col_labels, gt::html)) |>
  sub_missing(missing_text = "0 (0.0%)") |>
  tab_source_note("Source: ADSL. Percentages based on N per treatment group.")

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
gtsave(t_dm, "output/tables/t_dm_primary.html")
print(t_dm)
