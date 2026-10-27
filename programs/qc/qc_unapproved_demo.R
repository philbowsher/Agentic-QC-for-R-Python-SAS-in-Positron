# =============================================================================
# Program   : programs/qc/qc_unapproved_demo.R
# Purpose   : DEMO ONLY -- deliberately breaks the approved-package policy so the
#             {logrx} log shows what gets flagged. Not part of the QC pipeline.
# Breaks    : 1. calls {scales}, which is not on the approved list
#             2. defines an ad-hoc helper instead of using trusted/ code
# =============================================================================

library(dplyr)

adsl <- readRDS("data/primary/adsl.rds")

# Ad-hoc helper written on the fly (not in the trusted code bank)
pct_by_arm <- function(data, var) {
  data |>
    count(TRT01A, {{ var }}) |>
    group_by(TRT01A) |>
    mutate(pct = n / sum(n)) |>
    ungroup()
}

sex_pct <- pct_by_arm(adsl, SEX) |>
  mutate(label = scales::percent(pct, accuracy = 0.1))

print(sex_pct)
