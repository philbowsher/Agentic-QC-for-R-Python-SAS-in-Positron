# =============================================================================
# Program   : programs/qc/qc_adsl.R
# Purpose   : INDEPENDENT QC derivation of ADSL (double programming) and
#             dataset-level comparison against the primary ADSL
# Approach  : Plain tidyverse from SDTM + define.xml -- deliberately NOT
#             {admiral}, so primary and QC do not share derivation code.
# Drafted by: Posit Assistant (agentic-qc skill) -- reviewed by QC programmer
# Input     : pharmaversesdtm::dm, ds, ex; metadata/ADaM_define_CDISC_pilot3.xml
# Output    : data/qc/qc_adsl.rds, output/reports/adsl_diffdf.txt
# =============================================================================

library(dplyr)
library(tidyr)
source("trusted/qc_spec.R")
source("trusted/qc_compare.R")

# cite: metadata/ADaM_define_CDISC_pilot3.xml
spec <- qc_read_spec("ADSL")
ct <- qc_spec_table(spec)

dm <- pharmaversesdtm::dm |> mutate(across(where(is.character), \(x) na_if(x, "")))
ex <- pharmaversesdtm::ex |> mutate(across(where(is.character), \(x) na_if(x, "")))
ds <- pharmaversesdtm::ds |> mutate(across(where(is.character), \(x) na_if(x, "")))

# Treatment start/end: first/last exposure with a valid dose (active or placebo)
trt_dates <- ex |>
  filter(EXDOSE > 0 | (EXDOSE == 0 & grepl("PLACEBO", EXTRT))) |>
  mutate(
    st = as.Date(substr(EXSTDTC, 1, 10)),
    en = as.Date(substr(EXENDTC, 1, 10))
  ) |>
  group_by(USUBJID) |>
  summarise(
    TRTSDT = if (all(is.na(st))) as.Date(NA) else min(st, na.rm = TRUE),
    TRTEDT = if (all(is.na(en))) as.Date(NA) else max(en, na.rm = TRUE),
    .groups = "drop"
  )

eosstt <- ds |>
  filter(DSCAT == "DISPOSITION EVENT") |>
  transmute(USUBJID, EOSSTT = if_else(DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED"))

arm_codes <- c("Placebo" = 0, "Xanomeline Low Dose" = 54, "Xanomeline High Dose" = 81)
race_codes <- ct |>
  filter(variable == "RACEN") |>
  pull(code_id)
race_map <- spec$codelist |>
  filter(code_id == race_codes) |>
  pull(codes) |>
  _[[1]]

# define.xml: "one record per subject. Screen Failures are excluded."
qc_adsl <- dm |>
  filter(ARMCD != "Scrnfail") |>
  left_join(trt_dates, by = "USUBJID") |>
  left_join(eosstt, by = "USUBJID") |>
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM,
    TRT01PN = unname(arm_codes[TRT01P]),
    TRT01AN = unname(arm_codes[TRT01A]),
    TRTDURD = as.numeric(TRTEDT - TRTSDT) + 1,
    AGEGR1 = case_when(AGE < 65 ~ "<65", AGE <= 80 ~ "65-80", AGE > 80 ~ ">80"),
    AGEGR1N = case_when(AGEGR1 == "<65" ~ 1, AGEGR1 == "65-80" ~ 2, AGEGR1 == ">80" ~ 3),
    RACEN = as.numeric(race_map$code[match(RACE, race_map$decode)]),
    SAFFL = if_else(!is.na(TRTSDT), "Y", "N"),
    ITTFL = "Y",
    EOSSTT = coalesce(EOSSTT, "ONGOING")
  ) |>
  select(STUDYID, USUBJID, SUBJID, SITEID, ARM, TRT01P, TRT01PN, TRT01A, TRT01AN,
         TRTSDT, TRTEDT, TRTDURD, AGE, AGEGR1, AGEGR1N, AGEU, RACE, RACEN, SEX,
         ETHNIC, SAFFL, ITTFL, DTHFL, RFSTDTC, RFENDTC, EOSSTT) |>
  arrange(USUBJID)

dir.create("data/qc", recursive = TRUE, showWarnings = FALSE)
saveRDS(qc_adsl, "data/qc/qc_adsl.rds")

# ---- Double-programming comparison (PROC COMPARE analogue) -------------------
adsl <- readRDS("data/primary/adsl.rds")
adsl_cmp <- qc_compare_datasets(base = adsl, compare = qc_adsl, keys = "USUBJID")

dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)
capture.output(print(adsl_cmp$diffdf), file = "output/reports/adsl_diffdf.txt")

message("Subjects only in primary: ", adsl_cmp$extra_rows_base,
        " | only in QC: ", adsl_cmp$extra_rows_compare)
print(adsl_cmp$structure)   # obs/vars counts, missing variables, type & label differences
print(adsl_cmp$summary)
print(adsl_cmp$rows |> filter(VARIABLE %in% c("AGEGR1", "AGEGR1N")))
