# =============================================================================
# Program   : programs/primary/adsl.R
# Purpose   : PRIMARY derivation of ADSL (CDISC pilot 3 define.xml)
# Author    : Primary programmer
# Template  : pharmaverse/examples adam/adsl.R (copied to
#             reference/pharmaverse-examples/adsl.R) adapted to the study define
# Input     : pharmaversesdtm::dm, ds, ex, ae, suppdm
# Output    : data/primary/adsl.rds
# -----------------------------------------------------------------------------
# DEMO NOTE : This draft intentionally carries two realistic defects for the
#             Agentic QC demonstration (see README "Planted defects").
# =============================================================================

library(metacore)
library(metatools)
library(pharmaversesdtm)
library(admiral)
library(dplyr)
library(stringr)

# ---- Read SDTM ---------------------------------------------------------------
dm <- convert_blanks_to_na(pharmaversesdtm::dm)
ds <- convert_blanks_to_na(pharmaversesdtm::ds)
ex <- convert_blanks_to_na(pharmaversesdtm::ex)
ae <- convert_blanks_to_na(pharmaversesdtm::ae)
suppdm <- convert_blanks_to_na(pharmaversesdtm::suppdm)

dm_suppdm <- combine_supp(dm, suppdm)

# ---- Study specification (define.xml) ---------------------------------------
metacore <- suppressWarnings(define_to_metacore(
  "metadata/ADaM_define_CDISC_pilot3.xml", verbose = "silent"
)) |>
  select_dataset("ADSL", verbose = "silent")

# ---- Demographics ------------------------------------------------------------
adsl <- dm_suppdm |>
  select(STUDYID, USUBJID, SUBJID, SITEID, ARM, ARMCD, ACTARM, AGE, AGEU,
         SEX, RACE, ETHNIC, DTHFL, RFSTDTC, RFENDTC)

# Age groups per define.xml CL.AGEGR1 (<65, 65-80, >80)
agegr1_lookup <- exprs(
  ~condition,            ~AGEGR1, ~AGEGR1N,
  AGE <= 65,              "<65",        1,
  between(AGE, 66, 80), "65-80",        2,
  AGE > 80,               ">80",        3
)
adsl <- derive_vars_cat(adsl, definition = agegr1_lookup)

adsl <- adsl |>
  create_var_from_codelist(metacore, input_var = RACE, out_var = RACEN, decode_to_code = TRUE)

# ---- Exposure ----------------------------------------------------------------
ex_ext <- ex |>
  derive_vars_dtm(dtc = EXSTDTC, new_vars_prefix = "EXST") |>
  derive_vars_dtm(dtc = EXENDTC, new_vars_prefix = "EXEN", time_imputation = "last")

adsl <- adsl |>
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) |>
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) |>
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) |>
  derive_var_trtdurd() |>
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    false_value = "N",
    missing_value = "N",
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))
  )

# ---- Treatment variables (template logic) ------------------------------------
adsl <- adsl |>
  mutate(
    TRT01P = if_else(ARM %in% c("Screen Failure", "Not Assigned", "Not Treated"), "No Treatment", ARM),
    TRT01A = if_else(ACTARM %in% c("Screen Failure", "Not Assigned", "Not Treated"), "No Treatment", ACTARM),
    TRT01PN = case_when(TRT01P == "Placebo" ~ 0, TRT01P == "Xanomeline Low Dose" ~ 54,
                        TRT01P == "Xanomeline High Dose" ~ 81),
    TRT01AN = case_when(TRT01A == "Placebo" ~ 0, TRT01A == "Xanomeline Low Dose" ~ 54,
                        TRT01A == "Xanomeline High Dose" ~ 81),
    ITTFL = if_else(ARMCD != "Scrnfail", "Y", "N")
  )

# ---- Disposition -------------------------------------------------------------
adsl <- adsl |>
  derive_vars_merged(
    dataset_add = ds,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = DSCAT == "DISPOSITION EVENT",
    new_vars = exprs(EOSSTT = case_when(
      DSDECOD == "COMPLETED" ~ "COMPLETED",
      DSDECOD == "SCREEN FAILURE" ~ NA_character_,
      TRUE ~ "DISCONTINUED"
    )),
    missing_values = exprs(EOSSTT = "ONGOING")
  )

# ---- Finalise ----------------------------------------------------------------
adsl <- adsl |>
  select(STUDYID, USUBJID, SUBJID, SITEID, ARM, TRT01P, TRT01PN, TRT01A, TRT01AN,
         TRTSDT, TRTEDT, TRTDURD, AGE, AGEGR1, AGEGR1N, AGEU, RACE, RACEN, SEX,
         ETHNIC, SAFFL, ITTFL, DTHFL, RFSTDTC, RFENDTC, EOSSTT) |>
  arrange(USUBJID)

dir.create("data/primary", recursive = TRUE, showWarnings = FALSE)
saveRDS(adsl, "data/primary/adsl.rds")
