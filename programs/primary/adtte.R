# =============================================================================
# Program   : programs/primary/adtte.R
# Purpose   : PRIMARY derivation of ADTTE, PARAMCD = TTDE
#             (Time to First Dermatologic Event; CDISC pilot 3 ADTTE)
# Template  : {admiral} derive_param_tte() pattern (pharmaverse/examples adam/adtte.R)
# Input     : data/primary/adsl.rds, pharmaverseadam::adae
# Output    : data/primary/adtte.rds
# =============================================================================

library(admiral)
library(dplyr)
library(stringr)

adsl <- readRDS("data/primary/adsl.rds") |> filter(SAFFL == "Y")
adae <- pharmaverseadam::adae

# Event: first treatment-emergent dermatologic AE
derm_event <- event_source(
  dataset_name = "adae",
  filter = TRTEMFL == "Y" &
    (AESOC == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" | str_detect(AEDECOD, "APPLICATION SITE")),
  date = ASTDT,
  order = exprs(AESEQ),
  set_values_to = exprs(EVNTDESC = "Dermatologic event", SRCDOM = "ADAE", SRCVAR = "ASTDT", SRCSEQ = AESEQ)
)

# Censor: end of treatment
eot_censor <- censor_source(
  dataset_name = "adsl",
  date = TRTEDT,
  set_values_to = exprs(EVNTDESC = "End of treatment", SRCDOM = "ADSL", SRCVAR = "TRTEDT")
)

adtte <- derive_param_tte(
  dataset_adsl = adsl,
  start_date = TRTSDT,
  event_conditions = list(derm_event),
  censor_conditions = list(eot_censor),
  source_datasets = list(adsl = adsl, adae = adae),
  set_values_to = exprs(PARAMCD = "TTDE", PARAM = "Time to First Dermatologic Event (days)")
) |>
  derive_vars_duration(new_var = AVAL, start_date = STARTDT, end_date = ADT) |>
  derive_vars_merged(
    dataset_add = adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(TRT01A, TRT01AN, AGE, SEX, SAFFL)
  ) |>
  arrange(USUBJID)

saveRDS(adtte, "data/primary/adtte.rds")
