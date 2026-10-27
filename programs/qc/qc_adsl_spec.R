# =============================================================================
# Program   : programs/qc/qc_adsl_spec.R
# Purpose   : ACTION 1 -- Data QC. Automated check of the primary ADSL against
#             the study define.xml, rendered as a {pointblank} report.
# Drafted by: Posit Assistant (agentic-qc skill) -- reviewed by QC programmer
# Spec      : metadata/ADaM_define_CDISC_pilot3.xml  (copy of
#             system.file("extdata", "ADaM_define_CDISC_pilot3.xml", package = "metacore"))
# Input     : data/primary/adsl.rds
# Output    : output/reports/adsl_pointblank_report.html
# -----------------------------------------------------------------------------
# Provenance key (see programs/qc/provenance_audit.R):
#   TRUSTED  -- calls the approved code bank in trusted/
#   CITED    -- rule traced to a verified line range in the define.xml
#   REVIEW   -- novel, AI-drafted logic: QC programmer must review in depth
# =============================================================================

library(pointblank)
library(dplyr)
source("trusted/qc_spec.R")

adsl <- readRDS("data/primary/adsl.rds")

# ---- 1. Spec-driven rules: keys, types, lengths, controlled terminology ------
# All generated from the define.xml by the trusted code bank.
spec <- qc_read_spec("ADSL")
agent <- qc_spec_agent(adsl, spec = spec, tbl_name = "adsl (primary)")

# ---- 2. Derivation rules transcribed from define.xml MethodDefs --------------
# cite: metadata/ADaM_define_CDISC_pilot3.xml#L182-L182
agent <- col_vals_not_in_set(agent, columns = ARM, set = "Screen Failure",
  brief = "Structure: Screen Failures are excluded (define.xml)")

# cite: metadata/ADaM_define_CDISC_pilot3.xml#L4926-L4931
agent <- col_vals_expr(agent,
  expr = ~ AGEGR1 == case_when(AGE < 65 ~ "<65", AGE <= 80 ~ "65-80", AGE > 80 ~ ">80"),
  brief = "AGEGR1 derived from AGE: <65 / 65-80 / >80 (MT.ADSL.AGEGR1N)")

# cite: metadata/ADaM_define_CDISC_pilot3.xml#L5161-L5161
agent <- col_vals_expr(agent,
  expr = ~ is.na(TRTSDT) | TRTDURD == as.numeric(TRTEDT - TRTSDT) + 1,
  brief = "TRTDURD = TRTEDT - TRTSDT + 1 (define.xml)")

# cite: metadata/ADaM_define_CDISC_pilot3.xml#L5108-L5108
agent <- col_vals_expr(agent,
  expr = ~ SAFFL == if_else(ITTFL == "Y" & !is.na(TRTSDT), "Y", "N"),
  brief = "SAFFL = Y if ITTFL = Y and TRTSDT not missing (define.xml)")

# ---- 3. Plausibility rule (AI-drafted, no spec backing) ----------------------
agent <- col_vals_between(agent, columns = AGE, left = 18, right = 100,
  brief = "AGE plausible for an adult study (18-100)")

# ---- 4. Interrogate and report -----------------------------------------------
agent <- interrogate(agent, progress = FALSE)

dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)
export_report(agent, filename = "output/reports/adsl_pointblank_report.html", quiet = TRUE)

print(qc_agent_summary(agent) |> filter(status != "PASS"))
message("Spec variables not in this ADSL draft: ",
        paste(attr(agent, "spec_vars_missing"), collapse = ", "))

print(agent)
