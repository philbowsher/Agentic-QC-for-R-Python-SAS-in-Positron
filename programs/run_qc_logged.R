# =============================================================================
# Program   : programs/run_qc_logged.R
# Purpose   : Re-run the full primary + QC pipeline, each program wrapped in
#             logrx::axecute() to leave a reproducible audit trail in logs/.
#             Each run is also checked against the company approved-package
#             list, so any function from an unapproved package is reported.
# Output    : logs/<program>.log + logs/<program>.Rds, logs/approved.rds
# =============================================================================

library(logrx)

# ---- Approved packages (company validated list) --------------------------------
approved_pkgs <- list(
  base = "All", stats = "All", utils = "All", methods = "All", tools = "All",
  admiral = "All", metacore = "All", metatools = "All", pharmaversesdtm = "All",
  pharmaverseadam = "All", xportr = "All", cards = "All",
  dplyr = "All", tidyr = "All", stringr = "All", tibble = "All", purrr = "All",
  rlang = "All", lubridate = "All",
  pointblank = "All", gt = "All", diffdf = "All", magick = "All",
  ggplot2 = "All", ggsurvfit = "All", survival = "All",
  DBI = "All", fontawesome = "All", logrx = "All",
  nanoparquet = "All", yaml = "All", jsonlite = "All"
)
dir.create("logs", showWarnings = FALSE)
approved <- build_approved(approved_pkgs)

# The trusted code bank is sourced, so its functions live in .GlobalEnv.
# Approve exactly those names; any other ad-hoc function is reported.
source("trusted/qc_provenance.R")
approved <- rbind(approved, data.frame(
  function_name = qc_trusted_functions("trusted"),
  library = ".GlobalEnv"
))
saveRDS(approved, "logs/approved.rds")
options(log.rx.approved = "logs/approved.rds")

pipeline <- c(
  # Primary programmer
  "programs/primary/adsl.R",
  "programs/primary/adtte.R",
  "programs/primary/t_dm.R",
  "programs/primary/f_km_ttde.R",
  # Agentic QC (Posit Assistant drafted, QC programmer approved)
  "programs/qc/qc_adsl_spec.R",
  "programs/qc/qc_adsl.R",
  "programs/qc/qc_adsl_datadict.R",
  "programs/qc/qc_t_dm.R",
  "programs/qc/qc_f_km_ttde.R",
  "programs/qc/provenance_audit.R"
)

# Each program runs in a fresh R process (batch-submission semantics), so no
# state from the interactive session -- attached packages, options, open
# graphics devices -- can leak into a logged, reproducible run.
for (prog in pipeline) {
  callr::r(
    function(prog, root) {
      setwd(root)
      # Scripts that end by printing a plot would otherwise create Rplots.pdf
      options(log.rx.approved = "logs/approved.rds", device = function(...) grDevices::pdf(NULL))
      logrx::axecute(
        file = prog,
        log_name = sub("[.]R$", ".log", basename(prog)),
        log_path = "logs",
        include_rds = TRUE,
        quit_on_error = FALSE,
        show_repo_url = FALSE
      )
    },
    args = list(prog = prog, root = getwd())
  )
}

# ---- Audit-trail summary ------------------------------------------------------
run_summary <- do.call(rbind, lapply(pipeline, \(prog) {
  lg <- readRDS(file.path("logs", sub("[.]R$", ".Rds", basename(prog))))
  unapproved <- lg$unapproved_packages_functions
  data.frame(
    program = prog,
    errors = length(lg$errors),
    warnings = length(lg$warnings),
    unapproved_functions = if (is.null(unapproved)) 0L else NROW(unapproved),
    run_time = format(lg$run_time),
    hash_sum = substr(lg$hash_sum, 1, 12)
  )
}))
run_summary
