# Reusable construction of the Agentic QC {commons} agent.
# The semantic layer is the SAME trusted code bank used by the Posit Assistant
# QC scripts (../trusted), so a green shield here means the same thing as a
# TRUSTED label in programs/qc/provenance_audit.R.

qc_app_root <- function() {
  root <- getOption("agentic_qc.root")
  if (is.null(root)) {
    root <- if (dir.exists("trusted")) "." else ".."
    options(agentic_qc.root = normalizePath(root))
  }
  getOption("agentic_qc.root")
}

qc_app_data <- function(root = qc_app_root()) {
  source(file.path(root, "trusted", "qc_compare.R"), local = TRUE)
  flat <- function(x) as.data.frame(qc_flatten_ard(x))
  list(
    adsl = readRDS(file.path(root, "data/primary/adsl.rds")),
    qc_adsl = readRDS(file.path(root, "data/qc/qc_adsl.rds")),
    adtte = readRDS(file.path(root, "data/primary/adtte.rds")) |>
      dplyr::select(STUDYID, USUBJID, PARAMCD, PARAM, TRT01A, STARTDT, ADT, AVAL, CNSR,
                    EVNTDESC, SRCDOM, SRCVAR, SAFFL),
    ard_dm_primary = flat(readRDS(file.path(root, "data/primary/ard_dm.rds"))),
    ard_dm_qc = flat(utils::read.csv(file.path(root, "data/qc/ard_dm_qc.csv"), na.strings = ""))
  )
}

build_agent <- function(model = getOption("agentic_qc.model", "us.anthropic.claude-sonnet-4-6")) {
  root <- qc_app_root()
  tables <- qc_app_data(root)

  adam <- commons::data_source(
    adsl = tables$adsl,
    qc_adsl = tables$qc_adsl,
    adtte = tables$adtte,
    ard_dm_primary = tables$ard_dm_primary,
    ard_dm_qc = tables$ard_dm_qc,
    dictionary = file.path(root, "commons_app/dictionaries/adam.yaml")
  )

  commons::commons(
    client = ellmer::chat_aws_bedrock(model = model, echo = "none"),
    data_sources = list(adam = adam),
    semantic_layer = commons::semantic_layer(file.path(root, "trusted")),
    context_layer = commons::context_layer(
      list.files(file.path(root, "commons_app/context"), pattern = "[.]md$", full.names = TRUE)
    ),
    instructions = file.path(root, "commons_app/instructions.md")
  )
}
