# =============================================================================
# Program   : programs/qc/qc_adsl_datadict.R
# Purpose   : ACTION 1b -- Data contract. Generate a data-dict.yaml from the
#             define.xml, then validate primary and QC ADSL against it with the
#             language-agnostic data-dict CLI (Rust; same check from R, Python
#             or SQL), render the HTML data dictionary, and translate one rule
#             into R / Python / SQL.
# Drafted by: Posit Assistant (agentic-qc skill) -- reviewed by QC programmer
# Input     : data/primary/adsl.rds, data/qc/qc_adsl.rds, define.xml
# Output    : data/parquet/*.parquet, metadata/datadict/adsl.yaml,
#             output/reports/adsl_datadict.html, output/reports/adsl_datadict_validation.html
# =============================================================================

source("trusted/qc_spec.R")
source("trusted/qc_datadict.R")

spec <- qc_read_spec("ADSL")
adsl <- readRDS("data/primary/adsl.rds")
qc_adsl <- readRDS("data/qc/qc_adsl.rds")

# ---- Parquet hand-off (data-dict validates Parquet) ----------------------------
qc_write_parquet(adsl, "data/parquet/adsl.parquet")
qc_write_parquet(qc_adsl, "data/parquet/qc_adsl.parquet")

# ---- define.xml -> data-dict.yaml ----------------------------------------------
# Paths inside the dictionary are relative to the dictionary file.
dict_path <- "metadata/datadict/adsl.yaml"
qc_write_datadict(
  tables = list(
    qc_datadict_table(adsl, spec, "adsl", parquet = "../../data/parquet/adsl.parquet",
                      origin = "../../programs/primary/adsl.R",
                      description = "PRIMARY ADSL. One record per subject. Screen Failures are excluded."),
    qc_datadict_table(qc_adsl, spec, "qc_adsl", parquet = "../../data/parquet/qc_adsl.parquet",
                      origin = "../../programs/qc/qc_adsl.R",
                      description = "QC (double-programmed) ADSL. One record per subject. Screen Failures are excluded.")
  ),
  path = dict_path
)

# ---- Validate: spec -> metadata (names/types) -> data (values) ------------------
qc_datadict_cli("validate-spec", dict_path)
qc_datadict_cli("validate-meta", dict_path)
dd_validation <- qc_datadict_cli("validate-data", dict_path,
                                 "--html", "output/reports/adsl_datadict_validation.html")
cat(dd_validation, sep = "\n")

# ---- Human-readable data dictionary (HTML) -------------------------------------
qc_datadict_cli("render", dict_path, "-o", "output/reports/adsl_datadict.html")

# ---- One rule, three languages -------------------------------------------------
dd_translate <- jsonlite::fromJSON(paste(qc_datadict_cli(
  "translate", dict_path, "--table", "adsl",
  "--expr", "AGEGR1 = CASE WHEN AGE < 65 THEN '<65' WHEN AGE <= 80 THEN '65-80' ELSE '>80' END"
), collapse = "\n"))
print(dd_translate$translations[[1]][, c("target", "code")])
