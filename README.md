# Agentic QC for R, Python & SAS in Positron

Code for the PHUSE 2026 paper *"Agentic QC" for R and Python in Positron* (ET14 & SM10), Phil Bowsher, Posit.

## Contents

- `.posit/assistant/skills/` – the `agentic-qc` skill written for the paper, plus community skills from [RConsortium/pharma-skills](https://github.com/RConsortium/pharma-skills) (km-digitizer, rounding, statistical-reviewer) and the skill shipped with [{commons}](https://posit-dev.github.io/commons/)
- `trusted/` – approved QC functions (the code bank)
- `programs/primary/` – primary programmer (ADSL, ADTTE, Table 14.1.2, Figure 14.2.1). Contains planted defects for the demo.
- `programs/qc/` – QC programs
- `programs/run_qc_logged.R` – runs everything under {logrx}
- `commons_app/` – {commons} QC app
- `metadata/` – CDISC pilot 3 ADaM define.xml (from {metacore})

## Run

```r
source("programs/00_setup.R")         # installs/loads packages
source("programs/run_qc_logged.R")    # runs all primary and QC programs, writes output/ and logs/
shiny::runApp("commons_app")          # optional; needs an LLM provider (Bedrock used in the paper)
```

The km-digitizer example also needs Python with `opencv-python-headless`, `numpy`, `pandas`, `python-docx` and `matplotlib` in `.venv/`.

Data are the public CDISC pilot data from {pharmaversesdtm} and {pharmaverseadam}.
