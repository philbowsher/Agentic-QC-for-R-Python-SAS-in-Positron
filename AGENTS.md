# Agentic QC demo project (PHUSE 2026)

- Purpose: demonstrate AI-assisted ("agentic") QC of CDISC pilot 3 ADaM datasets and TLFs in Positron with Posit Assistant.
- For any QC, double-programming, define.xml, ARD/CAMIS, figure-compare, provenance or logging task, load the `agentic-qc` skill (`.posit/assistant/skills/agentic-qc/`).
- `programs/primary/` is the primary programmer's code. Treat it as read-only. The defects in it are intentional demo material; do not fix them unless asked.
- `trusted/` is the approved code bank and the {commons} semantic layer. Changes there change what gets a green shield, so propose them and do not apply them silently.
- Spec: `metadata/ADaM_define_CDISC_pilot3.xml` (copied from `system.file("extdata", "ADaM_define_CDISC_pilot3.xml", package = "metacore")`).
- Logged runs go through `programs/run_qc_logged.R`, which runs each program in a fresh R process via callr + logrx.
- Python for the km-digitizer skill: `.venv/bin/python` (uv-managed; opencv, python-docx).
