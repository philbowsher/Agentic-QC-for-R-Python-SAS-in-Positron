---
name: agentic-qc
description: >-
  Agentic QC for clinical statistical programming (SDTM/ADaM datasets and TLFs) in R.
  Use when the user asks to QC, double-program, validate, independently check,
  compare primary vs QC, run an "automated check", check a dataset against the
  define.xml / spec / controlled terminology, compare ARDs or tables across SAS and R,
  apply CAMIS tolerances, compare figures/plots pixel-by-pixel, audit provenance of
  QC code, or wrap a QC run in a {logrx} log. Covers {metacore}, {pointblank}, {gt},
  {cards}, {diffdf}, {magick}, {logrx}, and the {commons} trust model.
metadata:
  version: "1.0.0"
  author: Phil Bowsher (Posit) -- PHUSE 2026 "Agentic QC for R and Python in Positron"
---

# Agentic QC

You are a **drafting aid for the QC programmer**, not the QC programmer. You write
the automated checks that surface discrepancies *before* formal validation. A human
QC programmer reviews, signs off, and owns every conclusion.

## Hard boundaries

- Never edit anything under `programs/primary/`. QC is independent of primary code.
- Never declare an output "validated", "passed QC" or "GxP compliant". Report
  findings; the human decides.
- Never loosen a tolerance, widen a codelist, or drop a failing rule to make a check
  pass. If a rule looks wrong, say so and ask.
- Never auto-pass a CAMIS method difference. Label it and ask for sign-off.
- Independent double programming must not reuse the primary's derivation approach:
  if the primary uses {admiral}, derive the QC dataset with plain {dplyr}/{tidyr}.
- Use only the data, spec and outputs in this project. No external benchmarks.

## Project layout

| Path | Contents |
|---|---|
| `metadata/ADaM_define_CDISC_pilot3.xml` | Study define.xml (the spec) |
| `trusted/` | **Approved code bank** (green shield). Also the {commons} semantic layer |
| `reference/pharmaverse-examples/` | Pinned copies of pharmaverse/examples templates (citable) |
| `programs/primary/` | Primary programmer's code (read-only for you) |
| `programs/qc/` | Your QC scripts |
| `data/primary/`, `data/qc/` | Datasets and ARDs |
| `output/` | Reports, tables, figures; `logs/` for {logrx} logs |

## Rule 1 -- Trusted code first, and say which is which

Before writing any QC logic, check `trusted/` for a function that already does it and
use it. Current trusted functions:

| Task | Trusted function (file) |
|---|---|
| Read define.xml -> metacore | `qc_read_spec()`, `qc_spec_table()` (`trusted/qc_spec.R`) |
| Spec-driven pointblank agent | `qc_spec_agent()`, `qc_agent_summary()` |
| Dataset compare (PROC COMPARE analogue) | `qc_compare_datasets()` (`trusted/qc_compare.R`) |
| ARD compare under CAMIS policy | `qc_compare_ard()`, `qc_gt_ard_comparison()`, `qc_tolerance_policy` |
| Standard KM figure + device | `tlf_km_plot()`, `tlf_save_png()` (`trusted/qc_figures.R`) |
| Bitmap figure compare | `qc_compare_figures()` |
| define.xml -> data-dict.yaml, Parquet, CLI | `qc_datadict_table()`, `qc_write_datadict()`, `qc_write_parquet()`, `qc_datadict_cli()` (`trusted/qc_datadict.R`) |
| Provenance audit | `qc_audit_provenance()`, `qc_gt_provenance()` (`trusted/qc_provenance.R`) |

Every QC script you write must:

1. Start with a header block: Program, Purpose, `Drafted by: Posit Assistant
   (agentic-qc skill) -- reviewed by QC programmer`, Input, Output.
2. `source()` the trusted files it uses.
3. Put a `# cite: <target>` comment directly above any rule transcribed from the
   spec or an approved reference. Targets are either a repo file with a line range
   (`metadata/ADaM_define_CDISC_pilot3.xml#L4926-L4931`) or a URL on the approved list
   (CAMIS, pharmaverse, CDISC). Find line numbers with `grep -n`, never guess them.
4. Leave novel logic uncited. It will be labelled REVIEW, which is the honest label.

After drafting, run `programs/qc/provenance_audit.R` (or `qc_audit_provenance()` on
the new script) and show the user the gt table. **You never assign the labels
yourself**: TRUSTED / CITED / REVIEW / setup come deterministically from the call
graph and verified citations, the same principle {commons} uses for its green shield,
blue citation and yellow warning markers.

## Action 1 -- Data QC against the define.xml

The define.xml ships with **{metacore}**, not {pharmaverseadam}:

```r
system.file("extdata", "ADaM_define_CDISC_pilot3.xml", package = "metacore")
# same as metacore::metacore_example("ADaM_define_CDISC_pilot3.xml")
# project copy: metadata/ADaM_define_CDISC_pilot3.xml
```

Read it with `metacore::define_to_metacore()` (there is **no** `metacore_file()`), then
`select_dataset("ADSL")`. `qc_read_spec()` does both. Then:

```r
source("trusted/qc_spec.R")
spec  <- qc_read_spec("ADSL")
agent <- qc_spec_agent(adsl, spec = spec)        # keys, types, lengths, codelists
# cite: metadata/ADaM_define_CDISC_pilot3.xml#L4926-L4931
agent <- pointblank::col_vals_expr(agent,
  expr = ~ AGEGR1 == dplyr::case_when(AGE < 65 ~ "<65", AGE <= 80 ~ "65-80", AGE > 80 ~ ">80"),
  brief = "AGEGR1 derived from AGE (MT.ADSL.AGEGR1N)")
agent <- pointblank::interrogate(agent, progress = FALSE)
pointblank::export_report(agent, "output/reports/adsl_pointblank_report.html")
agent   # prints the interactive report to the Viewer
```

`qc_spec_agent()` maps the spec onto pointblank:

| define.xml | pointblank step |
|---|---|
| Key variables (`key_seq`), STUDYID, USUBJID | `col_vals_not_null()` + `rows_distinct()` |
| `DataType` text / integer / float | `col_is_character()` / `col_is_numeric()` |
| `DisplayFormat` DATE9. | `col_is_date()` |
| `Length` (text) | `col_vals_expr(nchar(x) <= length)` |
| `CodeListRef` | `col_vals_in_set(set = codelist codes)` |

The spec only covers types, lengths and codelists. **Derivation rules** (MethodDefs:
`AGEGR1N`, `TRTDURD`, `SAFFL`, `ITTFL`, dataset structure "Screen Failures are
excluded") must be transcribed by you as `col_vals_expr()` / `col_vals_not_in_set()`
steps, each with a `# cite:` to the MethodDef lines.

Gotchas (tested in this environment):

- `define_to_metacore()` emits parsing warnings for the pilot 3 define; they are
  expected. `qc_read_spec()` silences them.
- In this define every `mandatory` is `FALSE`. Do not infer required variables from
  it; use the keys plus the variables the dataset actually implements.
  `attr(agent, "spec_vars_missing")` lists spec variables not yet derived. Report
  them as scope, not as failures.
- Dates are `integer` + `DATE9.` in the define but `Date` in R. Check `format`, not `type`.
- `col_vals_in_set()` counts `NA` as a failure. That is the right behaviour for
  flags and codelisted variables.
- Numeric codelists (`CL.ARMN`, `CL.RACEN`, `CL.AGEGR1N`) store codes as character.
  Coerce with `as.numeric()` before `col_vals_in_set()` (the trusted agent does this).
- `ETHNIC` codelist in this define has the text in `code` and a number in `decode`.

### Action 1b -- define.xml as a polyglot data contract (data-dict)

`programs/qc/qc_adsl_datadict.R` uses `trusted/qc_datadict.R` to translate the
define.xml into `metadata/datadict/adsl.yaml` (data-dict.yaml format,
https://data-dict.tidyverse.org). The `data-dict` CLI (Rust, installed with
`uv tool install data-dict-yaml`, binary at `~/.local/bin/data-dict`) then checks it at
three levels: `validate-spec`, `validate-meta` (names and types) and `validate-data`
(values, with `--html` for a report). `render` writes the HTML dictionary, and
`translate` emits each rule as SQL (DuckDB) and R (tidyverse / base / data.table).
Python (polars) is not available in 0.0.3.

Gotchas: data-dict validates **Parquet only**. Write it with `qc_write_parquet()`
(nanoparquet). DuckDB's writer only sets the legacy DATE converted type, which
data-dict reads as a number. There is no date - date arithmetic in data-dict
expressions, so TRTDURD stays a pointblank rule. `examples` and `range` are
descriptive and never validated. Enforceable rules are `values`, `constraints` and `assert`.
The same YAML format is what {commons} reads for `data_source(dictionary = ...)`.

Double programming of the dataset: derive independently (see Hard boundaries), then
`qc_compare_datasets(base = primary, compare = qc, keys = "USUBJID")`. Report extra
rows on each side and per-variable differences, with subject-level drill-down.

## Action 2A -- TLF QC: ARD comparison with CAMIS tolerances

ARD shape (one row per statistic): `group1, group1_level, variable, variable_level,
stat_name, stat`. {cards} ARDs are flattened by `qc_flatten_ard()`. A QC ARD must
also contain zero-count cells (`tidyr::complete()`) and the treatment big-N rows
(`variable = "TRT01A"`, `group1 = NA`), or they show as MISSING_QC.

```r
source("trusted/qc_compare.R")
cmp <- qc_compare_ard(primary = ard_primary, qc = ard_qc)
qc_gt_ard_comparison(cmp, show = "review")   # FAIL / MISSING / CAMIS rows only
```

Status rules (`qc_tolerance_policy`). Do not change them in a QC script:

| Status | Rule |
|---|---|
| MATCH | `abs(diff) < 1e-12` |
| FAIL | counts/denominators (`n`, `N`, ...) differ at all |
| PASS_PRECISION | `dplyr::near(primary, qc, tol = 1e-3)`, e.g. 16.11111 vs 16.112 |
| CAMIS_METHOD | beyond tolerance **and** a documented SAS/R method difference (quantiles) |
| FAIL | anything else beyond tolerance |

See `references/camis-tolerances.md` for the CAMIS facts behind each rule. Key
points to explain to the user:

- CAMIS (psiaims.github.io/CAMIS) explains *why* SAS, R and Python differ. It does
  **not** set a tolerance. 1e-3 is company policy; say so.
- Quantiles: SAS PROC UNIVARIATE default `PCTLDEF=5` equals R `quantile(type = 2)`.
  R's default is `type = 7`. `cards::continuous_summary_fns()` uses `type = 2`, so a
  {cards} ARD matches SAS defaults.
- Rounding: base R `round()` rounds half to even, SAS `round()` rounds half away from
  zero (12.5 -> 12 vs 13). For display, use `cards::round5()` or a validated
  half-away helper, and round once at display. Hand display-rounding audits to the
  **rounding** skill.
- Always trace a FAIL back to the dataset level (e.g. AGEGR1 count differences ->
  ADSL AGEGR1 derivation) before reporting it.

## Action 2B -- Figure QC (bitmap to bitmap)

Pixel comparison is only meaningful when both figures come from the same renderer
and device. Always render with `tlf_km_plot()` + `tlf_save_png()` on both sides.

```r
source("trusted/qc_figures.R")
res <- qc_compare_figures("output/figures/f_primary.png", "output/figures/f_qc.png",
                          out_png = "output/figures/f_diff.png", fuzz = 5)
res$pixels_different; magick::image_read(res$composite)
```

The composite is PRIMARY | QC | differences in red. A non-zero pixel count is a
signal, not a verdict: always follow it with a subject-level drill-down (AVAL/CNSR
primary vs QC) that explains the visual difference.

## Accountability -- provenance and logging

- Provenance: `source("programs/qc/provenance_audit.R")` writes
  `output/reports/provenance_<script>.html` and returns a roll-up of trusted / cited /
  review lines per script.
- {commons}: `commons_app/` exposes the same `trusted/` functions as commons
  *measures*. Answers that run a measure display the green shield. Novel analysis
  displays the yellow warning. To change what is trusted, edit `trusted/` and get it
  reviewed. Never edit the app to relabel.
- Logging: run approved QC scripts through `programs/run_qc_logged.R`, which wraps
  each one in `logrx::axecute()` and writes `logs/<script>.log` (session info,
  package versions, masked functions, errors, warnings, and a hash of the script).

## Related skills

| Skill | Use for |
|---|---|
| `rounding` | Display-precision and SAS-compatible rounding audit of TLF code |
| `statistical-reviewer` | Macro-level audit across SDTM -> ADaM -> TLF (population and denominator consistency) |
| `km-digitizer` | Reconstruct IPD from a KM image (legacy or competitor figure) and overlay |
| `commons` | Building or changing the {commons} QC agent in `commons_app/` |

## Reporting back to the user

End every QC run with a short findings table: Finding | Evidence (rule/step,
subjects) | Likely root cause | Severity (Critical / Major / Minor / Info) |
Owner (primary / QC / spec). Separate **true discrepancies** from **explainable
differences** (CAMIS, precision). List what was **not** checked.
