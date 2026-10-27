# CAMIS facts behind the tolerance policy

CAMIS = Comparing Analysis Method Implementations in Software (PHUSE DVOST / PSI AIMS).
Stated goal: "demystify conflicting results in statistical analysis methods and
results between primarily SAS, R, and Python". Site: https://psiaims.github.io/CAMIS/

| Topic | SAS default | R default | How to align | CAMIS page |
|---|---|---|---|---|
| Percentiles / quartiles | PROC UNIVARIATE `PCTLDEF=5` | `quantile(type = 7)` | R `quantile(type = 2)` (SAS cannot reproduce R type 7 in UNIVARIATE) | https://psiaims.github.io/CAMIS/Comp/r-sas-summary-stats.html |
| Rounding at .5 | `round()` half away from zero (12.5 -> 13) | `round()` IEC 60559 half to even (12.5 -> 12) | `cards::round5()` (half away); SAS `rounde()` (half even). Both can still differ in rare floating-point edge cases | https://psiaims.github.io/CAMIS/Comp/r-sas_rounding.html |
| Kaplan-Meier / log-rank / Cox | PROC LIFETEST / PHREG defaults (e.g. CI type, ties) | `survival::survfit()` / `coxph()` defaults (e.g. Efron ties) | Match CI transformation (log vs log-log) and ties method explicitly | https://psiaims.github.io/CAMIS/Comp/r-sas_survival.html |

## Policy encoded in `trusted/qc_compare.R`

- Counts and denominators: exact.
- All other numerics: absolute tolerance 1e-3 (`dplyr::near(tol = 1e-3)`). This
  absorbs storage and precision artifacts (a QC dataset stored at 3 d.p., binary
  floating-point representation), e.g. 16.11111 vs 16.112 (|diff| = 8.9e-4).
- Beyond tolerance on a statistic with a documented method difference (p25/p75):
  `CAMIS_METHOD`. The reviewer confirms which definition the SAP/shell requires and
  signs off. This is not a pass.
- Everything else beyond tolerance: `FAIL`.

Tolerance is company policy. CAMIS documents differences but does not prescribe a
numeric tolerance. When a user asks "what is the industry standard tolerance", say
that and show the policy object.
