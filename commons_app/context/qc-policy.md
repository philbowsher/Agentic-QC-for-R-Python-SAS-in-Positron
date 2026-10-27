---
provenance: trusted/qc_compare.R (retrieved 2026-10-23); https://psiaims.github.io/CAMIS/Comp/r-sas-summary-stats.html (retrieved 2026-10-23); https://psiaims.github.io/CAMIS/Comp/r-sas_rounding.html (retrieved 2026-10-23)
---

# Double-programming comparison policy

Counts and denominators (n, N) must match exactly between primary and QC.
All other numeric statistics pass when the absolute difference is at most 0.001;
such differences are treated as floating-point or storage-precision artifacts
(for example 16.11111 vs 16.112). This tolerance is company policy: CAMIS
documents why SAS, R and Python results differ but does not prescribe a tolerance.

A difference beyond tolerance on a statistic with a documented cross-language
method difference is labelled a CAMIS method difference and requires human
sign-off. It is never an automatic pass. Known cases:

- Quantiles: SAS PROC UNIVARIATE default PCTLDEF=5 equals R quantile(type = 2).
  R's default quantile type is 7. SAS cannot reproduce R type 7 in PROC UNIVARIATE.
- Rounding: SAS round() rounds half away from zero (12.5 -> 13); base R round()
  rounds half to even (12.5 -> 12). cards::round5() rounds half away from zero.

Any other difference beyond tolerance is a true discrepancy and must be traced
back to the dataset derivation.
