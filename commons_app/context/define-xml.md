---
provenance: metadata/ADaM_define_CDISC_pilot3.xml (copy of system.file("extdata", "ADaM_define_CDISC_pilot3.xml", package = "metacore"), retrieved 2026-10-23)
---

# Study specification (define.xml) rules used in QC

- ADSL structure: one record per subject. Screen Failures are excluded.
- ARM, TRT01P, TRT01A codelist CL.ARM: Placebo, Xanomeline Low Dose, Xanomeline High Dose.
  TRT01PN/TRT01AN codelist CL.ARMN: 0, 54, 81.
- AGEGR1N algorithm: 1 if AGE < 65; 2 if AGE 65-80; 3 if AGE > 80. AGEGR1 is the
  character decode (<65, 65-80, >80).
- TRTDURD = TRTEDT - TRTSDT + 1.
- SAFFL = Y if ITTFL = 'Y' and TRTSDT is not missing, N otherwise.
