---
title: "honupsy: An R package for working with routinely collected Swiss inpatient psychiatric data"
tags:
  - R
  - psychiatry
  - routine data
  - quality measurement
  - HoNOS
  - health services research
authors:
  - name: Michael Ketzer
    orcid: 0000-0000-0000-0000
    corresponding: true
    affiliation: "1"
affiliations:
  - name: University of Basel, Switzerland
    index: 1
date: 25 May 2026
bibliography: paper.bib
---

# Summary

`honupsy` is an R package for working with routinely collected Swiss inpatient psychiatric data. In Swiss inpatient psychiatry, clinics collect standardised quality measurement data coordinated by the Swiss National Association for Quality Development in Hospitals and Clinics (ANQ). These data include information on cases, treatment episodes, HoNOS ratings, patient-reported measures, and coercive measures. The Health of the Nation Outcome Scales (HoNOS) are an internationally used clinician-rated instrument for assessing mental health and social functioning in psychiatric services [@wing1998honos].

`honupsy` provides tools to import, validate, score, and summarise ANQ data directly in R. The package is intended for clinicians, quality officers, data analysts, and health services researchers who work with routinely collected inpatient psychiatric data. It supports reproducible workflows for internal reporting, research, teaching, and methodological development.

# Statement of need

Swiss inpatient psychiatric clinics are required to collect and submit ANQ quality measurement data. This creates a large, standardised routine dataset across Swiss psychiatric care. Despite this standardisation, practical work with the ANQ submission format often depends on manual spreadsheet workflows, local database exports, or clinic-specific scripts. These workflows are difficult to share, test, reproduce, and audit.

This is a problem for psychiatric quality reporting and health services research. Many analyses based on routine data require multiple data preparation steps before any substantive results can be produced. These steps may include importing different record types, checking completeness, handling missing HoNOS items, identifying valid cases, summarising patient composition, and aggregating information at case or unit level. If these steps are performed manually or with undocumented scripts, the resulting analyses are less transparent and harder to reproduce.

`honupsy` addresses this gap by providing an open-source R implementation for the ANQ data workflow. The package reads standard ANQ submission files, separates record types, applies validation checks, computes HoNOS scores, and produces structured summaries. This allows users to keep data preparation, scoring, and reporting inside a script-based R workflow.

The package is relevant beyond a single institution. Since the ANQ programme uses a standardised submission structure, the import and validation tools can support reproducible reporting across Swiss psychiatric clinics. In addition, parts of the package that handle HoNOS scoring, severity classification, and change-score logic are relevant beyond the Swiss context because HoNOS is used internationally in mental health services [@wing1998honos].

The main target use cases are internal clinic reporting, quality monitoring, research on psychiatric outcomes, analyses of coercive measures, and secondary analyses linking routine psychiatric data to staffing, workload, or service characteristics.

# State of the field

General-purpose R packages such as `readr`, `readxl`, `dplyr`, and `tidyr` provide tools for importing, transforming, and summarising tabular data [@r_core_team; @readr; @readxl; @dplyr; @tidyr]. These packages are widely used in reproducible research workflows, but they do not encode the structure of ANQ psychiatric submission files or the scoring rules needed for HoNOS-based reporting.

Generic HoNOS scoring scripts exist in some institutions, but these are often not distributed as tested, documented R packages. They are also usually not connected to the full ANQ submission format. Commercial or internal reporting tools may support clinic-specific reporting, but these tools are commonly closed-source and do not provide transparent, reusable code for research workflows.

`honupsy` fills this gap by combining ANQ-specific import functionality with HoNOS scoring and modular summary functions. The package does not replace general-purpose R tools. Instead, it provides domain-specific functions that can be combined with existing R workflows for data cleaning, modelling, visualisation, and reporting.

# Software design and functionality

`honupsy` is organised around a modular workflow: import, validate, score, and summarise. The package is designed to work with standard ANQ submission files and to return regular R objects that can be inspected, tested, and used in downstream analyses.

The import function `import_anq()` reads standard ANQ submission formats, including pipe-delimited, semicolon-delimited, tab-delimited TXT files, XLSX files, and files following the official ANQ template. It supports both mixed-record files and files split by record type. The imported data are returned as a named list containing the ANQ record types, such as `MB`, `MP`, `PH`, `PB`, and `FM`.

Validation functions check imported records for completeness and plausibility. These checks are intended to identify common data issues before scoring or aggregation. This is particularly important when routine data are exported from clinical information systems and may contain missing values, inconsistent coding, or incomplete records.

HoNOS handling is a core part of the package. `honupsy` computes HoNOS total scores using explicit rules for missing and not-applicable items. This makes the scoring process reproducible and allows users to inspect how incomplete item-level ratings are handled.

The package also includes synthetic data generation through `sample_cases()`. This function allows users to test workflows, teach ANQ data handling, and create reproducible examples without disclosing real patient data.

For reporting, `honupsy` provides modular summary functions. These include `summarise_composition()` for describing case composition, `summarise_honos_severity()` for summarising HoNOS severity, and `summarise_occupancy()` for summarising occupancy-related indicators. This modular family replaces earlier monolithic workflows and allows users to select only the summaries needed for a specific analysis.

# Example

A typical workflow starts by importing an ANQ file, inspecting the available record types, and producing case- or unit-level summaries.

```r
library(honupsy)

path <- system.file(
  "extdata",
  "anq_example.xlsx",
  package = "honupsy"
)

anq <- import_anq(path)

names(anq)
#> [1] "MB" "MP" "PH" "PB" "FM"

head(anq$MB)
head(anq$PH)
head(anq$FM)

composition <- summarise_composition(anq$MB)
honos_severity <- summarise_honos_severity(anq$PH)
occupancy <- summarise_occupancy(anq$MB)

composition
honos_severity
occupancy
```

This example keeps the complete workflow inside R. Users can therefore rerun the same code when data are updated, include it in reproducible reports, or adapt it for institutional reporting and research analyses.

# Research impact statement

`honupsy` supports research and reporting based on routinely collected inpatient psychiatric data. The package makes it easier to use ANQ data in reproducible analyses by replacing manual spreadsheet processing with tested R functions.

The package can support health services research on psychiatric care, including analyses of patient composition, clinical severity, treatment episodes, coercive measures, staffing, workload, and outcomes. It is also suitable for internal quality monitoring because the same scripted workflow can be reused across reporting periods.

By providing synthetic example data and modular summary functions, `honupsy` also supports teaching and methodological development. Users can demonstrate ANQ-style data workflows without exposing patient-level data. This is useful for training analysts, clinicians, and researchers who need to understand how routine psychiatric data are structured and prepared for analysis.

# Limitations and future work

The current version of `honupsy` focuses on importing ANQ data and supporting admission HoNOS workflows. Further development is planned for discharge HoNOS, change-score calculation, and diagnosis groupings.

The package imports `FM` records on coercive measures, but full analysis utilities for coercive measures are still under development. Additional functions for workload and staffing estimation are also planned. These extensions will allow users to link psychiatric routine data more directly to service-level indicators and health workforce analyses.

# AI usage disclosure

Generative AI tools were used to support drafting and editing of this manuscript. All AI-generated text was reviewed and revised by the author. The author verified the package functionality through package documentation, examples, tests, and manual checks. The author takes full responsibility for the content of the software, documentation, and manuscript.

# Acknowledgements

The author acknowledges ANQ for providing the national quality measurement framework for Swiss inpatient psychiatry. The author also acknowledges colleagues and collaborators who contributed to discussions about psychiatric routine data, quality reporting, and reproducible analysis workflows.

The author received no specific funding for this work.

# References
