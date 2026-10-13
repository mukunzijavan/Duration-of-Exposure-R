# Duration-of-Exposure-R
## Overview
This project demonstrates an R workflow for a safety-analysis population:
- Selecting the Safety Analysis Set (`SAFFL == "Y"`)
- Replacing source treatment names with **Treatment A** and **Treatment B**
- Calculating exposure interval duration
- Estimating participant-level duration after subtracting gaps between recorded exposure intervals
- Summarising duration and producing a `gt` table
- Exporting review datasets and running basic quality-control checks

## Input data
Place the following files in `data/`:
- `adsl.sas7bdat`: expected to contain `USUBJID`, `SAFFL`, and `TRT01AN`
- `adex.sas7bdat`: expected to contain `USUBJID`, `EXSTDTC`, and `EXENDTC`

 ## Libraries(Packages)
library(tidyverse)
library(haven)        
library(gt)           
library(docorator)


## Method notes
- Exposure-interval duration counts both the start and end dates and uses a 30-day month for the interval summary.
- Participant-level duration spans the first recorded exposure start through the last recorded exposure end, then subtracts positive gaps between intervals. It uses the average Gregorian month length (`365.25 / 12`) for conversion.
- The interval summary counts exposure records; the participant-level summary counts participants. They answer different questions and should not be interpreted as interchangeable.
- The approach assumes records represent non-overlapping intervals. Overlapping or duplicate records should be reviewed because simple gap subtraction does not resolve all possible data anomalies.
- Final study reporting conventions must follow the protocol, Statistical Analysis Plan (SAP), and ADaM specifications.

## Privacy
Do not publish real patient-level data, subject identifiers, sponsor names, protocol identifiers, or confidential study metadata. Use synthetic data and generic treatment labels in public repositories.
