# SVII Survey (2024)

This repository contains code that does two things:

1) Formats data for imputation.
2) Produces national estimates and state-by-state costs

## Data

This repository uses SVII data collected from states, which was cleaned and reformatted for analysis. 

## Repository Structure 

    |-- code 
      |-- survey
          |-- 03_clean.R               # reformat data for analysis
          |-- execute_report.R         # create final results for national report - executes 05_multiple_imputation.R
          |-- 05_multiple_imputation.R 
              |-- sources the following programs in this order: 03_clean.R, MImp1.R, MImp3.R, MImp3.R, Costs.R

## INSTRUCTIONS

In order to successfully produce national estimates and state-by-state costs simply run the execute_report.R program, which accepts parameters (set to a default for the current survey year) as well as for setting output file name and format.