# SVII Survey (2024)

This repository contains code that does four things:

1) Formats data for imputation.
2) Produces national estimates and state-by-state costs

## Data

This repository uses MCLC data from state specific Google Sheets. Version 4 had manual edits in Excel and replaced total admissions and total population with BJS numbers. Therefore, the most recent version of the data is version 5.  

## Repository Structure 

    |-- code 
      |-- survey
          |-- 03_clean.R               # Format data like 2022 survey data, replace data with BJS numbers
          |-- execute_report.R         # create final results for national report - executes 05_multiple_imputation.R
          |-- 05_multiple_imputation.R 
              |-- sources the following programs in this order: 03_clean.R, MImp1.R, MImp3.R, MImp3.R, Costs.R

## INSTRUCTIONS

In order to successfully produce national estimates and state-by-state costs, survey programs should be run in the following order: 

1) execute_report.R

    - execute_report.R accepts parameters (set to a default for the current survey year) as well as for setting output file name and format.

