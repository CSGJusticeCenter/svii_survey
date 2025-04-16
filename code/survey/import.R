############################################
# Project:  SVII Survey (2024)
# File: import.R
# Last updated: April 16, 2025

# Format final version MCLC survey data
############################################

#set path
sp_data_path <- csgjcr::csg_sp_path(file.path("50 State Revocations Project",
                                              "50 State Survey (2024)"))

#import adm/pop data
adm_pop_analysis_with_bjs <- readRDS(paste0(sp_data_path,"/Data/raw/combined/svii_main_agg_wide.rds")) %>%
  dplyr::rename(states = state_name)

#import costs
costs <- readRDS(paste0(sp_data_path,"/Data/raw/combined/svii_cost_wide.rds")) %>%
  dplyr::rename(state = state_name)

# add labels
var.labels = c(states                                     = "State name",
               year                                       = "Year",
               total_prison_admissions                    = "Total admissions", overall_admissions = "Overall admissions",
               total_supervision_violation_admissions     = "Total probation and parole violation admissions",
               probation_violation_admissions             = "Total probation violation admissions",
               parole_violation_admissions                = "Total parole violation admissions",
               total_technical_violation_admissions       = "Total technical violation admissions",
               technical_probation_violation_admissions   = "Admissions for technical violations, probation",
               technical_parole_violation_admissions      = "Admissions for technical violations, parole",
               total_new_offense_violation_admissions     = "Total new offense violation admissions",
               new_offense_probation_violation_admissions = "Admissions for new crime violations, probation",
               new_offense_parole_violation_admissions    = "Admissions for new crime violations, parole",

               total_prison_population                    = "Total population", overall_population = "Overall population",
               total_supervision_violation_population     = "Total probation and parole violation population",
               probation_violation_population             = "Total probation violation population",
               parole_violation_population                = "Total parole violation population",
               total_technical_violation_population       = "Total technical violation population",
               technical_probation_violation_population   = "Technical violator population, probation",
               technical_parole_violation_population      = "Technical violator population, parole",
               total_new_offense_violation_population     = "Total new offense violation population",
               new_offense_probation_violation_population = "New crime violator population, probation",
               new_offense_parole_violation_population    = "New crime violator population, parole"
               )

adm_pop_analysis_with_bjs = upData(adm_pop_analysis_with_bjs, labels = var.labels)

write.xlsx(adm_pop_analysis_with_bjs, file = paste0(sp_data_path, "/Data/analysis/national_estimates/mclc_data_2024_handlingmissings_allyears.xlsx"))
