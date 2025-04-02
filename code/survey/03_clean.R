############################################
# Project:  MCLC Survey (2024)
# File: 03_clean.R
# Last updated: April 1, 2025

# Format final version MCLC survey data
############################################

#set path
sp_data_path <- csgjcr::csg_sp_path(file.path("50 State Revocations Project",
                                              "50 State Survey (2024)"))

#import and clean adm/pop data
adm_pop_analysis <- readRDS(paste0(sp_data_path,"/Data/raw/combined/svii_main.rds")) |>
  select(-c(FY_end, time_period, metric_short, type, metric_abbr, group_cat, state_abbr, state_fips)) |>
  filter(group == "aggregate", 
         metric != "Unknown Parole Violation Admissions",
         metric != "Unknown Parole Violation Population",
         metric != "Unknown Probation Violation Admissions",
         metric != "Unknown Probation Violation Population",
         metric != "New court commitment/Other intake (non-supervision) Admissions",
         metric != "New court commitment/Other intake (non-supervision) Population"
         ) |>
  select(-group) |>
  pivot_wider(names_from = metric, values_from = n) |>
  dplyr::rename(states                                     = state_name,
                total_prison_admissions                    = "Total Prison Admissions",
                total_supervision_violation_admissions     = "Total Supervision Violation Admissions",
                total_technical_violation_admissions       = "Total Technical Violation Admissions",    
                total_new_offense_violation_admissions     = "Total New Offense Violation Admissions",    
                probation_violation_admissions             = "Probation Violation Admissions",             
                technical_probation_violation_admissions   = "Technical Probation Violation Admissions",
                new_offense_probation_violation_admissions = "New Offense Probation Violation Admissions", 
                parole_violation_admissions                = "Parole Violation Admissions",               
                technical_parole_violation_admissions      = "Technical Parole Violation Admissions",
                new_offense_parole_violation_admissions    = "New Offense Parole Violation Admissions",   
         
                total_prison_population                    = "Total Prison Population",   
                total_supervision_violation_population     = "Total Supervision Violation Population",
                total_technical_violation_population       = "Total Technical Violation Population",       
                total_new_offense_violation_population     = "Total New Offense Violation Population",
                probation_violation_population             = "Probation Violation Population",    
                technical_probation_violation_population   = "Technical Probation Violation Population",
                new_offense_probation_violation_population = "New Offense Probation Violation Population", 
                parole_violation_population                = "Parole Violation Population",
                technical_parole_violation_population      = "Technical Parole Violation Population",
                new_offense_parole_violation_population    = "New Offense Parole Violation Population")

################################
# COSTS: read cost data for 2018-2023
################################
#include previous year costs to fill in for missing costs
readin           <- paste0(csgjcr::csg_sp_path(file.path("50 State Revocations Project",
                                                         "50 State Survey (2022)")), 
                           "/Data/mclc_pre_data_2022.xlsx")

costs_old        <- read_xlsx(readin, sheet = "Costs", .name_repair = "universal") %>%
  mutate_at(vars(-c("state")), as.numeric) %>%
  dplyr::rename(Cost.prev4 = year_2019,
                Cost.prev3 = year_2020,
                Cost.prev2 = year_2021)

costs_old$state <- gsub("\\.", " ", costs_old$state)

costs_new            <- readRDS(paste0(sp_data_path,"/Data/raw/combined/svii_cost.rds")) |>
  select(c(state_name, year, n)) |>
  pivot_wider(names_from = year, values_from = n) |>
  dplyr::rename(state = state_name,
                Cost.prev1 = `2022`,
                Cost.now   = `2023`)

costs <- merge(costs_old,costs_new, by = "state", all=T)

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

adm_pop_analysis = upData(adm_pop_analysis, labels = var.labels)

write.xlsx(adm_pop_analysis, file = paste0(sp_data_path, "/Data/analysis/national_estimates/mclc_data_2024.xlsx"))

