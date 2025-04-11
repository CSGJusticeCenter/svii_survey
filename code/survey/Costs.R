################################################################################
#COSTS
#
#Take state-level estimates and calculate costs
################################################################################

#just for state-level cost report
#all aggregations except overall
forreport           <- national.est[,grepl(paste0("population",last(refyear)), names(national.est))] %>% select(-c(starts_with("overall"),contains("_probation_"),contains("_parole_")))
#remove year from column names
colnames(forreport) <-gsub(last(year), "", colnames(forreport))

#use non-imputed, original, state-reported data for calculating costs by state
forreport.state <- adm_pop_analysis_with_bjs[,c("states","year",
                                                "probation_violation_population",
                                                "parole_violation_population",
                                                "technical_parole_violation_population",
                                                "technical_probation_violation_population")] %>%
  filter(year==as.numeric(last(refyear))) %>%
  dplyr::rename(
    probation_violation_population.NA           = probation_violation_population,
    parole_violation_population.NA              = parole_violation_population,
    technical_parole_violation_population.NA    = technical_parole_violation_population,
    technical_probation_violation_population.NA = technical_probation_violation_population
  ) %>%
  arrange(states) %>%
  column_to_rownames(var="states") %>%
  select(-c(year))


cost.final <- costs %>%
  mutate(
    Cost.prev3 = case_when(is.na(Cost.prev3) ~ Cost.prev4,
                           TRUE ~ Cost.prev3),
    Cost.prev2 = case_when(is.na(Cost.prev2) ~ Cost.prev3,
                           TRUE ~ Cost.prev2),
    Cost.prev1 = case_when(is.na(Cost.prev1) ~ Cost.prev2,
                             TRUE ~ Cost.prev1),
    Cost.now = case_when(is.na(Cost.now) ~ Cost.prev1,
                             TRUE ~ Cost.now)
  ) %>% 
  bind_cols(forreport,forreport.state) %>%
  mutate(
    #national estimated costs (most recent year)
    cost.pr.vp    = (Cost.now*365*probation_violation_population),
    cost.pa.vp    = (Cost.now*365*parole_violation_population),
    cost.tvp      = (Cost.now*365*total_technical_violation_population),
    cost.novp     = (Cost.now*365*total_new_offense_violation_population),
    #national estimated costs (previous year)
    cost.pr.vp22    = (Cost.prev1*365*probation_violation_population),
    cost.pa.vp22    = (Cost.prev1*365*parole_violation_population),
    cost.tvp22      = (Cost.prev1*365*total_technical_violation_population),
    cost.novp22     = (Cost.prev1*365*total_new_offense_violation_population),    
    
    #original state-reported costs (most recent year)
    cost.pr.state    = as.numeric(Cost.now*365*probation_violation_population.NA),
    cost.pa.state    = as.numeric(Cost.now*365*parole_violation_population.NA),
    cost.tpa.state   = as.numeric(Cost.now*365*technical_parole_violation_population.NA),
    cost.tpr.state   = as.numeric(Cost.now*365*technical_probation_violation_population.NA),
    #original state-reported costs (previous year)
    cost.pr.state22    = as.numeric(Cost.prev1*365*probation_violation_population.NA),
    cost.pa.state22    = as.numeric(Cost.prev1*365*parole_violation_population.NA),
    cost.tpa.state22   = as.numeric(Cost.prev1*365*technical_parole_violation_population.NA),
    cost.tpr.state22   = as.numeric(Cost.prev1*365*technical_probation_violation_population.NA),
    ) %>%
  dplyr::rename(State=state) 

##Annual costs for incarceration nationally (most recent year)
total.pr.vp     <- sum(cost.final$cost.pr.vp)
total.pa.vp     <- sum(cost.final$cost.pa.vp)
total.tvp       <- sum(cost.final$cost.tvp)
total.novp      <- sum(cost.final$cost.novp)
##Annual costs for incarceration nationally (previous year)
total.pr.vp22   <- sum(cost.final$cost.pr.vp22)
total.pa.vp22   <- sum(cost.final$cost.pa.vp22)
total.tvp22     <- sum(cost.final$cost.tvp22)
total.novp22    <- sum(cost.final$cost.novp22)
