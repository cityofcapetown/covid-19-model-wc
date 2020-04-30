rm(list = ls())
options(scipen = 999)

# Libraries
library(yaml)
library(readxl)
library(lubridate)
library(dplyr)
library(tibble)

# LOAD PARAMETERS ============================================================
param_distribution <- read_yaml("wc_model_parameters.yaml")
time_varying_assumptions <- read_excel("time_varying_assumptions.xlsx")

# GENERATE SCENARIOS =========================================================
simulation_id <- gsub("[: -]", "" , Sys.time(), perl=TRUE)

simulation_sequence <- seq(from=1, 
                           to=param_distribution$GlobalDetails$Simulations)

simulation_run_ids <- paste(simulation_id, simulation_sequence, sep = "_")

generator_list <- param_distribution$StatisticalParameters
timevarying_list <- param_distribution$TimeVaryingParameters

all_simulation_parameters <- list()
for (run in simulation_run_ids) {
  # Initialise a new simulation instance
  run_parameters <- list()
  
  # Assign an ID
  run_parameters$id <- run
  
  # Copy across global details and artificial constraints flags
  run_parameters$GlobalDetails <- param_distribution$GlobalDetails
  run_parameters$ArtificialConstraints <- param_distribution$ArtificialConstraints
  
  # Generate values for statistical parameters
  for (par_group in names(generator_list)) {
    for (par in names(generator_list[[par_group]])) {
      par_sample <- do.call(get(generator_list[[par_group]][[par]]$generating_function), generator_list[[par_group]][[par]]$args)
      run_parameters[[par_group]][[par]] <- par_sample
    }
  }
  
  # Generate value for time varying parameters
  time_varying_run <- time_varying_assumptions 
  for (par in names(timevarying_list)) {
    time_varying_run[[par]] <-   mapply(function(x,y) rnorm(n = 1, mean = x, sd = y), 
         time_varying_run[[par]], 
         timevarying_list[[par]]$sd_pct_mean * time_varying_run[[par]])
  }
  
  # Generate dates
  date_seq <- seq(ymd(param_distribution$GlobalDetails$StartDate), 
                ymd(param_distribution$GlobalDetails$StartDate) + days(nrow(time_varying_run) - 1), 
                by = param_distribution$GlobalDetails$TimeStepDays)
  
  # Join dates to dataframe and then add to simulation object
  run_parameters$TimeSeries <- time_varying_run %>% mutate(Date = date_seq) %>% select(Date, everything())
  
  # Save to all simulations list
  all_simulation_parameters[[run]] <- run_parameters
  
}

# Remove everything from environment except all_simulation_parameters
rm(list=setdiff(ls(), "all_simulation_parameters"))


# MODEL ===========================================================
start <- Sys.time()

# Simulation parameters
# TODO FOR LOOP THROUGH SIMULATIONS FROM HERE
for (sim_n in seq_along(1:length(all_simulation_parameters))) {
  sim_object <- all_simulation_parameters[[sim_n]]
  model_df <- sim_object$TimeSeries
  
  keep_out_of_df <- ls()
  
  # TODO FOR LOOP THROUGH SINGLE SIMULATON HERE
  
  for (i in seq_along(1:nrow(model_df))) {
  
    # Incrementing variables
    private_susceptible <- ifelse(i == 1, 
                                  sim_object$PopulationDetails[["TotalPopulation"]] * sim_object$PopulationDetails[["PrivateSectorPopulation"]],
                                  private_susceptible - new_infections_private - new_asymptomatic_private)
    private_infected <- ifelse(i == 1, 
                               0,
                               private_infected + imported_not_infectious + new_infections_private - new_infectious_cases_private)
    
    
    private_asymptomatic <- ifelse(i == 1, 
                               0,
                               private_asymptomatic + new_asymptomatic_private - new_infectious_from_asymptomatic_private)
    
    # TODO: What about died?
    private_infectious <- ifelse(i == 1, 
                               0,
                               private_infectious + 
                               imported_not_isolated + 
                               new_infectious_cases_private + 
                               new_infectious_from_asymptomatic_private - 
                               new_hospital_admissions_private - 
                               new_icu_admissions_private - 
                               other_recoveries_private -
                               non_hospital_mortality_private)
      
    private_hospital <- ifelse(i == 1, 
                               0,
                               private_hospital + 
                               new_hospital_admissions_private - 
                               hospital_recoveries_private - 
                               hospital_mortality_private)
      
    private_icu <- ifelse(i == 1, 
                               0,
                               private_icu + new_icu_admissions_private - icu_recoveries_private - icu_mortality_private)
    
      
    private_recovered <- ifelse(i == 1, 
                               0,
                               private_recovered + imported_cases_isolated + hospital_recoveries_private + icu_recoveries_private + other_recoveries_private)
    
      
    private_died <- ifelse(i == 1, 
                               0,
                               private_died + icu_mortality_private + hospital_mortality_private + non_hospital_mortality_private)
    
    public_susceptible <- ifelse(i == 1, 
                                  sim_object$PopulationDetails[["TotalPopulation"]] * (1 - sim_object$PopulationDetails[["PrivateSectorPopulation"]]),
                                  public_susceptible - new_infections_public - new_asymptomatic_public)
    
    public_infected <- ifelse(i == 1, 
                               0,
                               public_infected + new_infections_public - new_infectious_cases_public)
    
    public_asymptomatic <- ifelse(i == 1, 
                               0,
                               public_asymptomatic + new_asymptomatic_public - new_infectious_from_asymptomatic_public)
    
    # TODO: What about died?
    public_infectious <- ifelse(i == 1, 
                               0,
                               public_infectious + 
                               new_infectious_cases_public + 
                               new_infectious_from_asymptomatic_public - 
                               new_hospital_admissions_public - 
                               new_icu_admissions_public - 
                               other_recoveries_public -
                               non_hospital_mortality_public)
      
    public_hospital <- ifelse(i == 1, 
                               0,
                               public_hospital + new_hospital_admissions_public - hospital_recoveries_public - hospital_mortality_public)
      
    public_icu <- ifelse(i == 1, 
                               0,
                               public_icu + new_icu_admissions_public - icu_recoveries_public - icu_mortality_public)
    
      
    public_recovered <- ifelse(i == 1, 
                               0,
                               public_recovered + hospital_recoveries_public + icu_recoveries_public + other_recoveries_public)
    
      
    public_died <- ifelse(i == 1, 
                               0,
                               public_died + icu_mortality_public + hospital_mortality_public + non_hospital_mortality_public)
    
    died <- private_died + public_died  
    
    imports <- ifelse(i == 1, 
               0,
               imports + imported_cases_new - imported_cases_isolated - imported_not_isolated - imported_not_infectious)
      
    imported_cases_new <- model_df$Imports[i]
    
    imported_cases_isolated <- imports * sim_object$ImportedCasesEarlyOn[['pctAlreadyInfectious']] * sim_object$ImportedCasesEarlyOn[['pctInfectiousIsolated']]
    
    imported_not_isolated <- imports * sim_object$ImportedCasesEarlyOn[['pctAlreadyInfectious']] * (1 - sim_object$ImportedCasesEarlyOn[['pctInfectiousIsolated']])
    
    imported_not_infectious <- imports * (1 - sim_object$ImportedCasesEarlyOn[['pctAlreadyInfectious']])
    
    private_total_population <- private_susceptible + 
                                private_infected + 
                                private_asymptomatic + 
                                private_infectious + 
                                private_hospital +
                                private_icu +  
                                private_recovered +  
                                private_died
    
    proportion_infectious_private <- private_infectious / private_total_population
    
    public_total_population <- public_susceptible + 
                                public_infected + 
                                public_asymptomatic + 
                                public_infectious + 
                                public_hospital +
                                public_icu +  
                                public_recovered +  
                                public_died
    
    proportion_infectious_public <- public_infectious / public_total_population
    
    pop_not_in_care_private <- private_susceptible + 
                                private_infected + 
                                private_asymptomatic + 
                                private_infectious + 
                                private_recovered 
    
    pop_not_in_care_public <- public_susceptible + 
                                public_infected + 
                                public_asymptomatic + 
                                public_infectious + 
                                public_recovered 
    
    
    # Why not include died here?
    cumulative_cases_private <- private_infectious + 
                                private_hospital +
                                private_icu +  
                                private_recovered
    
    cumulative_cases_public <-  public_infectious + 
                                public_hospital +
                                public_icu +  
                                public_recovered
    cumulative_cases <- cumulative_cases_private + 
      cumulative_cases_public
    
    
    heterogeniety <- ifelse(sim_object$ArtificialConstraints$ArtificiallyConstrainEstimates == TRUE,
                            1 - sqrt(cumulative_cases / (pop_not_in_care_private + pop_not_in_care_public )) *
                            sim_object$ArtificialConstraints$ConstrainFactor,
                            1)
    
    incidence_rate_private <- proportion_infectious_private * 
      (sim_object$InfectionCharacteristics[["R0"]] *
         (1 - ifelse(
           sim_object$ArtificialConstraints[["ApplyTimeVaryingInterventions"]],
           model_df$InterventionEffectivenessPrivate[i],
           0)) / sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]]) *
      heterogeniety
             
    incidence_rate_public <- proportion_infectious_public * 
      (sim_object$InfectionCharacteristics[["R0"]] *
         (1 - ifelse(
           sim_object$ArtificialConstraints[["ApplyTimeVaryingInterventions"]],
           model_df$InterventionEffectivenessPublic[i],
           0)) / sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]]) *
      heterogeniety
             
    new_infections_private <- private_susceptible * 
                              incidence_rate_private * 
                              sim_object$GlobalDetails$TimeStepDays *
                              (1- sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]])
    
    new_infections_public <- public_susceptible * 
                             incidence_rate_public * 
                             sim_object$GlobalDetails$TimeStepDays *
                             (1- sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]]) +
                             ifelse(model_df$Day[i] < sim_object$ImportedCasesEarlyOn[["HowLongToSeedCasesFor"]],
                                    as.integer(private_infectious/sim_object$ImportedCasesEarlyOn[["RatioImported_CommunityTxm"]]),
                                    0)
    
    
    new_asymptomatic_private <- private_susceptible * 
                                incidence_rate_private * 
                                sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]] *
                                sim_object$InfectionCharacteristics[["R0_RatioInAsymptomatic"]]
    
    
    new_asymptomatic_public <- public_susceptible * 
                               incidence_rate_public * 
                               sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]] *
                               sim_object$InfectionCharacteristics[["R0_RatioInAsymptomatic"]]
    
    total_cases <- private_infected + 
                   private_asymptomatic + 
                   private_infectious + 
                   private_hospital + 
                   private_icu + 
                   public_infected + 
                   public_asymptomatic + 
                   public_infectious + 
                   public_hospital + 
                   public_icu
    
    new_infectious_cases_private <- private_infected * 
                                    (sim_object$GlobalDetails$TimeStepDays /  sim_object$InfectionCharacteristics[["IncubationLatentPeriod"]])
    
    new_infectious_cases_public <- public_infected * 
                                    (sim_object$GlobalDetails$TimeStepDays /  sim_object$InfectionCharacteristics[["IncubationLatentPeriod"]])
    
    new_cases <- new_infectious_cases_private + new_infectious_cases_public
    
    # TODO: verify this formula - does it mean no comunity transmission for private?
    new_diagnoses <- new_infectious_cases_private * model_df$TestingImportedCases[i] + 
                     new_infectious_cases_public * model_df$TestingCommunityCases[i] 
    
    cumulative_cases <- private_infectious + 
                        private_hospital + 
                        private_icu + 
                        private_recovered +
                        public_infectious + 
                        public_hospital + 
                        public_icu + 
                        public_recovered
    
    new_infectious_from_asymptomatic_private <- private_asymptomatic *
                                                (sim_object$GlobalDetails$TimeStepDays / sim_object$InfectionCharacteristics[["IncubationLatentPeriod"]])
    
    new_infectious_from_asymptomatic_public <- public_asymptomatic * 
                                                (sim_object$GlobalDetails$TimeStepDays / sim_object$InfectionCharacteristics[["IncubationLatentPeriod"]])
    
    
    new_hospital_admissions_private <- private_infectious * 
                                       (sim_object$GlobalDetails$TimeStepDays / sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]] ) *
                                       sim_object$MorbidityAndMortalityInSymptomatic[["pctHospitalised"]] *
                                       (1 - sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]])
    
    new_hospital_admissions_public <- public_infectious * 
                                       (sim_object$GlobalDetails$TimeStepDays / sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]] ) *
                                       sim_object$MorbidityAndMortalityInSymptomatic[["pctHospitalised"]] *
                                       (1 - sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]])
    
    new_icu_admissions_private <- private_infectious * 
                                       (sim_object$GlobalDetails$TimeStepDays / sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]]) *
                                       sim_object$MorbidityAndMortalityInSymptomatic[["pctAdmittedToICU"]] *
                                       (1 - sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]])
    
    new_icu_admissions_public <- public_infectious * 
                                       (sim_object$GlobalDetails$TimeStepDays / sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]]) *
                                       sim_object$MorbidityAndMortalityInSymptomatic[["pctAdmittedToICU"]] *
                                       (1 - sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]])
    
    attack_rate <- (private_recovered + public_recovered + private_died + public_died) / 
                   (pop_not_in_care_private + pop_not_in_care_public)
    
    hospital_recoveries_private <- private_hospital * 
                                   (1 - sim_object$MorbidityAndMortalityInSymptomatic[["pctHospitalisedDying"]]) * 
                                   (sim_object$GlobalDetails$TimeStepDays / sim_object$MorbidityAndMortalityInSymptomatic[["LOSGeneralAdmissions"]])
    
    hospital_recoveries_public <- public_hospital * 
                                   (1 - sim_object$MorbidityAndMortalityInSymptomatic[["pctHospitalisedDying"]]) * 
                                   (sim_object$GlobalDetails$TimeStepDays / sim_object$MorbidityAndMortalityInSymptomatic[["LOSGeneralAdmissions"]])
    
    icu_recoveries_private <- private_icu * 
                                   (1 - sim_object$MorbidityAndMortalityInSymptomatic[["pctICUDying"]]) * 
                                   (sim_object$GlobalDetails$TimeStepDays / sim_object$MorbidityAndMortalityInSymptomatic[["LOSICUAverage"]])
    
    icu_recoveries_public <- public_icu * 
                                   (1 - sim_object$MorbidityAndMortalityInSymptomatic[["pctICUDying"]]) * 
                                   (sim_object$GlobalDetails$TimeStepDays / sim_object$MorbidityAndMortalityInSymptomatic[["LOSICUAverage"]])
    
    other_recoveries_private <- private_infectious *
                                (sim_object$GlobalDetails$TimeStepDays / sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]]) *
                                (1 - sim_object$MorbidityAndMortalityInSymptomatic[["pctAdmittedToICU"]] - 
                                     sim_object$MorbidityAndMortalityInSymptomatic[["pctHospitalised"]] -
                                     sim_object$MorbidityAndMortalityInSymptomatic[["pctDeathsNotHospitalised"]])
    
    other_recoveries_public <- public_infectious *
                                (sim_object$GlobalDetails$TimeStepDays / sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]]) *
                                (1 - sim_object$MorbidityAndMortalityInSymptomatic[["pctAdmittedToICU"]] - 
                                     sim_object$MorbidityAndMortalityInSymptomatic[["pctHospitalised"]] -
                                     sim_object$MorbidityAndMortalityInSymptomatic[["pctDeathsNotHospitalised"]])
    
    icu_mortality_private <- private_icu * 
                             sim_object$MorbidityAndMortalityInSymptomatic[["pctICUDying"]] * 
                             (sim_object$GlobalDetails$TimeStepDays / sim_object$MorbidityAndMortalityInSymptomatic[["LOSICUAverage"]])
    
    icu_mortality_public <- public_icu * 
                             sim_object$MorbidityAndMortalityInSymptomatic[["pctICUDying"]] * 
                             (sim_object$GlobalDetails$TimeStepDays / sim_object$MorbidityAndMortalityInSymptomatic[["LOSICUAverage"]])
    
    
    hospital_mortality_private <- private_hospital * 
                             sim_object$MorbidityAndMortalityInSymptomatic[["pctHospitalisedDying"]] * 
                             (sim_object$GlobalDetails$TimeStepDays / sim_object$MorbidityAndMortalityInSymptomatic[["LOSGeneralAdmissions"]])
    
    hospital_mortality_public <- public_hospital * 
                             sim_object$MorbidityAndMortalityInSymptomatic[["pctHospitalisedDying"]] * 
                             sim_object$GlobalDetails$TimeStepDays / 
                             sim_object$MorbidityAndMortalityInSymptomatic[["LOSGeneralAdmissions"]]
    
    
    non_hospital_mortality_private <- private_infectious * 
                             sim_object$MorbidityAndMortalityInSymptomati[["pctDeathsNotHospitalised"]] * 
                             sim_object$GlobalDetails$TimeStepDays / 
                             sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]] * 
                             (1 - sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]])
    
    non_hospital_mortality_public <- public_infectious * 
                             sim_object$MorbidityAndMortalityInSymptomati[["pctDeathsNotHospitalised"]] * 
                             sim_object$GlobalDetails$TimeStepDays / 
                             sim_object$InfectionCharacteristics[["AverageDurationOfInfectiousness"]] * 
                             (1 - sim_object$InfectionCharacteristics[["ProportionAsymptomatic"]])
    
    new_admissions_hospital <- new_hospital_admissions_private + new_hospital_admissions_public
    
    new_admissions_icu <- new_icu_admissions_private + new_icu_admissions_public
    
    icu_beds_occupied <- private_icu + public_icu
    
    icu_recovered <- icu_recoveries_private + icu_recoveries_public
    
    hospital_beds_occupied <- private_hospital + public_hospital
    
    checksum <- (private_susceptible + 
                 private_infected + 
                 private_asymptomatic + 
                 private_infectious + 
                 private_hospital + 
                 private_icu + 
                 private_recovered) +
                (public_susceptible + 
                 public_infected + 
                 public_asymptomatic + 
                 public_infectious + 
                 public_hospital + 
                 public_icu + 
                 public_recovered)
    
    # The forumla fro day 2 onwards is different
    if(i != 1) {
      checksum <- checksum - sum(results_df$imports)
    } 
    
    # TODO: Is this wrong? Conflating imported in time varying figures with private as a population class
    # Alos there is an error in the spreadsheet - the cell lookups are offset - i'e they look into the future
    # I've corrected this in my spreadsheet version
    tested_positive_private <- new_infectious_cases_private * 
                               model_df$TestingImportedCases[i]
    
    tested_positive_public <- new_infectious_cases_public * 
                              model_df$TestingCommunityCases[i]
    
    tested_positive_total <- tested_positive_private + 
                             tested_positive_public
    
    # Make sure you don't create any new vars here or else oyu'll have a really bad time
    if (!exists("results_df")) {
      results_df <- data.frame(mget(setdiff(ls(), append(keep_out_of_df, c("keep_out_of_df", "results_df")))))
    } else {
      results_df <- bind_rows(results_df, 
                              data.frame(mget(setdiff(ls(), append(keep_out_of_df, c("keep_out_of_df", "results_df"))))))
    }
    
  }
  
  # Bind with original timeseries data
  model_df <- bind_cols(model_df, results_df)
  
  # Attach to simulation instance
  all_simulation_parameters[[sim_n]]$model_output <- model_df
}

end <- Sys.time()
difftime(end, start)