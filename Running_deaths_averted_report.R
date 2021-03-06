# Script to calculate number of death as averted from COVID-19 vaccination programs in WHO European Region
# Margaux Mesle - meslem@who.int
# First created: June 2021
# Latest update: November 2021

#To calculate the number of deaths averted by vaccination, we need three sets of information:
#  - Number of deaths that occurred in country, by age group, in given weeks
#  - Vaccination coverage in country, for same age groups, in given weeks
#  - Vaccine effectiveness: first dose=60%, second dose=95%

# Please note: 
# - this code is meant to be run with age-group selection at a time. This includes either one or all three age-groups at one time.
# - code information for both sensitivity analyses have been included at the very end of this script.

  rm( list=ls() )
  library(ggplot2)
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(janitor)
  library(forcats)
  library(plotly)
  library(patchwork)
  library(scales)
  library(AzureStor)
  library(assertthat)
  library(here)
  library(readxl)
  library(ISOweek)
  library(tidyverse)
  library(formattable)
  library(rmarkdown)
  library(cowplot)
  library(zoo)
  
  
  setwd(here::here())

  ##############
  # USER INPUT #
  ##############
  # Set reporting week as the latest week with data available
  reporting.week <- "2021-W45"
  # Name last month of reporting - this is used in figure titles
  current.month <- "November"
  
  # Determine if running analysis or sensitivity analyses
    # If yes, choose fro, the following: yes_VE_low, yes_VE_high, yes_lag_short, yes_lag_long
  sensitivity <- "no"
  # sensitivity <- "yes_VE_low"
  
  # Determine whether interested in single age-group (60-69, 70-79, 80+) or over 60
    # COMMENT AND UNCOMMENT ACCORDINGLY 
  # Vaccination data
  age.group.vax <- c("Age60-69", "Age70-79", "Age80+")  
  #age.group.vax <- c("Age60-69") 
  #age.group.vax <- c("Age70-79") 
  #age.group.vax <- c("Age80+") 
  
  # Mortality data
  age.group <- c("60-69", "70-79", "80+")
  #age.group <- c("60-69")
  #age.group <- c("70-79")
  #age.group <- c("80+")
  
 
  ####################
  # DEFINE FUNCTIONS #
  ####################

  # Source the functions
  source(here::here("Deaths_averted_calculations.R"))
  source(here::here("R", "0_functions.R"))
  source(here::here("R", "clean_vaccination_data.R"))
  source(here::here("R", "plot_country_expected_cases.R"))
  source(here::here("R", "plot_country_vaccination_curves.R"))
  source(here::here("R", "plot_regional_vaccination_curves.R"))
  source(here::here("R", "Create_table_1.R"))
  source(here::here("R", "Create_table_2.R"))
  source(here::here("R", "Create_sensitivity_analyses_summary_table.R"))
  
  
  
  ################################
  # SET THE GRAPHICS ENVIRONMENT #
  ################################
  
  # Define x axis and colours for plotting
  week_breaks=c("2020-51", "2021-01", "2021-05", "2021-10", "2021-15", "2021-20", "2021-25", "2021-30", "2021-35", "2021-40", "2021-45")
  week_labels=c("51", "1\n2021", "5", "10", "15", "20", "25", "30", "35", "40", "45")
  colours =c("#558ed5", "#c3d69b")
  
  
  ####################################
  # SET THE CALCULATIONS ENVIRONMENT #
  ####################################
  
  # Set Vaccine Effectiveness (VE) values: 60% and 95%
  # Is the value for the sensitivity analysis?
  # IF yes: Low values: VE1=0.50 and VE2=0.70
          # High values: VE1=0.70 and VE2=0.975
  if(sensitivity=="no"){
    VE1 = 0.60 # First dose
    VE2 = 0.95 # Second dose
  } 
  
  if(sensitivity=="yes_VE_low") {
    VE1 = 0.50 # First dose
    VE2 = 0.70 # Second dose
  }
  
  if(sensitivity=="yes_VE_high") {
    VE1 = 0.70 # First dose
    VE2 = 0.975 # Second dose
  }
  
  # Set lag weeks used in analysis (representing immune response and reporting delay): 4 and 3
  # For sensitivity timing analysis, replace lag values values above with 
  # IF yes: short values: weeks.lag.1=3 and weeks.lag.2=2
          # Long values: weeks.lag.1=5 and weeks.lag.2=4
  if(sensitivity=="no"){
    weeks.lag.1 = 4 # after first dose
    weeks.lag.2 = 3 # after second dose
  }
  
  if(sensitivity=="yes_lag_short"){
    weeks.lag.1 = 3 # after first dose
    weeks.lag.2 = 2 # after second dose
  }
  
  if(sensitivity=="yes_lag_long"){
    weeks.lag.1 = 5 # after first dose
    weeks.lag.2 = 4 # after second dose
  }
  
  
  # Remove countries without enough data to be used
  # Some countries (Ukraine, Moldova, Romania) only have 60+ age breakdown - remove when looking at single age group
  if(length(age.group.vax)>1) {
  country <- c("Andorra", "Albania", "Armenia", "Azerbaijan", "Belarus", "Bosnia and Herzegovina", "Bulgaria", "Denmark", "Georgia",  
               "Germany", "Kazakhstan", "Kosovo", "Kyrgyzstan", "Netherlands", "San Marino", "Serbia", "Turkey", "Turkmenistan", 
               "United Kingdom (Northern Ireland)", "United Kingdom (Wales)", "Uzbekistan")
  
  } else {
  country <- c("Andorra", "Albania", "Armenia", "Azerbaijan", "Belarus", "Bosnia and Herzegovina", "Bulgaria", "Denmark", "Georgia",  
               "Germany", "Kazakhstan", "Kosovo", "Kyrgyzstan", "Netherlands", "Republic of Moldova", "Romania", "San Marino", "Serbia", 
               "Turkey", "Turkmenistan","United Kingdom (Northern Ireland)", "United Kingdom (Wales)", "Ukraine", "Uzbekistan")
  }
  
  ################
  # READ IN DATA #
  ################  
    # Helper data sets
  Vax_denom <- read.csv("Data/un_vaccine_age_group_denominators_2020_all.csv",header = TRUE, stringsAsFactors = FALSE)
  Vax_denom <- Vax_denom %>%
    mutate(population=ifelse(report_country=="Israel" & targetgroup=="Age60-69", 741639, population),
           population=ifelse(report_country=="Israel" & targetgroup=="Age70-79", 490784, population),
           population=ifelse(report_country=="Israel" & targetgroup=="Age80+", 277034, population))
  
  country_codes <- read.csv("Data/CountryCode.csv", header=T, stringsAsFactors = F)
  
    # data sets
  deaths <- read.csv(paste0("Data/tessy_aggregated_all_weeks_incl_new_age_sex_", reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE)
  vaccine <- read.csv("Data/2021_vaccine_rollout_dataset.csv",header = TRUE, stringsAsFactors = FALSE)
  
  ####################
  # Run calculations #
  ####################
  
  
  # Calculate percentage of deaths aged over 60
  if(length(age.group.vax)>1) {
    deaths.in.over60 <- calculate.percentage.deaths.over60(deaths)
  }
  
  deaths_age <- as.data.frame(calculate.mortality.by.age(deaths))
  vax_clean <- clean.vaccination.data(vaccine, Vax_denom)
  country_list <- select.correct.countries(country, deaths_age, vax_clean) 
  country_summary <- vax_clean %>%  
    # filter(pretty_targetgroup%in%age.group.long) %>%
    filter(report_country%in%country_list$countries)
  if(length(age.group.vax)>1) {
    vax_countries <- find.vaccine.brand.per.country(vaccine, country_summary)
    write.csv(vax_countries, "vaccinations_by_countries.csv")
  }
  
  # Calculate overall full vaccination coverage
  Reg.vax.coverage=vax_clean %>%
    filter(report_country%in% country_list$countries) %>%
    filter(!report_country%in% country) %>%
    #group_by(report_country) %>% # if want country breakdown - uncomment
    summarise(first.dose=round((sum_keep_na(dosefirst)/sum(unique(derived_denominator)))*100,1),
              full.coverage=round((sum_keep_na(dosesecond)/sum(unique(derived_denominator)))*100,1)) 
  First.dose.coverage=Reg.vax.coverage$first.dose
  Full.vax.coverage = Reg.vax.coverage$full.coverage
  
  vax_lim <- restrict.vaccine.data(vax_clean)
  vax_coverage_cnty <- calculate.country.vaccinations(vax_lim)
  vax_coverage_rgn <- calculate.regional.vaccinations(vax_lim)
  

  Expected_cases <- calculate.expected.cases(deaths_age, vax_lim, age.group, country_list, country_summary, VE1, VE2, weeks.lag.1, weeks.lag.2, save.data=TRUE)

  #############
  # Run plots #
  #############
  
  # Generate country specific plots - for age group over 60 only
  if(length(age.group.vax)>1) {
    # Vaccination overview
  country.vaccination.curves <- plot.country.vaccination.curves(vax_coverage_cnty, figure="A")
    ggsave(paste0("Deaths_averted_Fig1A_",reporting.week,".pdf"), width = 12, height = 10, dpi = 1000)
  country.vaccination.curves.age <- plot.country.vaccination.curves(vax_coverage_cnty, figure="B")
    ggsave(paste0("Deaths_averted_Fig1B_",reporting.week,".pdf"), width = 15, height = 10, dpi = 1000)
    # Regional overview
  expected.mortality <- plot.expected.mortality(Expected_cases, country_summary, age.group)
    ggsave(paste0("Deaths_averted_Fig2_",reporting.week,".pdf"), width = 15, height = 10, dpi = 1000)
  }
  
  #############################################################
  # Run tables                                                # 
  # ONLY WHEN ALL CODE FOR SINGLE AGE GROUPS HAVE BEEN RUN!!! #
  #############################################################
  # Read table created above with country level vaccination information
  vax_countries <- read.csv("vaccinations_by_countries.csv", header = TRUE, stringsAsFactors = FALSE)
  
  Table.1 <- create.table1(vax_countries) 
  if(sensitivity=="no") {
    write.csv(Table.1, paste0("Deaths_averted_table1_", reporting.week,".csv"))
  }
  if(sensitivity=="yes_VE_low") {
    write.csv(Table.1, paste0("Deaths_averted_table1_", reporting.week,"_low_VE.csv"))
  }
  if(sensitivity=="yes_VE_high") {
    write.csv(Table.1, paste0("Deaths_averted_table1_", reporting.week,"_high_VE.csv"))
  }
  if(sensitivity=="yes_lag_short") {
    write.csv(Table.1, paste0("Deaths_averted_table1_", reporting.week,"_short_lag.csv"))
  }
  if(sensitivity=="yes_lag_long") {
    write.csv(Table.1, paste0("Deaths_averted_table1_", reporting.week,"_long_lag.csv"))
  }
  
  
  Table.2 <- create.table2(vax_countries) 
  if(sensitivity=="no"){
    write.csv(Table.2, paste0("Deaths_averted_table2_", reporting.week,".csv"))
  }
  if(sensitivity=="yes_VE_low"){
    write.csv(Table.2, paste0("Deaths_averted_table2_", reporting.week,"_low_VE.csv"))
  }
  if(sensitivity=="yes_VE_high"){
    write.csv(Table.2, paste0("Deaths_averted_table2_", reporting.week,"_high_VE.csv"))
  }
  if(sensitivity=="yes_lag_short"){
    write.csv(Table.2, paste0("Deaths_averted_table2_", reporting.week,"_short_lag.csv"))
  }
  if(sensitivity=="yes_lag_long"){
    write.csv(Table.2, paste0("Deaths_averted_table2_", reporting.week,"_long_lag.csv"))
  }
  
  #############################################################
  # Run sensitivity analyses                                  # 
  # ONLY WHEN ALL CODE FOR SINGLE AGE GROUPS HAVE BEEN RUN!!! #
  #############################################################
  # For VE analysis, replace VE values above with 
  
  summary.table <- create.sensitivity.analyses.summary(reporting.week)
  write.csv(summary.table, "Sensitivity_analyses_summary.csv")
  