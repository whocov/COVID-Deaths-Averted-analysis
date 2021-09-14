# Script to calculate number of death as averted from COVID-19 vaccination programs in WHO European Region
# Margaux Mesle - meslem@who.int
# First created: June 2021
# Lastest update: September 2021

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
  reporting.week <- "2021-W35"
  
  # Determine whether interested in single age-group (60-69, 70-79, 80+) or over 60
    # COMMENT AND UNCOMMENT ACCORDINGLY 
  # Vaccination data
  #age.group.vax <- c("Age60-69", "Age70-79", "Age80+")  
  #age.group.vax <- c("Age60-69") 
  #age.group.vax <- c("Age70-79") 
  age.group.vax <- c("Age80+") 
  
  # Mortality data
  #age.group <- c("60-69", "70-79", "80+")
  #age.group <- c("60-69")
  #age.group <- c("70-79")
  age.group <- c("80+")
  
 
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
  
  
  # Allow cumulative counting while keeping NAs
  cumsum_keep_na <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      return(cumsum(x, na.rm=TRUE))
    }
  }
  # Allow counting while keeping NAs
  sum_keep_na <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      return(sum(x, na.rm=TRUE))
    }
  }
  
  
  ################################
  # SET THE GRAPHICS ENVIRONMENT #
  ################################
  
  # Define x axis and colours for plotting
  week_breaks=c("2020-51", "2021-01", "2021-05", "2021-10", "2021-15", "2021-20", "2021-25", "2021-30", "2021-35")
  week_labels=c("51", "1\n2021", "5", "10", "15", "20", "25", "30", "35")
  colours =c("#558ed5", "#c3d69b")
  
  
  ####################################
  # SET THE CALCULATIONS ENVIRONMENT #
  ####################################
  
  # Set Vaccine Effectiveness (VE) values: 60% and 95%
  VE1 = 0.60 # First dose
  VE2 = 0.95 # Second dose
  
  # Set lag weeks used in analysis (representing immune response and reporting delay): 3 and 1
  weeks.lag.1 = 3 # after first dose
  weeks.lag.2 = 1 # after first dose
  
  
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
    ggsave(paste0("Deaths_averted_Fig1A_",reporting.week,".png"), width = 10, height = 7)
  country.vaccination.curves.age <- plot.country.vaccination.curves(vax_coverage_cnty, figure="B")
    ggsave(paste0("Deaths_averted_Fig1B_",reporting.week,".png"), width = 10, height = 7)
    # Regional overview
  expected.mortality <- plot.expected.mortality(Expected_cases, country_summary, age.group)
    ggsave(paste0("Deaths_averted_Fig2_",reporting.week,".png"), width = 12, height = 7)
  }
  
  #############################################################
  # Run tables                                                # 
  # ONLY WHEN ALL CODE FOR SINGLE AGE GROUPS HAVE BEEN RUN!!! #
  #############################################################
  # Read table created above with country level vaccination information
  vax_countries <- read.csv("vaccinations_by_countries.csv", header = TRUE, stringsAsFactors = FALSE)
  
  Table.1 <- create.table1(vax_countries) 
  write.csv(Table.1, paste0("Deaths_averted_table1_", reporting.week,".csv"))
  
  Table.2 <- create.table2(vax_countries) 
  write.csv(Table.2, paste0("Deaths_averted_table2_", reporting.week,".csv"))
  
  #############################################################
  # Run sensitivity analyses                                  # 
  # ONLY WHEN ALL CODE FOR SINGLE AGE GROUPS HAVE BEEN RUN!!! #
  #############################################################
  # For VE analysis, replace VE values above with 
  # Low values: VE1=0.5 and VE2=0.7
  # High values: VE1=0.7 and VE2=0.957
  
  # For timing analysis, replace lag values values above with 
  # short values: weeks.lag.1=2 and weeks.lag.2=1
  # Long values: weeks.lag.1=4 and weeks.lag.2=2
  