# Script to create Table 2 for Deaths Averted analysis
# Margaux Mesle - meslem@who.int
# September 2021


create.table2 <- function(vax_countries) {
  
  # Read in outputs created by 'Running_deaths_averted_report.R'
  age.6069 <- read.csv(paste0("Expected_cases_60-69_",reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE)
  age.6069 <- prepare.data.for.tables(age.6069) %>%
    rename_with( ~ paste0(.x, ".6069")) %>%
    rename(report_country=`report_country.6069`)
  
  age.7079 <- read.csv(paste0("Expected_cases_70-79_",reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE)
  age.7079 <- prepare.data.for.tables(age.7079) %>%
    rename_with( ~ paste0(.x, ".7079")) %>%
    rename(report_country=`report_country.7079`)
  
  age.80 <- read.csv(paste0("Expected_cases_80+_",reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE)
  age.80 <- prepare.data.for.tables(age.80) %>%
    rename_with( ~ paste0(.x, ".80")) %>%
    rename(report_country=`report_country.80`)
  
  # Join all datasets together
  age6079 <- full_join(age.6069, age.7079)
  age6080 <- full_join(age6079, age.80) 

  # Create Table 2 with age breakdowns per country
  Table.2 <- age6080 %>%
    adorn_totals("row") %>%
    group_by(report_country) %>%
    mutate(uptakesecond.6069=ifelse(report_country=="Total", round((dosesecond.6069/denominator.6069)*100), uptakesecond.6069),
           uptakesecond.7079=ifelse(report_country=="Total", round((dosesecond.7079/denominator.7079)*100), uptakesecond.7079),
           uptakesecond.80=ifelse(report_country=="Total", round((dosesecond.80/denominator.80)*100), uptakesecond.80)) %>%
    mutate(Mortality.Rate.6069=ifelse(report_country=="Total", round((DeathsObserved.6069/denominator.6069)*100000,2), Mortality.Rate.6069),
           Expected.Mortality.Rate.6069=ifelse(report_country=="Total", round(((DeathsAverted.6069+DeathsObserved.6069)/denominator.6069)*100000,2), Expected.Mortality.Rate.6069),
           Mortality.Rate.7079=ifelse(report_country=="Total", round((DeathsObserved.7079/denominator.7079)*100000,2), Mortality.Rate.7079),
           Expected.Mortality.Rate.7079=ifelse(report_country=="Total", round(((DeathsAverted.7079+DeathsObserved.7079)/denominator.7079)*100000,2), Expected.Mortality.Rate.7079),
           Mortality.Rate.80=ifelse(report_country=="Total", round((DeathsObserved.80/denominator.80)*100000,2), Mortality.Rate.80),
           Expected.Mortality.Rate.80=ifelse(report_country=="Total", round(((DeathsAverted.80+DeathsObserved.80)/denominator.80)*100000,2), Expected.Mortality.Rate.80)) %>%
    dplyr::select(-c(dosefirst.6069, uptakefirst.6069, dosesecond.6069, DeathsAvertedDose1.6069, DeathsAvertedDose2.6069, 
                     dosefirst.7079, uptakefirst.7079, dosesecond.7079, DeathsAvertedDose1.7079, DeathsAvertedDose2.7079, 
                     dosefirst.80, uptakefirst.80, dosesecond.80, DeathsAvertedDose1.80, DeathsAvertedDose2.80, 
                     denominator.6069, denominator.7079, denominator.80)) %>%
    rename(Country=report_country,
           `Full VU (%) (60-69)`=uptakesecond.6069,
           `Full VU (%) (70-79)`=uptakesecond.7079,
           `Full VU (%) (80+)`=uptakesecond.80,
           `Deaths Observed (60-69)`=DeathsObserved.6069,
           `Deaths Observed (70-79)`=DeathsObserved.7079,
           `Deaths Observed (80)`=DeathsObserved.80,
           `Deaths Averted (60-69)`=DeathsAverted.6069,
           `Deaths Averted (70-79)`=DeathsAverted.7079,
           `Deaths Averted (80)`=DeathsAverted.80,
           `Observed Mortality Rate per 100,000 (60-69)`=Mortality.Rate.6069,
           `Observed Mortality Rate per 100,000 (70-79)`=Mortality.Rate.7079,
           `Observed Mortality Rate per 100,000 (80)`=Mortality.Rate.80,
           `Expected Mortality Rate per 100,000 (60-69)`=Expected.Mortality.Rate.6069,
           `Expected Mortality Rate per 100,000 (70-79)`=Expected.Mortality.Rate.7079,
           `Expected Mortality Rate per 100,000 (80)`=Expected.Mortality.Rate.80)
  
  return(Table.2) 
}
