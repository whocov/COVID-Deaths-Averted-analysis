# Script to create Table 1 for Deaths Averted analysis
# Margaux Mesle - meslem@who.int
# September 2021


create.table1 <- function(vax_countries) {

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
  
  # Calculate NUmber of deaths and mortality rates per 100,000
  age6080 <- age6080 %>%
    filter(!report_country %in% c("Republic of Moldova", "Romania", "Ukraine")) %>%
    group_by(report_country) %>%
    mutate(dosefirst.over60=dosefirst.6069+dosefirst.7079+dosefirst.80,
           dosesecond.over60=dosesecond.6069+dosesecond.7079+dosesecond.80,
           uptakefirst.over60=round(((dosefirst.6069+dosefirst.7079+dosefirst.80)/(denominator.6069+denominator.7079+denominator.80))*100),
           uptakesecond.over60=round(((dosesecond.6069+dosesecond.7079+dosesecond.80)/(denominator.6069+denominator.7079+denominator.80))*100),
           DeathsObserved.over60=(DeathsObserved.6069+DeathsObserved.7079+DeathsObserved.80),
           DeathsAvertedDose1.over60=(DeathsAvertedDose1.6069+DeathsAvertedDose1.7079+DeathsAvertedDose1.80),
           DeathsAvertedDose2.over60=(DeathsAvertedDose2.6069+DeathsAvertedDose2.7079+DeathsAvertedDose2.80),
           DeathsAverted.over60=(DeathsAverted.6069+DeathsAverted.7079+DeathsAverted.80),
           denominator.over60=denominator.6069+denominator.7079+denominator.80) %>%
    dplyr::select(report_country, dosefirst.over60, dosesecond.over60, uptakefirst.over60, uptakesecond.over60, DeathsObserved.over60, DeathsAvertedDose1.over60,
                  DeathsAvertedDose2.over60, DeathsAverted.over60, denominator.over60) %>%
    ungroup() 
  
  # Treat 3 countries seperatly 
  ageover60 <- read.csv(paste0("Expected_cases_over60yo_",reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE) 
  ageover60 <- prepare.data.for.tables(ageover60) %>%
    rename_with( ~ paste0(.x, ".over60")) %>%
    rename(report_country=`report_country.over60`) %>%
    mutate(report_country=str_replace(report_country,"report_country.*$", "report_country")) %>%
    filter(report_country %in% c("Republic of Moldova", "Romania", "Ukraine"))
  
  Table.1 <- full_join(age6080, ageover60) %>%
    arrange(report_country) %>%
    adorn_totals("row") %>%
    group_by(report_country) %>%
    mutate(Mortality.Rate.over60=ifelse(is.na(Mortality.Rate.over60), round((DeathsObserved.over60/denominator.over60)*100000,1), Mortality.Rate.over60),
           Expected.Mortality.Rate.over60=ifelse(is.na(Expected.Mortality.Rate.over60), round(((DeathsObserved.over60+DeathsAverted.over60)/denominator.over60)*100000,1), Expected.Mortality.Rate.over60),
           Mortality.Rate.over60=ifelse(report_country=="Total", round((DeathsObserved.over60/denominator.over60)*100000,1), Mortality.Rate.over60),
           Expected.Mortality.Rate.over60=ifelse(report_country=="Total", round(((DeathsObserved.over60+DeathsAverted.over60)/denominator.over60)*100000,1), Expected.Mortality.Rate.over60)) %>%
    mutate(uptakefirst.over60=round((dosefirst.over60/denominator.over60)*100), 
           uptakesecond.over60=round((dosesecond.over60/denominator.over60)*100),
           uptakefirst.over60=ifelse(uptakefirst.over60>100, 100, uptakefirst.over60),
           uptakesecond.over60=ifelse(uptakesecond.over60>100, 100, uptakesecond.over60),
           pc.Expected.deaths=round((1-(Mortality.Rate.over60/Expected.Mortality.Rate.over60))*100)) %>%
    ungroup() %>%
    select(-c(dosefirst.over60, dosesecond.over60, denominator.over60)) %>%
    rename(Country=report_country,
           `Partial VU`=uptakefirst.over60,
           `Full VU`=uptakesecond.over60,
           `Observed deaths`=DeathsObserved.over60,
           `Averted after 1 dose`=DeathsAvertedDose1.over60,
           `Averted after 2 doses`=DeathsAvertedDose2.over60,
           `Averted - total`=DeathsAverted.over60,
           `Observed Mortality Rate`=Mortality.Rate.over60,
           `Expected Mortality Rate`=Expected.Mortality.Rate.over60,
           `% Expected Deaths Averted by Vaccination`=pc.Expected.deaths) 
  
    Table.1$`Vaccinations used` <- vax_countries$vax_list[match(Table.1$Country, vax_countries$report_country)]
    Table.1 <- Table.1 %>%
      select(Country, `Vaccinations used`, `Partial VU`, `Full VU`, `Observed deaths`, `Averted after 1 dose`, `Averted after 2 doses`,
             `Averted - total`, `Observed Mortality Rate`, `Expected Mortality Rate`, `% Expected Deaths Averted by Vaccination`)
  
 return(Table.1) 
}
