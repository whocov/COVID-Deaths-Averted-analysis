# Run sensitivity analyses with different lag times
# Margaux Mesle - meslem@who.int
# June 2021


  # The number of lag weeks used in the report are 3 and 1 for the first and second doses of vaccination, respectively
  # The script below is set to run with a short lag time (2 and 0.5 weeks) or long lag time (4 and 2 weeks)

  lag <- "short" # or "long
  
  if(lag=="short") {
    
    weeks.first.dose=2
    weeks.second.dose=1
    
    # Calculate number of expected deaths after each round of vaccination
    Expected_cases <- merge(vax_lim, deaths_age_lim, by.x=c("report_country", "year_week"), by.y=c("CountryName", "year_week"), all=TRUE)
    Expected_cases <- Expected_cases %>%
      filter(report_country %in% country_list$countries) %>%
      group_by(report_country) %>%
      fill(derived_denominator, .direction = "downup") %>%
      mutate(dosefirst=replace_na(dosefirst, 0),
             dosesecond=replace_na(dosesecond, 0),
             VaxImpact1=((cumsum(dosefirst)/derived_denominator)*0.5), # Calculate vaccination impact - first dose
             VaxImpact2=((cumsum(dosesecond)/derived_denominator)*0.95), # Calculate vaccination impact - second dose
             DeathsNoVax1=DeathsObserved+round(DeathsObserved*lag((VaxImpact1/(1-VaxImpact1)), n=weeks.first.dose, default = 0)),
             DeathsNoVax2=DeathsNoVax1+round(DeathsNoVax1*lag((VaxImpact2/(1-VaxImpact2)), n=weeks.second.dose, default = 0))) 
    
    countries.missing.data <- c("Germany", "Netherlands")
    Expected_cases <- Expected_cases[!(Expected_cases$report_country %in% countries.missing.data),]
    
    cases_summary <- Expected_cases %>% 
      group_by(report_country) %>%
      mutate(FullCoverage=(cumsum(dosesecond)/derived_denominator)*100) %>%
      summarise(FirstDose=sum(dosefirst),
                CompleteSeries=sum(dosesecond, na.rm=TRUE),
                FullCoverage=last(round(FullCoverage,2)),
                DeathsObserved=sum(DeathsObserved, na.rm=TRUE),
                DeathsAverted=sum(DeathsNoVax2, na.rm=TRUE)-DeathsObserved) 
    
    table_summary <- cases_summary %>%
      arrange(report_country) %>%
      adorn_totals("row") %>%
      rename(Country=report_country) %>%
      mutate(FullCoverage=ifelse(FullCoverage>100, Full.vax.coverage, FullCoverage)) %>%
      mutate(`First Dose`=format(round(as.numeric(FirstDose), 1), nsmall=0, big.mark=","),
             `Complete Series`=format(round(as.numeric(CompleteSeries), 1), nsmall=0, big.mark=","),
             `Full Coverage (%)`=format(round(as.numeric(FullCoverage), 1), nsmall=0, big.mark=","),
             `Observed Deaths`=format(round(as.numeric(DeathsObserved), 1), nsmall=0, big.mark=","),
             `Averted Deaths`=format(round(as.numeric(DeathsAverted), 1), nsmall=0, big.mark=",")) %>%
      select(-c(FirstDose, CompleteSeries, FullCoverage, DeathsObserved, DeathsAverted))
    
    write.csv(table_summary, "Expected_mortality_short_lag.csv")
    
  } else if (lag=="long") {
    
    
    weeks.first.dose=4
    weeks.second.dose=2
    

  # Calculate number of expected deaths after each round of vaccination
  Expected_cases <- merge(vax_lim, deaths_age_lim, by.x=c("report_country", "year_week"), by.y=c("CountryName", "year_week"), all=TRUE)
  Expected_cases <- Expected_cases %>%
    filter(report_country %in% country_list$countries) %>%
    group_by(report_country) %>%
    fill(derived_denominator, .direction = "downup") %>%
    mutate(dosefirst=replace_na(dosefirst, 0),
           dosesecond=replace_na(dosesecond, 0),
           VaxImpact1=((cumsum(dosefirst)/derived_denominator)*0.5), # Calculate vaccination impact - first dose
           VaxImpact2=((cumsum(dosesecond)/derived_denominator)*0.95), # Calculate vaccination impact - second dose
           DeathsNoVax1=DeathsObserved+round(DeathsObserved*lag((VaxImpact1/(1-VaxImpact1)), n=weeks.first.dose, default = 0)),
           DeathsNoVax2=DeathsNoVax1+round(DeathsNoVax1*lag((VaxImpact2/(1-VaxImpact2)), n=weeks.second.dose, default = 0))) 
  
  countries.missing.data <- c("Germany", "Netherlands")
  Expected_cases <- Expected_cases[!(Expected_cases$report_country %in% countries.missing.data),]
  
  cases_summary <- Expected_cases %>% 
    group_by(report_country) %>%
    mutate(FullCoverage=(cumsum(dosesecond)/derived_denominator)*100) %>%
    summarise(FirstDose=sum(dosefirst),
              CompleteSeries=sum(dosesecond, na.rm=TRUE),
              FullCoverage=last(round(FullCoverage,2)),
              DeathsObserved=sum(DeathsObserved, na.rm=TRUE),
              DeathsAverted=sum(DeathsNoVax2, na.rm=TRUE)-DeathsObserved) 
  
  table_summary <- cases_summary %>%
    arrange(report_country) %>%
    adorn_totals("row") %>%
    rename(Country=report_country) %>%
    mutate(FullCoverage=ifelse(FullCoverage>100, Full.vax.coverage, FullCoverage)) %>%
    mutate(`First Dose`=format(round(as.numeric(FirstDose), 1), nsmall=0, big.mark=","),
           `Complete Series`=format(round(as.numeric(CompleteSeries), 1), nsmall=0, big.mark=","),
           `Full Coverage (%)`=format(round(as.numeric(FullCoverage), 1), nsmall=0, big.mark=","),
           `Observed Deaths`=format(round(as.numeric(DeathsObserved), 1), nsmall=0, big.mark=","),
           `Averted Deaths`=format(round(as.numeric(DeathsAverted), 1), nsmall=0, big.mark=",")) %>%
    select(-c(FirstDose, CompleteSeries, FullCoverage, DeathsObserved, DeathsAverted))

  write.csv(table_summary, "Expected_mortality_long_lag.csv")
  
}

  
  # Calculate number of expected deaths after each round of vaccination
  Expected_cases <- merge(vax_lim, deaths_age_lim, by.x=c("report_country", "year_week"), by.y=c("CountryName", "year_week"), all=TRUE)
  Expected_cases <- Expected_cases %>%
    filter(report_country %in% country_list$countries) %>%
    group_by(report_country) %>%
    fill(derived_denominator, .direction = "downup") %>%
    mutate(dosefirst=replace_na(dosefirst, 0),
           dosesecond=replace_na(dosesecond, 0),
           VaxImpact1=((cumsum(dosefirst)/derived_denominator)*0.5), # Calculate vaccination impact - first dose
           VaxImpact2=((cumsum(dosesecond)/derived_denominator)*0.95), # Calculate vaccination impact - second dose
           DeathsNoVax1.short=DeathsObserved+round(DeathsObserved*lag((VaxImpact1/(1-VaxImpact1)), n=2, default = 0)),
           DeathsNoVax2.short=DeathsNoVax1.short+round(DeathsNoVax1.short*lag((VaxImpact2/(1-VaxImpact2)), n=1, default = 0)),
           DeathsNoVax1.used=DeathsObserved+round(DeathsObserved*lag((VaxImpact1/(1-VaxImpact1)), n=3, default = 0)),
           DeathsNoVax2.used=DeathsNoVax1.used+round(DeathsNoVax1.used*lag((VaxImpact2/(1-VaxImpact2)), n=1, default = 0)),
           DeathsNoVax1.long=DeathsObserved+round(DeathsObserved*lag((VaxImpact1/(1-VaxImpact1)), n=4, default = 0)),
           DeathsNoVax2.long=DeathsNoVax1.long+round(DeathsNoVax1.long*lag((VaxImpact2/(1-VaxImpact2)), n=2, default = 0))) 
   
   
  
  countries.missing.data <- c("Germany", "Netherlands")
  Expected_cases <- Expected_cases[!(Expected_cases$report_country %in% countries.missing.data),]
  
  cases_summary <- Expected_cases %>% 
    group_by(report_country) %>%
    mutate(FullCoverage=(cumsum(dosesecond)/derived_denominator)*100) %>%
    summarise(FirstDose=sum(dosefirst),
              CompleteSeries=sum(dosesecond, na.rm=TRUE),
              FullCoverage=last(round(FullCoverage,2)),
              DeathsObserved=sum(DeathsObserved, na.rm=TRUE),
              DeathsAverted.short=sum(DeathsNoVax2.short, na.rm=TRUE)-DeathsObserved,
              DeathsAverted.used=sum(DeathsNoVax2.used, na.rm=TRUE)-DeathsObserved,
              DeathsAverted.long=sum(DeathsNoVax2.long, na.rm=TRUE)-DeathsObserved) 
  
  table_summary <- cases_summary %>%
    arrange(report_country) %>%
    adorn_totals("row") %>%
    rename(Country=report_country) %>%
    mutate(FullCoverage=ifelse(FullCoverage>100, Full.vax.coverage, FullCoverage)) %>%
    mutate(`Start vaccination`=vax_start_dates$year_week[match(table_summary$Country, vax_start_dates$report_country)],
           `First Dose`=format(round(as.numeric(FirstDose), 1), nsmall=0, big.mark=","),
           `Complete Series`=format(round(as.numeric(CompleteSeries), 1), nsmall=0, big.mark=","),
           `Full Coverage (%)`=format(round(as.numeric(FullCoverage), 1), nsmall=0, big.mark=","),
           `Observed Deaths`=format(round(as.numeric(DeathsObserved), 1), nsmall=0, big.mark=","),
           `Averted Deaths - short`=format(round(as.numeric(DeathsAverted.short), 1), nsmall=0, big.mark=","),
           `Averted Deaths - used`=format(round(as.numeric(DeathsAverted.used), 1), nsmall=0, big.mark=","),
           `Averted Deaths - long`=format(round(as.numeric(DeathsAverted.long), 1), nsmall=0, big.mark=",")) %>%
    select(-c(FirstDose, CompleteSeries, FullCoverage, DeathsObserved, DeathsAverted.short, DeathsAverted.used, DeathsAverted.long))
  
  write.csv(table_summary, "Expected_mortality_comparisons.csv")
  