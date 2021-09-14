# Functions to calculate number of deaths averted from COVID vaccinations
# Margaux Mesle - meslem@who.int
# First created: June 2021
# Lastest update: September 2021



restrict.vaccine.data <- function(vax_clean) {
  # Check that age group selected is correct
  print(unique(vax_clean$pretty_targetgroup))
  

  # Restrict vaccination data to countries of interest and calculate number of doses
  vax_lim <- vax_clean %>%
    dplyr::select(report_country, year_week, pretty_targetgroup, derived_denominator, dosefirst, dosesecond) %>%
    filter(report_country%in% country_list$countries) %>%
    filter(!report_country%in% country) %>%
    arrange(report_country, year_week) %>%
    group_by(report_country, year_week) %>%
    mutate(denominator=sum(unique(derived_denominator))) %>%
    summarise(FirstDose=sum_keep_na(dosefirst),
              SecondDose=sum_keep_na(dosesecond),
              denominator=unique(denominator)) %>%
    ungroup() %>%
    arrange(report_country, year_week)

  return(vax_lim)
}  


calculate.country.vaccinations <- function(vax_lim) {
  
   # Calculate vaccination coverage by country and number of doses
  vax_coverage_cnty <- vax_lim %>%
    mutate(FirstDose=ifelse(is.na(FirstDose), 0, FirstDose),
           SecondDose=ifelse(is.na(SecondDose), 0, SecondDose)) %>%
    arrange(report_country, year_week) %>%
    group_by(report_country) %>%
    mutate(nFirstDose=cumsum(FirstDose),
           nSecondDose=cumsum(SecondDose),
           pcFirstDose=round((cumsum(FirstDose)/denominator)*100),
           pcSecondDose=round((cumsum(SecondDose)/denominator)*100)) %>%
    ungroup()  %>%
    dplyr::select(-c(FirstDose, SecondDose)) %>%
    pivot_longer(cols=nFirstDose:nSecondDose) %>%
    rename(Dosage=name,
           NumberDoses=value)  %>%
    pivot_longer(cols=pcFirstDose:pcSecondDose) %>%
    rename(DoseNo=name,
           percentage=value)  
  
 return(vax_coverage_cnty)
} 
  
calculate.regional.vaccinations <- function(vax_lim) {
  
  # Calculate region's total denominator for age groups considered
  RegTotalDenom <- vax_lim %>%
    dplyr::select(year_week, denominator, report_country) %>%
    group_by(year_week) %>%
    summarise(nunique=unique(denominator),
              totalDenom=sum(nunique)) %>%
    dplyr::select(-nunique) %>%
    distinct() 
  Region.total.denom <- max(RegTotalDenom$totalDenom)
  
  # Calculate regional vaccination coverage for both doses
  vax_coverage_rgn <- vax_lim %>%
    mutate(FirstDose=ifelse(is.na(FirstDose), 0, FirstDose),
           SecondDose=ifelse(is.na(SecondDose), 0, SecondDose)) %>%
    arrange(report_country, year_week) %>%
    group_by(year_week) %>%
    summarise(FirstDose=sum(FirstDose),
              SecondDose=sum(SecondDose)) %>%
    mutate(nFirstDose=cumsum(FirstDose),
           nSecondDose=cumsum(SecondDose),
           ndenominator=max(RegTotalDenom$totalDenom),
           pcFirstDose=round((cumsum(FirstDose)/ndenominator)*100),
           pcSecondDose=round((cumsum(SecondDose)/ndenominator)*100)) %>%
    ungroup()  %>%
    dplyr::select(-c(FirstDose, SecondDose)) %>%
    pivot_longer(cols="nFirstDose":"nSecondDose") %>%
    rename(Dosage=name,
           NumberDoses=value)  %>%
    pivot_longer(cols="pcFirstDose":"pcSecondDose") %>%
    rename(DoseNo=name,
           percentage=value)  
  
  return(vax_coverage_rgn)
  
}
  
calculate.expected.cases <- function(deaths_age, vax_lim, age.group, country_list, country_summary, VE1, VE2, weeks.lag.1, weeks.lag.2, save.data=TRUE) {
    
  # Restrict data to age and weeks interested in and countries with info about cases and vaccination
  deaths_age_lim <- deaths_age %>%
    filter(CountryName %in% country_summary$report_country) %>%
    subset(age_group %in% age.group) %>%
    subset(year_week %in% vax_lim$year_week) %>%
    group_by(CountryName, year_week) %>%
    summarise(DeathsObserved=sum_keep_na(DeathsObserved))
  

  # Calculate number of expected deaths after each round of vaccination
  Expected_cases <- merge(vax_lim, deaths_age_lim, by.x=c("report_country", "year_week"), by.y=c("CountryName", "year_week"), all=TRUE)

  if(length(age.group) ==1) {
    Expected_cases <- Expected_cases[!Expected_cases$report_country=="Ukraine",]
  } 
  
  # Make sure all countries are included, sorted by date and all have a denominator
  Expected_cases <- Expected_cases %>%
    filter(!report_country %in% country) %>%
    arrange(report_country, year_week) %>%
    group_by(report_country) %>%
    fill(denominator, .direction = "downup") 
  
    Expected_cases <- Expected_cases %>%
      mutate(DeathsObserved=replace_na(DeathsObserved, 0),
             DeathsObserved.roll=rollmean(DeathsObserved, k=3, align="center", fill=NA), # Add 3 weeks rolling average number of deaths
             FirstDose=replace_na(FirstDose, 0),
             SecondDose=replace_na(SecondDose, 0),
             dosefirst=cumsum(FirstDose),
             dosesecond=cumsum(SecondDose),
             uptakefirst = pmin(dosefirst/denominator, 1),
             uptakesecond = pmin(dosesecond/denominator, 1),
             uptakefirst.lag = lag(uptakefirst, n=weeks.lag.1, default=0),
             uptakesecond.lag = lag(uptakesecond, n=weeks.lag.2, default=0),
             uptakefirstonly.lag = pmax(uptakefirst.lag - uptakesecond.lag, 0),
             # Add lags to each vaccine dose calculation
             DeathsAvertedDose1 = DeathsObserved.roll * ((uptakefirstonly.lag * VE1)/(1 - uptakefirstonly.lag*VE1 - uptakesecond.lag*VE2)),
             DeathsAvertedDose2 = DeathsObserved.roll * ((uptakesecond.lag * VE2)/(1 - uptakefirstonly.lag*VE1 - uptakesecond.lag*VE2)),
             TotalAverted = DeathsAvertedDose1 + DeathsAvertedDose2,
             DeathsExpected = TotalAverted + DeathsObserved)

  if(save.data==TRUE) {
    if(length(age.group)==1) {
      write.csv(Expected_cases,paste0("Expected_cases_",age.group, "_", reporting.week, ".csv"))
      
    } else if(length(age.group)>1) {
      write.csv(Expected_cases,paste0("Expected_cases_over60yo_", reporting.week, ".csv"))
    }
  }
  
  
  return(Expected_cases)

}
