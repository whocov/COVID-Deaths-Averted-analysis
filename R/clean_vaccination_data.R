# Script to clean vaccination data
# Margaux Mesle - meslem@who.int
# July 2021




clean.vaccination.data <- function(vaccine, Vax_denom) {
  
  # Tidy up vaccination data set
  vax_data <- vaccine %>%
    dplyr::select(DateUsedForStatisticsISO, Region, TargetGroup, Vaccine, DoseFirst, DoseSecond) %>%
    filter(TargetGroup%in%c("Age60_69" , "Age70_79", "Age80+") | TargetGroup=="1_Age60+") %>%
    mutate(TargetGroup=str_replace(TargetGroup, "_", "-"),
           TargetGroup=str_replace(TargetGroup, "1-", "1_")) %>%
    mutate(Region=ifelse(Region=="UKM", "UKScotland", Region)) %>%
    mutate(Region=ifelse(Region=="UK_ENG", "UKEngland", Region)) %>%
    mutate(DoseSecond=ifelse(Vaccine=="JANSS", DoseFirst, DoseSecond)) %>%
    arrange(Region, DateUsedForStatisticsISO) %>%
    mutate(year_week = str_replace(DateUsedForStatisticsISO, "-W", "-")) %>%
    dplyr::select(-c(Vaccine, DateUsedForStatisticsISO))
  
  # Replace country codes with country names
  vax_data$report_country <- country_codes$region[match(vax_data$Region, country_codes$ReportingCountry)] 
  vax_data <- vax_data[!is.na(vax_data$report_country),]
  vax_data <- select(vax_data, -Region)
  
  # Make template with all weeks and age groups per country
  template <- expand.grid(report_country=unique(vax_data$report_country),
                   year_week=unique(vax_data$year_week),
                   TargetGroup=unique(vax_data$TargetGroup))
  
  template <- template %>%
    arrange(report_country, year_week)
  
  # Combine dataset with template: ensure complete data considered
  vax_data <- full_join(template, vax_data) %>%
    mutate(report_country=ifelse(report_country=="Moldova, Republic of", "Republic of Moldova", report_country))
  
  # Add denominator column and keep only age groups interested in
  vax_clean <- merge(vax_data, Vax_denom, by.x = c("report_country", "TargetGroup"), by.y=c("report_country", "targetgroup"), all=T)
  vax_clean <- vax_clean %>%
    filter(!(TargetGroup=="1_Age<60" | TargetGroup=="ALL" | TargetGroup=="Age00-17" | TargetGroup=="Age18-24" | 
             TargetGroup=="Age25-49" | TargetGroup=="Age50-59"))
  
  
  if (length(age.group.vax)>1) {
    
    # Tidy up some countries that don't have age breakdowns
   countries.to.tidy <- c("Ukraine", "Republic of Moldova", "Romania")
   country.clean <- vax_clean %>%
      filter(report_country %in% countries.to.tidy & TargetGroup=="1_Age60+") %>%
      group_by(year_week, report_country, population) %>%
      summarise(DoseFirst=sum_keep_na(DoseFirst),
                DoseSecond=sum_keep_na(DoseSecond)) %>%
      ungroup() %>%
      mutate(TargetGroup="Age 60+", .after =report_country )
    
  vax_clean <- vax_clean %>%
    filter(!report_country %in% countries.to.tidy) %>%
    full_join(country.clean) %>%
    filter(TargetGroup %in% age.group.vax | TargetGroup=="Age 60+") %>%
    rename(derived_denominator=population,
           pretty_targetgroup=TargetGroup) %>%
    mutate(pretty_targetgroup=str_replace(pretty_targetgroup, "1_", ""),
           pretty_targetgroup=str_replace(pretty_targetgroup, "Age", "Age ")) %>%
    rename(dosefirst=DoseFirst,
           dosesecond=DoseSecond) %>%
    group_by(report_country, year_week) %>%
    summarise(dosefirst=sum_keep_na(dosefirst),
              dosesecond=sum_keep_na(dosesecond)) %>%
    ungroup() %>%
    mutate(pretty_targetgroup="Age 60+")
  
  vax_denom_60 <- Vax_denom %>% filter(targetgroup=="1_Age60+")
  vax_clean$derived_denominator <- vax_denom_60$population[match(vax_clean$report_country, vax_denom_60$report_country)]
  
  } else {

    vax_clean <- vax_clean %>%
      filter(TargetGroup %in% age.group.vax) %>%
      rename(derived_denominator=population,
             pretty_targetgroup=TargetGroup) %>%
      mutate(pretty_targetgroup=str_replace(pretty_targetgroup, "1_", ""),
             pretty_targetgroup=str_replace(pretty_targetgroup, "Age", "Age ")) %>%
      rename(dosefirst=DoseFirst,
             dosesecond=DoseSecond) %>%
      group_by(report_country, pretty_targetgroup, year_week, derived_denominator) %>%
      summarise(dosefirst=sum_keep_na(dosefirst),
                dosesecond=sum_keep_na(dosesecond)) %>%
      ungroup()
  }
  
 
  return(vax_clean)
}
