# Script for functions
# Margaux Mesle - meslem@who.int
# June 2021



assign.country.names <- function(data) {
  CountryCode <- read.csv("Data/CountryCode.csv", header = TRUE, stringsAsFactors = FALSE)
  
  data$CountryName <- CountryCode$region[match(data$ReportingCountry, CountryCode$ReportingCountry)]
  data <- data %>% dplyr::select(-c(ReportingCountry, Region))
  
  return(data)
}


clean.tessy.country.name <- function(country_names, data_source) {
  
  # define functions -------------------------------------------------------------------------------------
  
  # categorise UK nation based on CountryName and DataSource columns
  categorise_UK_nation <- function(country_names, data_source) {
    
    country_ids <- c("United Kingdom, England", "United Kingdom, Scotland", "United Kingdom, Wales", 
                     "United Kingdom, Northern Ireland")
    
    country_source_terms <- c("^UK-ENG", "^UK-SCOT", "^UK-WALES", "^UK-NI")
    
    for (i in 1:length(country_ids)) {
      contains_country <- str_detect(data_source, country_source_terms[i])
      country_names <- if_else(contains_country, country_ids[i], country_names)
    }
    
    return(country_names)
  }
  
  # transform data ---------------------------------------------------------------------------------------
  cleaned_country_names <- categorise_UK_nation(country_names, data_source)
  cleaned_country_names <- recode(cleaned_country_names, 
                                  "Kosovo" = "Kosovo (in accordance with Security Council resolution 1244 (1999))",
                                  "Russia" = "Russian Federation",
                                  "Moldova" = "Republic of Moldova",
                                  "Republic of North Macedonia" = "North Macedonia"
  )
  
  return(cleaned_country_names)
}

select.correct.countries <- function(country, deaths_age, vax_clean) {
  
  country_list_deaths = deaths_age %>%
    summarise(unique(CountryName)) %>% 
    rename(countries=`unique(CountryName)`)
  
  country_list_vax = vax_clean %>%
    #filter(!targetgroup=="ALL") %>%
    mutate(report_country=ifelse(report_country=="United Kingdom, Scotland", "United Kingdom (Scotland)", report_country)) %>%
    mutate(report_country=ifelse(report_country=="United Kingdom, England", "United Kingdom (England)", report_country)) %>%
    summarise(unique(report_country)) %>% 
    rename(countries=`unique(report_country)`)
  
  
  country_list <- inner_join(country_list_deaths, country_list_vax) %>%
    filter(!countries %in% country) %>%
    arrange(countries) 
  
  return(country_list)
}


find.start.vaccination.dates <- function (country_summary) {
  
  start_dates <- country_summary %>%
    dplyr::select(report_country, year_week, dosefirst, derived_denominator) %>%
    mutate(dosefirst=ifelse(is.na(dosefirst), 0, dosefirst)) %>%
    filter(dosefirst>0) %>%
    arrange(report_country, year_week) %>%
    group_by(report_country) %>%
    slice(1) %>%
    mutate(dosefirst="start")
  
  
  pp10_dates <- country_summary %>%
    dplyr::select(report_country, year_week, dosefirst, derived_denominator) %>%
    #filter(dosefirst>0) %>%
    mutate(dosefirst=ifelse(is.na(dosefirst), 0, dosefirst)) %>%
    arrange(report_country, year_week) %>%
    group_by(report_country) %>%
    mutate(dose.pp=round((cumsum(dosefirst)/derived_denominator)*100,1)) %>%
    filter(dose.pp>10) %>%
    slice(1) %>%
    mutate(dosefirst="z10pp")
  
  vax_start_dates <- full_join(start_dates, pp10_dates) %>%
    dplyr::select(-c(derived_denominator, dose.pp))
  
  return(vax_start_dates)
}


map.countries.included <- function(country_list){
  library(raster)
  library(ggplot2)
  library(dplyr)
  library(stringr)
  

  map_EU <- shapefile("~/GitHub/Deaths_averted/Data/Shapefiles/Detailed_Boundary_ADM0.shp")
  map_EU <- map_EU[map_EU$WHO_REGION=="EURO",]
  map_EU <- map_EU[!map_EU$ISO_2_CODE=="GL",]
  
  map_UK <- shapefile("~/GitHub/Deaths_averted/Data/Shapefiles/GLOBAL_ADM1.shp")
  map_UK <- map_UK[(map_UK$ADM1_NAME=="SCOTLAND" | map_UK$ADM1_NAME=="ENGLAND"),]
  
  country_list <- country_list %>%
    mutate(countries=str_to_upper(countries))
  
  map.to.plot <- map_EU[map_EU$ADM0_NAME %in% country_list$countries,]
  
  ggplot() +
    geom_polygon(data = map_EU, aes(x = long, y = lat, group = group), colour = "black", fill = "white") +
    geom_polygon(data = map.to.plot, aes(x = long, y = lat, group = group), colour = "black", fill = "forestgreen") +
    geom_polygon(data = map_Scot, aes(x = long, y = lat, group = group), colour = "black", fill = "forestgreen") +
    coord_map("rectangular",lat0=0, xlim=c(-30,70), ylim=c(30, 70)) +
    scale_y_continuous(breaks=c()) +
    scale_x_continuous(breaks=c()) +
    labs(fill="legend", title="", x="", y="") +
    theme_void()
  
  
  
  return(plot.countries)
  
}

# Number of deaths by country, age and week
calculate.mortality.by.age <- function(deaths) {
  
  
  deaths_age <- deaths %>%
    mutate(CountryName=ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName)) %>%
    mutate(CountryName=ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName)) %>%
    mutate(CountryName=ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName)) %>%
    mutate(CountryName=ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName)) %>%
    # Filter data down to needed columns
    dplyr::select(c(CountryName, ReportingCountry, DateUsedForStatisticsWeek) | (starts_with("Deaths"))) %>%
    # melt data frame    
    pivot_longer(cols=13:43) %>% 
    rename(year_week=DateUsedForStatisticsWeek,
           age_group=name,
           DeathsObserved=value) %>%
    dplyr::select(-c(`Deaths00.04`, `Deaths05.14`, `Deaths15.24`, `Deaths25.49`, `Deaths50.64`, `Deaths65.79`,`Deaths80.`, 
             `DeathsHCW`, `DeathsUNK`)) %>%
    filter(!(age_group=="DeathsAgeGenderUnk" | age_group=="DeathsUNK" | age_group=="DeathsHCW" | 
             age_group=="DeathsUnkM" | age_group=="DeathsUnkF" )) %>%
    filter((age_group=="Deaths60.64M" | age_group=="Deaths60.64F" | age_group=="Deaths65.69M" | age_group=="Deaths65.69F" | 
           age_group=="Deaths70.74M" | age_group=="Deaths70.74F" | age_group=="Deaths75.79M" | age_group=="Deaths75.79F" | 
           age_group=="Deaths80.F" | age_group=="Deaths80.M")) %>%
    # Tidy up element names
    mutate(year_week=str_replace(year_week, "'", ""),
           age_group=str_replace(age_group, "Deaths", ""),
           age_group=str_replace(age_group, "\\D$", "")) %>%
    dplyr::select(-ReportingCountry)
  
  
  # Re-name age groups
  deaths_age <- deaths_age %>%
    mutate(age_group=case_when((age_group=="60.64" | age_group=="65.69") ~'60-69',
                               (age_group=="70.74" | age_group=="75.79") ~'70-79',
                               (age_group=="80.") ~'80+')) 
  
  deaths_age <- deaths_age %>%
    mutate(CountryName = recode(CountryName, "Moldova"="Republic of Moldova")) %>%
    #filter(age_group=="60-69" | age_group=="70-79" | age_group=="80+") %>%
    group_by(CountryName, year_week, age_group) %>%
    summarise(DeathsObserved=sum_keep_na(DeathsObserved))

  return(deaths_age)
}

calculate.percentage.deaths.over60 <- function(deahts) {
  
  under.60 <- c("00.04", "05.09", "10.14", "15.19", "20.24", "25.29", "30.39","40.49", "50.59")
  over.60 <- c("60.64", "65.69", "70.74", "75.79", "80.")
  
  deaths_pc <- deaths %>%
    mutate(CountryName=ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName)) %>%
    mutate(CountryName=ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName)) %>%
    mutate(CountryName=ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName)) %>%
    mutate(CountryName=ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName)) %>%
    # Filter data down to needed columns
    dplyr::select(c(CountryName, ReportingCountry, DateUsedForStatisticsWeek) | (starts_with("Deaths"))) %>%
    # melt data frame    
    pivot_longer(cols=13:43) %>% 
    rename(year_week=DateUsedForStatisticsWeek,
           age_group=name,
           DeathsObserved=value) %>%
    dplyr::select(-c(`Deaths00.04`, `Deaths05.14`, `Deaths15.24`, `Deaths25.49`, `Deaths50.64`, `Deaths65.79`,`Deaths80.`, 
                     `DeathsHCW`, `DeathsUNK`)) %>%
    mutate(year_week=str_replace(year_week, "'", ""),
           age_group=str_replace(age_group, "Deaths", ""),
           age_group=str_replace(age_group, "\\D$", "")) %>%
    dplyr::select(-ReportingCountry) %>%
    mutate(age_group=ifelse(age_group %in% under.60, 'under60', age_group),
           age_group=ifelse(age_group %in% over.60, 'over60', age_group)) %>%
    group_by(age_group) %>%
    summarise(deaths_total=sum_keep_na(DeathsObserved)) %>%
    mutate(deaths_pc=(deaths_total/(sum(deaths_total)))*100) %>%
    ungroup()
  
}



determine.countries.included <- function(deaths_age, vax_clean) {
  
  country_list_deaths = deaths_age %>%
    summarise(unique(CountryName)) %>% 
    rename(countries=`unique(CountryName)`)
  
    # Not all countries have data refined to smaller age groups - take these out before running analysis
    countries.avoid <- c("Azerbaijan", "Georgia", "Kazakhstan", "Republic of Moldova", "Romania", "Serbia", "Ukraine", "Uzbekistan")
    country_list_deaths <- country_list_deaths %>%
      filter(!countries%in%countries.avoid) 

  country_list_vax = vax_clean %>%
    filter(!targetgroup=="ALL") %>%
    mutate(report_country=ifelse(report_country=="United Kingdom, Scotland", "United Kingdom (Scotland)", report_country)) %>%
    mutate(report_country=ifelse(report_country=="United Kingdom, England", "United Kingdom (England)", report_country)) %>%
    summarise(unique(report_country)) %>% 
    rename(countries=`unique(report_country)`)
  
  
  country_list <- inner_join(country_list_deaths, country_list_vax) %>%
    arrange(countries) 
  
  return(country_list)
}


find.vaccine.brand.per.country <- function(vaccine, country_summary) {
   # Which vaccines are used in which countries - this because a column in Table 1
  country_codes <- read.csv("Data/CountryCode.csv", header=T, stringsAsFactors = F)
  vax_countries <- vaccine %>%
    mutate(Region=ifelse(Region=="UKM", "UKScotland", Region)) %>%
    mutate(Region=ifelse(Region=="UK_ENG", "UKEngland", Region)) 
  
  vax_countries$report_country <- country_codes$region[match(vax_countries$Region, country_codes$ReportingCountry)] 
  vax_countries <- vax_countries[!is.na(vax_countries$report_country),]
  vax_countries <- select(vax_countries, -Region)
  
  
  vax_countries <- vax_countries %>%
    group_by(report_country) %>%
    mutate(report_country=ifelse(report_country=="Moldova, Republic of", "Republic of Moldova", report_country)) %>%
    filter(report_country%in% country_summary$report_country) %>%
    summarise(Vax=n_distinct(Vaccine),
              vax_list=paste0(sort(unique(Vaccine)), collapse="-"),
              vax_list=str_remove(vax_list, "-UNK")) # Remove 'Unknown product' if country has other vaccines listed
  
 return(vax_countries)
}


calculate.nonEU.denominators  <- function(extra_vaccine_denominators) {
  # add eurostat denominators for non-eu/eea countries where un pop data not available (and Serbia as UN pop data contains Kosovo)
  extra_vaccine_denominators <- extra_vaccine_denominators %>% 
    filter(Region == ReportingCountry | Region %in% c("England", "Northern Ireland", "Scotland", "Wales"), !(Region %in% c("UK", "LI"))) %>%
    mutate(Region = recode(Region, "England" = "UKEngland", "Wales" = "UKWales", "Scotland" = "UKScotland", "Northern Ireland" = "UKNI")) %>%
    filter(Region %in% c("RS", "AD", "MC", "SM", "XK"))
  
  extra_vaccine_denom <- extra_vaccine_denominators %>% 
    group_by(ReportingCountry, Region, TargetGroup) %>%
    filter(row_number() == 1) %>%
    mutate(num = n()) %>%
    summarise(Denominator = sum(Denominator), num = first(num)) %>%
    ungroup() %>%
    dplyr::select(-c(ReportingCountry, num))
  
  return(extra_vaccine_denom)
}

prepare.data.for.tables <- function(age) {
  # Tidy up weekly results table to calculate number of deaths averted by age and mortality rate
  # This is used to create Tables 1 and 2
  age <- age %>% 
    select(-X) %>% 
    group_by(report_country) %>%
    mutate(DeathsAverted=round(DeathsExpected-DeathsObserved)) %>%
    mutate(uptakesecond=round(uptakesecond*100)) %>%
    summarise(dosefirst=last(dosefirst),
              dosesecond=last(dosesecond),
              uptakefirst=last(uptakefirst),
              uptakesecond=last(uptakesecond),
              DeathsObserved=sum(DeathsObserved),
              DeathsAvertedDose1=round(sum_keep_na(DeathsAvertedDose1)),
              DeathsAvertedDose2=round(sum_keep_na(DeathsAvertedDose2)),
              DeathsAverted=sum(DeathsAvertedDose1+DeathsAvertedDose2, na.rm=TRUE),
              denominator=unique(denominator)) %>%
    mutate(Mortality.Rate=round((DeathsObserved/denominator)*100000,2),
           Expected.Mortality.Rate=round(((DeathsObserved+DeathsAverted)/denominator)*100000,2))  
  
  return(age)
}
