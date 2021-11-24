# Script to create summary table of sensitivity analyses
# Margaux Mesle - meslem@who.int
# September 2021

create.sensitivity.analyses.summary <- function(reporting.week){
  long_lag <- read.csv(paste0("Deaths_averted_table1_", reporting.week, "_long_lag.csv"), header=TRUE, stringsAsFactors = FALSE) %>%
    select(Country, `Averted...total`, `Expected.Mortality.Rate`, `X..Expected.Deaths.Averted.by.Vaccination`) %>%
    rename(Total_averted_over_60 = `Averted...total`,
           Expected_MR_over_60 = `Expected.Mortality.Rate`,
           Pc_change = `X..Expected.Deaths.Averted.by.Vaccination`) %>%
    filter(Country=="Total") %>%
    mutate(Scenario="Long time lag")
  
  short_lag <- read.csv(paste0("Deaths_averted_table1_", reporting.week, "_short_lag.csv"), header=TRUE, stringsAsFactors = FALSE) %>%
    select(Country, `Averted...total`, `Expected.Mortality.Rate`, `X..Expected.Deaths.Averted.by.Vaccination`) %>%
    rename(Total_averted_over_60 = `Averted...total`,
           Expected_MR_over_60 = `Expected.Mortality.Rate`,
           Pc_change = `X..Expected.Deaths.Averted.by.Vaccination`)%>%
    filter(Country=="Total") %>%
    mutate(Scenario="Short time lag")
  
  low_VE <- read.csv(paste0("Deaths_averted_table1_", reporting.week, "_low_VE.csv"), header=TRUE, stringsAsFactors = FALSE) %>%
    select(Country, `Averted...total`, `Expected.Mortality.Rate`, `X..Expected.Deaths.Averted.by.Vaccination`) %>%
    rename(Total_averted_over_60 = `Averted...total`,
           Expected_MR_over_60 = `Expected.Mortality.Rate`,
           Pc_change = `X..Expected.Deaths.Averted.by.Vaccination`)%>%
    filter(Country=="Total") %>%
    mutate(Scenario="Low VE values")
  
  high_VE <- read.csv(paste0("Deaths_averted_table1_", reporting.week, "_high_VE.csv"), header=TRUE, stringsAsFactors = FALSE) %>%
    select(Country, `Averted...total`, `Expected.Mortality.Rate`, `X..Expected.Deaths.Averted.by.Vaccination`) %>%
    rename(Total_averted_over_60 = `Averted...total`,
           Expected_MR_over_60 = `Expected.Mortality.Rate`,
           Pc_change = `X..Expected.Deaths.Averted.by.Vaccination`)%>%
    filter(Country=="Total") %>%
    mutate(Scenario="High VE values")
  
  summary.table <- rbind(short_lag, long_lag, low_VE, high_VE)
  
  summary.table <- summary.table %>%
    select(Scenario, Total_averted_over_60, Expected_MR_over_60, Pc_change) %>%
    mutate(Description="VE values VE1= 50%  VE2= 70%; time lags: first dose= 4 weeks full coverage= 3 weeks",
           Description=ifelse(Scenario=="High VE values", 
                              "VE values VE1= 70%  VE2= 95.7%; time lags first dose= 4 weeks full coverage= 3 weeks", Description),
           Description=ifelse(Scenario=="Short time lag", 
                              "Time lags first dose= 3 weeks full coverage= 2 weeks; VE values VE1= 70%  VE2= 97%", Description),
           Description=ifelse(Scenario=="Long time lag", 
                              "Time lags first dose= 5 weeks full coverage= 4 weeks; VE values VE1= 70%  VE2= 97%", Description)) %>%
    select(Scenario, Description, Total_averted_over_60, Expected_MR_over_60, Pc_change) 
    
    
    
  long_lag <- read.csv(paste0("Deaths_averted_table2_", reporting.week, "_long_lag.csv"), header=TRUE, stringsAsFactors = FALSE) %>%
    select(Country, `Deaths.Averted..60.69.`, `Deaths.Averted..70.79.`, `Deaths.Averted..80.`) %>%
    rename(Total_averted_60 = `Deaths.Averted..60.69.`,
           Total_averted_70 = `Deaths.Averted..70.79.`,
           Total_averted_80 = `Deaths.Averted..80.`) %>%
    filter(Country=="Total") %>%
    mutate(Scenario="Long time lag")
  
  short_lag <- read.csv(paste0("Deaths_averted_table2_", reporting.week, "_short_lag.csv"), header=TRUE, stringsAsFactors = FALSE) %>%
    select(Country, `Deaths.Averted..60.69.`, `Deaths.Averted..70.79.`, `Deaths.Averted..80.`) %>%
    rename(Total_averted_60 = `Deaths.Averted..60.69.`,
           Total_averted_70 = `Deaths.Averted..70.79.`,
           Total_averted_80 = `Deaths.Averted..80.`) %>%
    filter(Country=="Total") %>%
    mutate(Scenario="Short time lag")
  
  low_VE <- read.csv(paste0("Deaths_averted_table2_", reporting.week, "_low_VE.csv"), header=TRUE, stringsAsFactors = FALSE) %>%
    select(Country, `Deaths.Averted..60.69.`, `Deaths.Averted..70.79.`, `Deaths.Averted..80.`) %>%
    rename(Total_averted_60 = `Deaths.Averted..60.69.`,
           Total_averted_70 = `Deaths.Averted..70.79.`,
           Total_averted_80 = `Deaths.Averted..80.`) %>%
    filter(Country=="Total") %>%
    mutate(Scenario="Low VE values")
  
  high_VE <- read.csv(paste0("Deaths_averted_table2_", reporting.week, "_high_VE.csv"), header=TRUE, stringsAsFactors = FALSE) %>%
    select(Country, `Deaths.Averted..60.69.`, `Deaths.Averted..70.79.`, `Deaths.Averted..80.`) %>%
    rename(Total_averted_60 = `Deaths.Averted..60.69.`,
           Total_averted_70 = `Deaths.Averted..70.79.`,
           Total_averted_80 = `Deaths.Averted..80.`) %>%
    filter(Country=="Total") %>%
    mutate(Scenario="High VE values")
  
  
  summary.table.age <- rbind(short_lag, long_lag, low_VE, high_VE)
  
  summary.table.age <- summary.table.age %>%
    select(Scenario, Total_averted_60, Total_averted_70, Total_averted_80) %>%
    mutate(Description="VE values VE1= 50%  VE2= 70%; time lags: first dose= 4 weeks full coverage= 3 weeks",
           Description=ifelse(Scenario=="High VE values", 
                              "VE values VE1= 70%  VE2= 95.7%; time lags first dose= 4 weeks full coverage= 3 weeks", Description),
           Description=ifelse(Scenario=="Short time lag", 
                              "Time lags first dose= 3 weeks full coverage= 2 weeks; VE values VE1= 70%  VE2= 97%", Description),
           Description=ifelse(Scenario=="Long time lag", 
                              "Time lags first dose= 5 weeks full coverage= 4 weeks; VE values VE1= 70%  VE2= 97%", Description)) %>%
    select(Scenario, Description, Total_averted_60, Total_averted_70, Total_averted_80) 
  
    
    
  summary.table <- full_join(summary.table, summary.table.age) %>%
      select(Scenario, Description, Total_averted_60, Total_averted_70, Total_averted_80, Total_averted_over_60, Expected_MR_over_60, Pc_change) %>%
      rename(`Deaths averted (60-69 year olds)`=Total_averted_60,
             `Deaths averted (70-79 year olds)`=Total_averted_70,
             `Deaths averted (80 year olds)`=Total_averted_80,
             `Total deaths averted`=Total_averted_over_60,
             `Expected mortality rate per 100 000 (over 60)`=Expected_MR_over_60,
             `% expected deaths averted by vaccination`= Pc_change)
  
    return(summary.table)
}