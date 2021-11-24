# Script to plot vaccination coverage by country
# Margaux Mesle - meslem@who.int
# June 2021

plot.country.vaccination.curves <- function(vax_coverage_cnty, figure="A") {
  
  vax_coverage_cnty <- vax_coverage_cnty %>%
    mutate(report_country=ifelse(report_country=="United Kingdom (Scotland)", "United Kingdom\n(Scotland)", report_country)) %>%
    mutate(report_country=ifelse(report_country=="United Kingdom (England)", "United Kingdom\n(England)", report_country)) %>%
    mutate(percentage=ifelse(percentage>100, 100, percentage))
  
  
  if(figure=="A") {

  curves <- ggplot(vax_coverage_cnty, 
                   aes(year_week, percentage, group=DoseNo, colour=DoseNo) )+
    geom_line(size=1) +
    geom_hline(yintercept = c(60, 95), col="black", size=0.3) +
    facet_wrap(.~ report_country, scales = "free_x") +
    scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) +
    scale_x_discrete(breaks=week_breaks, labels=week_labels) +
    scale_colour_manual(name="Dosage",
                        values = colours, 
                        labels=c("pcFirstDose"="First dose", "pcSecondDose"="Full coverage")) +
    labs(x="Week",
         y="Vaccination Uptake %") +
    theme_minimal() +
    labs(subtitle = paste0("1A. Vaccination Uptake in population aged 60 and over in 33 WHO Europe countries between December 2020 and ", current.month ,"2021\nBlack horizontal lines represent 60% and (95% vaccination coverage respectively."))
    
      
  } else if(figure=="B") {
 
    age6069 <- read.csv(paste0("Expected_cases_60-69_",reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE)
    age6069 <- age6069 %>% select(-X) %>% mutate(age="60_69")
    age7079 <- read.csv(paste0("Expected_cases_70-79_",reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE)
    age7079 <- age7079 %>% select(-X) %>% mutate(age="70_79")
    age80 <- read.csv(paste0("Expected_cases_80+_",reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE)
    age80 <- age80 %>% select(-X) %>% mutate(age="80+")
    
    ageover60 <- read.csv(paste0("Expected_cases_over60yo_",reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE) 
    select.countries <- ageover60 %>%
      dplyr::select(-X) %>%
      filter(report_country %in% c("Republic of Moldova", "Romania", "Ukraine")) %>%
      mutate(age="over60") %>%
      dplyr::select(report_country, year_week, uptakesecond, age)
    
    age6079 <- full_join(age6069, age7079)
    age6080 <- full_join(age6079, age80) %>%
      dplyr::select(report_country, year_week, uptakesecond, age) %>%
      full_join(select.countries) %>%
      mutate(report_country=ifelse(report_country=="United Kingdom (Scotland)", "United Kingdom\n(Scotland)", report_country)) %>%
      mutate(report_country=ifelse(report_country=="United Kingdom (England)", "United Kingdom\n(England)", report_country)) %>%
      mutate(uptakesecond=uptakesecond*100) 
    
    curves <- ggplot(age6080, aes(year_week, uptakesecond, group=age, colour=age)) +
      geom_line(size=1) +
      facet_wrap(.~ report_country, scales="free_x") +
      scale_x_discrete(breaks=week_breaks, labels=week_labels) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) +
      scale_colour_manual(name="Age group",
                          values = c("#c6d9f1", "#558ed5", "#1f497d", "#6BBABF"), 
                          labels=c("60_69"="60-69 years", "70_79"="70-79 years", "80+"="over 80 years", "over60"="over 60 years")) +
      labs(subtitle ="B. Cumulative complete vaccination uptake by age group (where available) and by week",
           x="Week",
           y="Vaccination uptake %") +
      theme_minimal() +
      labs(subtitle =paste0("1B. Cumulative complete Vaccination Uptake by age group (where available) in population aged 60 and over in 33 WHO Europe countries between December 2020 and ", current.month ," 2021"))
 
    
  }
  
  return(curves)
}