# Script to plot observed and prevented deaths per country
# Margaux Mesle - meslem@who.int
# June 2021

# Plot Figure 2:
# Number of observed and expected mortality counts, as well as vaccination uptake (VU) per week


plot.expected.mortality <- function(Expected_cases, country_summary, age.group) {
  
  #colours =c("#558ed5", "#c3d69b")
  
  # Reshape data to have legend
  cases.plot <- Expected_cases %>%
    select(report_country, year_week, DeathsObserved, DeathsExpected) %>%
    pivot_longer(cols = c("DeathsObserved","DeathsExpected")) %>%
    rename(Measure=name,
           Mortality.count=value) %>%
    mutate(report_country=ifelse(report_country=="United Kingdom (Scotland)", "United Kingdom\n(Scotland)", report_country)) %>%
    mutate(report_country=ifelse(report_country=="United Kingdom (England)", "United Kingdom\n(England)", report_country))
  
  
  vax_start_dates <- find.start.vaccination.dates(vax_clean)
  vax_start_dates <- vax_start_dates %>%
    filter(report_country %in% country_list$countries) %>%
    mutate(report_country=ifelse(report_country=="United Kingdom (Scotland)", "United Kingdom\n(Scotland)", report_country)) %>%
    mutate(report_country=ifelse(report_country=="United Kingdom (England)", "United Kingdom\n(England)", report_country))
  
  
  
  Cases <- ggplot(cases.plot)+
    geom_line(aes(year_week, Mortality.count, group=Measure, colour=Measure)) +
    geom_vline(data=vax_start_dates, aes(xintercept =year_week, col=dosefirst, linetype=dosefirst)) +
    scale_color_manual(name="",
                       values=c("#003C50", "#558ed5", "#c3d69b"),
                       limits=c("DeathsExpected", "DeathsObserved"),
                       labels=c("DeathsExpected"= "Mortality without\nvaccination (complete series)", 
                                "DeathsObserved"="Observed mortality")) +
    scale_linetype_manual(name = "", 
                          labels=c("start"="Start of vaccination",
                                   "z10pp"="First 10% doses administered\n(first doses only)"),
                          values = c("solid","dashed")) +
    facet_wrap(.~ report_country, scales = "free") +
    scale_x_discrete(breaks=week_breaks, labels=week_labels) +
    guides(colour=guide_legend(order=1),
           shape=guide_legend(order=2)) +
    labs(x="Week",
         y="Number of deaths") +
    theme_minimal() 
  
  if(length(age.group==1)) {
    Cases <- Cases +
      labs(subtitle = "2. Observed and averted mortality in population aged 60 years and over ")
  } else  if(length(age.group>1)) {
    Cases <- Cases +
      labs(subtitle = paste0("2. Observed and averted mortality in age group: ", age.group))
    
  }
  
  return(Cases)
}
