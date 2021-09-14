# Script to plot regional vaccination coverage
# Margaux Mesle - meslem@who.int
# June 2021

# Choose between vaccination coverage ("coverage") or number of doses administered ("doses")


plot.regional.vaccination.coverage <- function(vax_coverage_rgn, measure="coverage") {
  

  if (measure=="coverage") {
    

    # Plot regional vaccination coverage
    curves_rgn <- ggplot(vax_coverage_rgn)+
      geom_line(aes(year_week, percentage, group=DoseNo, colour=DoseNo)) +
      scale_x_discrete(breaks=week_breaks, labels=week_labels) +
      scale_y_continuous(limits=c(0, 100)) +
      scale_colour_manual(name="Dosage",
                          labels=c("First dose", "Complete series"),
                          values=c("pcFirstDose" ="#558ed5", "pcSecondDose"="#c3d69b")) +
      labs(x="Week",
           y="Vaccination coverage %")+
      theme(legend.position = "none") +
      theme_minimal()
    
    if(length(age.group)==1) {
      curves_rgn <- curves_rgn +
        labs(subtitle = paste0("Vaccination coverage among ",age.group," by week"))
      
    } else if(length(age.group)>1) {
      curves_rgn <- curves_rgn +
        labs(subtitle = paste0("Vaccination coverage among over 60 by week"))
    }
    
    
  } else if(measure=="doses") {
    
 
    curves_rgn <- ggplot(vax_coverage_rgn)+
      geom_bar(aes(year_week, NumberDoses/1000, fill=Dosage), stat="identity", position="identity") + 
      scale_x_discrete(breaks=week_breaks, labels=week_labels) +
      scale_y_continuous(labels=comma_format(accuracy = 1)) +
      scale_fill_manual(values=c("#558ed5", "#c3d69b"), 
                        labels=c("nFirstDose"="First dose", "nSecondDose"="Complete series"),
                        guide = guide_legend()) +
      labs(x="Week",
           y="Number of doses (x1 000)",
           fill="Dosage")  +
      theme(legend.position = "bottom") +
      theme_minimal()
    
    if(length(age.group)==1) {
      curves_rgn <- curves_rgn +
        labs(subtitle = paste0("Cumulative number of doses administered among ",age.group," by week"))
      
    } else if(length(age.group)>1) {
      curves_rgn <- curves_rgn +
        labs(subtitle = paste0("Cumulative number of doses administered among over 60 by week"))
    }
    
    
  }
 
  return(curves_rgn)
  
}
