# State EI Stacked Bars
# Read CSV into R

library(ggplot2)
library(reshape2)
library(DT)
library(dplyr)


options(shiny.reactlog=TRUE)

shinyServer(function(input, output) {
  
  dat <- reactive({

emis.df <- read.csv(file="2022v1_state_sector_CAP_emissions_04oct2024.csv", header=TRUE, sep=",")

# Add headers and organize fields
temp1.df <- data.frame(Sector=emis.df$EISSector,State=emis.df$State,ST=emis.df$ST,Poll=emis.df$Pollutant,
                       'e2022'=emis.df$e2022,'e2026'=emis.df$e2026,'e2032'=emis.df$e2032,'e2038'=emis.df$e2038)

  # Remove non-US sectors and PM10
if ( input$state != "National") {
   temp_us.df <- filter(temp1.df, ST == input$state & Poll %in% input$pollutant & ST !='OTHER' & ST !='CANADA' & ST != 'MEXICO' & Poll != "PM10-PRI")
} else {
   temp_us.df <- filter(temp1.df, Poll %in% input$pollutant & ST !='OTHER' & ST !='CANADA' & ST != 'MEXICO' & Poll != "PM10-PRI")
}

# Put all pollutants on one plot
plot.df <- aggregate(cbind(e2022,e2026,e2032,e2038) ~ Sector+Poll, data=temp_us.df, FUN=sum,na.rm=FALSE)
})
  
  output$platformplot <- renderPlot({
    # Setup the plot
    charttitle <- paste("Annual Emissions by Sector",sep="")
    chartsubt1 <-paste("State: ",input$state,sep="")
    polls <- input$pollutant # c("SO2","NH3","VOC","NOX","CO","PM25-PRI","PEC","POC")
    cval_new=c("Airports"="#996633","Commercial Marine (C3)"="#421717","Commercial Marine (C1/C2)"="#660000","Nonroad Mobile"="#990000","Onroad Mobile"="#FF0000","Rail"="#FF99CC","Electricity Generation"="#FF9900","Industrial Point"="#FFCC00","Point Oil and Gas"="#FFFF00","Nonpoint Oil and Gas"="#314733","Solvents"="#336600","Nonpoint"="#99FF99","Livestock"="#003366","Fertilizer"="#0066CC","Residential Wood Combustion"="#99CCFF","Open Burning"="#660099","Prescribed Fires"="#9933FF","Ag Fires"="#CC99FF","Wildfires"="#000000","Fugitive Dust"="#474747","Biogenic"="#868a8f") 
    
      ggplot(melt(dat(),measure.vars=(input$platform),id.vars=c('Poll','Sector')),aes(x=variable,y=value*1e-6)) + 
      geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ Poll,ncol=3,scales="free_y") + theme_light() + 
      labs(title=charttitle,subtitle=chartsubt1,x="Emissions Year",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cval_new) 
  })
  
     output$platformtable <- DT::renderDT({
      # Melt the data
      melted_data <- melt(dat(), measure.vars = c('e2022', 'e2026', 'e2032', 'e2038'), 
                          id.vars = c('Poll', 'Sector'))
      
      # Filter the melted data based on selected platform
      filtered_data <- melted_data %>% filter(variable %in% input$platform)
      
      # Reshape the data to have years as columns
      reshaped_data <- tidyr::spread(filtered_data, key = variable, value = value)
      
      # Order the columns based on selected platform
      ordered_data <- reshaped_data[, c("Poll", "Sector", input$platform)]
      
      # Return the ordered data
      datatable(ordered_data) %>%
        formatRound(columns = input$platform, digits = 1) 
    
  })
})