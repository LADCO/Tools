# State EI Stacked Bars
# Read CSV into R

library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
library(shiny)
library(wrapr)

options(shiny.reactlog=TRUE)

shinyServer(function(input, output) {
  
  dat <- reactive({

emis.df <- read.csv(file="2022v1_state_sector_summary_v1.csv", header=TRUE, sep=",")

# Add headers and organize fields
temp1.df <- data.frame(Group=emis.df$EISGroup,Sector=emis.df$EISSector,T1Code=emis.df$T1Code,T1Desc=emis.df$T1Desc,FIPS=emis.df$FIPS,State=emis.df$State,ST=emis.df$ST,Poll=emis.df$Pollutant,
                       'e2016v3'=emis.df$e2016,'e2017'=emis.df$e2017,'e2018'=emis.df$e2018,
                       'e2019'=emis.df$e2019,'e2020'=emis.df$e2020,'e2021'=emis.df$e2021,'e2022v1'=emis.df$e2022)

  # Remove non-US sectors and PM10
temp_us.df <- filter(temp1.df,ST == input$state & Poll %in% input$pollutant & ST !='OTHER' & ST !='CANADA' & ST != 'MEXICO' & Poll != "PM10-PRI")

# Put all pollutants on one plot
plot.df <- aggregate(cbind(e2016v3,e2017,e2018,e2019,e2020,e2021,e2022v1) ~ Group+Poll, data=temp_us.df, FUN=sum,na.rm=FALSE)
})
  
  output$platformplot <- renderPlot({
    # Setup the plot
    charttitle <- paste("Annual Emissions by EIS Sector",sep="")
    chartsubt1 <-paste("State: ",input$state,sep="")
    polls <- input$pollutant # c("SO2","NH3","VOC","NOX","CO","PM25-PRI","PEC","POC")
   # cval_new=c("Fugitive dust"="#996633","Fertilizer"="#990000","Livestock"="#660000","Aircraft"="#421717","Commercial Marine Vessels"="#FF0000","Electric Generation"="#FF99CC","Commercial / Inst"="#FF9900","Fuel Comb - Industrial"="#FFCC00","Industrial Processes"="#FFFF00","Residential"="#314733","Locomotives"="#336600","Nonroad diesel"="#99FF99","Nonroad gas + other"="#003366","Oil and Gas"="#0066CC","Onroad Heavy Duty"="#99CCFF","Onroad Light Duty"="#660099","Prescribed and Ag Fires"="#9933FF","Wildfires"="#CC99FF","Solvents"="#adadad","Waste Disposal"="#474747","Miscellaneous"="#000000") 
    cval_new=c("Aircraft"="#996633","Commercial / Inst"="#421717","Commercial Marine Vessels"="#660000","Electric Generation"="#990000","Fertilizer"="#FF0000","Fuel Comb - Industrial"="#FF99CC","Electric Generation"="#FF9900","Fertilizer"="#FFCC00","Fuel Comb - Industrial"="#FFFF00","Fugitive dust"="#314733","Industrial Processes"="#336600","Livestock"="#99FF99","Locomotives"="#003366","Miscellaneous"="#0066CC","Nonroad diesel"="#99CCFF","Nonroad gas + other"="#660099","Oil and Gas"="#9933FF","Onroad Heavy Duty"="#CC99FF","Onroad Light Duty"="#000000","Prescribed and Ag Fires"="#474747","Residential"="#868a8f","Solvents"="#adadad","Waste disposal"="#ffffff","Wildfires"="#472828") 
    
#  dev.size("px")
  ggplot(melt(dat(),measure.vars=(input$platform),id.vars=c('Poll','Group')),aes(x=variable,y=value*1e-6)) + 
      geom_col(aes(fill=Group),na.rm=TRUE) +facet_wrap(~ Poll,ncol=3,scales="free_y") + theme_light() + 
      labs(title=charttitle,subtitle=chartsubt1,x="Emissions Year",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cval_new) 
    
})
  
#  output$platformtable <- renderDataTable({
    
#    arrange(melt(dat(),measure.vars=(input$platform),id.vars=c('Poll','Group')), desc("Emissions Inventory by Year"))
    
    output$platformtable <- renderDataTable({

      # Melt the data
      melted_data <- melt(dat(), measure.vars = c('e2016v3', 'e2017', 'e2018', 'e2019', 'e2020', 'e2021', 'e2022v1'), 
                          id.vars = c('Poll', 'Group'))
      
      # Filter the melted data based on selected platform
      filtered_data <- melted_data %>% filter(variable %in% input$platform)
      
      # Reshape the data to have years as columns
      reshaped_data <- tidyr::spread(filtered_data, key = variable, value = value)
      
      # Order the columns based on selected platform
      ordered_data <- reshaped_data[, c("Poll", "Group", input$platform)]
      
      # Return the ordered data
      ordered_data

    
  })
})