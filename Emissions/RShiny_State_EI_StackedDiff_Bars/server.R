# State EI Diff Stacked Bars
# Read CSV into R

library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
library(wrapr)
library(shiny)

options(shiny.reactlog=TRUE)

shinyServer(function(input, output) {
  
  dat <- reactive({

emis.df <- read.csv(file="2022v1_state_sector_summary_v1.csv", header=TRUE, sep=",")

# Add headers and organize fields
temp1.df <- data.frame(Group=emis.df$EISGroup,Sector=emis.df$EISSector,T1Code=emis.df$T1Code,T1Desc=emis.df$T1Desc,FIPS=emis.df$FIPS,State=emis.df$State,ST=emis.df$ST,Poll=emis.df$Pollutant,
                       'e2016v3'=emis.df$e2016,'e2017'=emis.df$e2017,'e2018'=emis.df$e2018,
                       'e2019'=emis.df$e2019,'e2020'=emis.df$e2020,'e2021'=emis.df$e2021,'e2022v1'=emis.df$e2022)


# Remove non-US sectors and PM10, get rid of NAs
#temp.df <- filter(temp1.df,ST == input$state & ST!='OTHER' & ST !='CANADA' & ST != 'MEXICO' & ST != 'SECA' & ST != 'OS' & Poll != "PM10" & Poll != "PEC" & Poll != "POC")
temp.df <- filter(temp1.df,ST == input$state & Poll %in% input$pollutant & ST !='OTHER' & ST !='CANADA' & ST != 'MEXICO' & Poll != "PM10-PRI")

temp_new.df <- temp.df %>% mutate_all(~replace(., is.na(.), 0))

# Group the data by sector and pollutant totals
plot.df <- temp_new.df %>%
  group_by(Group,Poll) %>%
  summarise(
    Emis1 = sum(across(all_of(input$platform1))),
    Emis2 = sum(across(all_of(input$platform2))),.groups='drop'
  )

# Calculate differences
plot.df$emisdiff <- plot.df$Emis2 - plot.df$Emis1
plot.df$emisratio <- 100 *(plot.df$Emis2 - plot.df$Emis1) / plot.df$Emis1

print(plot.df)
  })
  # Absolute Diff Plot
  output$diffplot <- renderPlot({
    # Setup the plot
    charttitle <- paste("Emissions Inventory Absolute Differences",sep="")
    chartsubt <-paste(input$platform2," - ",input$platform1," | State: ",input$state,sep="")
    polls <- c("SO2","NH3","VOC","NOX","CO","PM25")
    cvals=c("Aircraft"="#996633","Commercial / Inst"="#421717","Commercial Marine Vessels"="#660000","Electric Generation"="#990000","Fertilizer"="#FF0000","Fuel Comb - Industrial"="#FF99CC","Electric Generation"="#FF9900","Fertilizer"="#FFCC00","Fuel Comb - Industrial"="#FFFF00","Fugitive dust"="#314733","Industrial Processes"="#336600","Livestock"="#99FF99","Locomotives"="#003366","Miscellaneous"="#0066CC","Nonroad diesel"="#99CCFF","Nonroad gas + other"="#660099","Oil and Gas"="#9933FF","Onroad Heavy Duty"="#CC99FF","Onroad Light Duty"="#000000","Prescribed and Ag Fires"="#474747","Residential"="#868a8f","Solvents"="#adadad","Waste disposal"="#ffffff","Wildfires"="#472828") 
    
# 2016v2 platform diff plots
  ggplot(melt(dat(),measure.vars=c('emisdiff'),id.vars=c('Poll','Group')),aes(x=variable,y=value*1e-6)) + 
  geom_col(aes(fill=Group),na.rm=TRUE) +facet_wrap(~ Poll,ncol=3,scales="free_y") + theme_light() + 
  labs(title=charttitle,subtitle=chartsubt,x="Inventory",y="Emissions Diff (1e6 tons/year)",fill=NULL) + 
  scale_fill_manual(values=cvals)#+scale_x_discrete(labels=c("2023-2016")) 

  })
  # Percent Diff Plot
  output$percentplot <- renderPlot({
    # Setup the plot
    charttitle <- paste("Emissions Inventory Percent Differences",sep="")
    chartsubt <-paste("100 * (",input$platform2,"-",input$platform1,")/",input$platform1," | State: ",input$state,sep="")
    polls <- c("SO2","NH3","VOC","NOX","CO","PM25")
    cvals=c("Aircraft"="#996633","Commercial / Inst"="#421717","Commercial Marine Vessels"="#660000","Electric Generation"="#990000","Fertilizer"="#FF0000","Fuel Comb - Industrial"="#FF99CC","Electric Generation"="#FF9900","Fertilizer"="#FFCC00","Fuel Comb - Industrial"="#FFFF00","Fugitive dust"="#314733","Industrial Processes"="#336600","Livestock"="#99FF99","Locomotives"="#003366","Miscellaneous"="#0066CC","Nonroad diesel"="#99CCFF","Nonroad gas + other"="#660099","Oil and Gas"="#9933FF","Onroad Heavy Duty"="#CC99FF","Onroad Light Duty"="#000000","Prescribed and Ag Fires"="#474747","Residential"="#868a8f","Solvents"="#adadad","Waste disposal"="#ffffff","Wildfires"="#472828") 
    
    # 2016v2 platform ratio plots
    ggplot(melt(dat(),measure.vars=c('emisratio'),id.vars=c('Poll','Group')),aes(x=variable,y=value)) + 
      geom_col(aes(fill=Group),na.rm=TRUE) +facet_wrap(~ Poll,ncol=3,scales="free_y") + theme_light() + 
      labs(title=charttitle,subtitle=chartsubt,x="Inventory",y="Emissions (%)",fill=NULL) + 
      scale_fill_manual(values=cvals)#+scale_x_discrete(labels=c("2023/2016")) 
  })
})


