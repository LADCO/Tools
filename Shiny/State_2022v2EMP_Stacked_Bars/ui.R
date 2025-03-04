# ui.R for LADCO stacked bar charts #

library(shiny)
library(shinythemes)
library(DT)

pf.df<-c("e2022","e2026","e2032","e2038")
        
st.df<-c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
         "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
         "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
         "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
         "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY","National")
pol.df<-c("CO","NH3","NOX","PM25-PRI","SO2","VOC","VOC_anthro")

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  # Application title
  titlePanel("Emissions Inventory Summary"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("pollutant", label = "Select a Pollutant (multiple)", choices = pol.df,multiple=TRUE,selected = unique(pol.df)[1]),
      selectInput("state", label = "Select a State (only one)", choices = st.df),
      selectInput("platform", label = "Select Emissions Years (multiple)", choices = pf.df,multiple=TRUE,selected=unique(pf.df)[1])
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("platformplot"),
#      dataTableOutput("platformtable")
      DT::DTOutput("platformtable")
    )
  )
))
