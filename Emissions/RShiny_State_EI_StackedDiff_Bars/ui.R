# ui.R for LADCO stacked bar charts #

library(shiny)
library(shinythemes)

pf.df<-c("e2016v3","e2017","e2018","e2019","e2020","e2021","e2022v1")
st.df<-c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC","DE", "FL", "GA", 
         "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
         "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
         "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
         "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
pol.df<-c("CO","NH3","NOX","PEC","PM25-PRI","POC","SO2","VOC")

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  # Application title
  titlePanel("Emissions Inventory Differences"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("pollutant", label = "Select a Pollutant (multiple)", choices = pol.df,multiple=TRUE,selected = unique(pol.df)[1]),
      selectInput("state", label = "Select a State", choices = st.df),
      selectInput("platform1", label = "Select First Inventory", choices = pf.df,selected=unique(pf.df)[1]),
      selectInput("platform2", label = "Select Second Inventory", choices = pf.df,selected=unique(pf.df)[2])
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("diffplot"),
      plotOutput("percentplot")
    )
  )
))
