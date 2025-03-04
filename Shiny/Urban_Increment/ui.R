library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Washington U. 2022 PM2.5 Concentrations by Urban Area"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_state", 
                  "Select State(s):", 
                  choices = NULL, 
                  selected = NULL, 
                  multiple = TRUE),
      uiOutput("urban_area_selector")
    ),
    
    mainPanel(
      plotOutput("pm25_plot"),
      dataTableOutput("pm25_table"),
      downloadButton("download_data", "Download Data")
    )
  )
)
