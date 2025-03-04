library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = " Ozone Monitor Dashboard"),  # Header

  # No sidebar
  dashboardSidebar(disable = TRUE),  # This removes the sidebar
  
  dashboardBody(
        tags$head(tags$style(HTML("
      .main-header .logo {
        white-space: normal !important;
        overflow: visible !important;
        font-size: 18px !important;
      }
    "))),
    fluidRow(
      column(
        width = 8,
        # Box with instructions 
        box(
          title = "Instructions",
          width = NULL, # Spans full width of the column
          status = "primary",
          solidHeader = TRUE,
          div(HTML("This dashboard displays base year (2022) and future year (2026) county total emissions and ozone tracers for monitors in the Great Lakes region. Select (click) a site on the map to display the data for that monitor. Use the Emissions Data Selection drop down menu to select the emissions data to display in the pie charts and table.
             <br><br>The emissions pie charts and table show the annual total emissions from the <a href='https://www.epa.gov/air-emissions-modeling/2022v1-emissions-modeling-platform'>2022v1 and 2026v1 inventories</a> for the county in which the selected monitor is located. The CAMx 2016 Ozone Tracers are ozone concentration estimates from <a href='https://www.ladco.org/technical/ladco-internal/ladco-projects/ladco-2015-o3-naaqs-moderate-area-sip-technical-support-document/'>LADCO 2016 CAMx APCA simulations</a> that tagged inventory sectors and geographic source regions.
             <br><br>Mouse over the pie charts to see the values of each slice. Click on legend elements to remove/add slices in the chart."))
        )
          ),
      column(
        width = 4,
        # Box with emissions pollutant selection
        box(
          title = "Emissions Data Selection",
          width = NULL, # Spans full width of the column
          status = "primary",
          solidHeader = TRUE,
          div("Emissions data are from the 2022v1 emissions modeling platform."),
          br(),
          # Dropdown to select pollutant
          selectInput(
            inputId = "pollutant", 
            label = "Select Emissions Pollutant:", 
            choices = NULL, # Choices will be populated dynamically
            selected = NULL
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 8,
        # Map occupies full width of the page
        box(
          title = "Monitor Map",
          width = NULL, # Full width
          status = "success",
          solidHeader = TRUE,
          leafletOutput("map", height = 400) # Increase map height for better visibility
        )
      ),
    column(
      width = 4,
      # Monitor details on the left
      box(
        title = "Monitor Details",
        width = NULL, # Full width of column
        status = "info",
        solidHeader = TRUE,
        tableOutput("monitor_details")
      )
    )
    ),
      fluidRow(
      column(
        width = 6,
        # Pie chart on the right
        box(
          title = textOutput("base_emissions_pie_title"),  # Dynamically updated title
          width = NULL, # Full width of column
          status = "warning",
          solidHeader = TRUE,
          plotlyOutput("base_emissions_pie", height = "400px")
        )
      ),
      column(
        width = 6,
        # Pie chart on the right
        box(
          title = textOutput("future_emissions_pie_title"),  # Dynamically updated title
          width = NULL, # Full width of column
          status = "warning",
          solidHeader = TRUE,
          plotlyOutput("future_emissions_pie", height = "400px")
        )
      )
      ),
  fluidRow(
    column(
      width = 6,
      # Inventory sector pie chart
      box(
        title = "2016 CAMx Ozone Tracers: Inventory Sectors",
        width = NULL, # Full width of column
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput("apca_sector_pie", height = "400px")
      )
    ),
    column(
      width = 6,
      # Inventory region pie chart
      box(
        title = "2016 CAMx Ozone Tracers: Source Regions",
        width = NULL, # Full width of column
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput("apca_region_pie", height = "400px") 
      ) 
    )
  ),
  fluidRow(
  column(
    width = 12,
    # Top 10% SCCs table
    box(
      title = textOutput("emissions_table_title"),  # Dynamically updated title
      width = NULL, # Full width of column
      status = "info",
      solidHeader = TRUE,
      tableOutput("top_scc_table") # Table for top 10% SCCs
    )
  )
)
)
)