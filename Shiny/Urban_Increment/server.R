library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(DT)

options(shiny.reactlog=TRUE)

# Load your data
# Replace 'your_file.csv' with the path to your CSV file
pm25_data <- read_csv("./WashU_2022_PM25_urban_rural_state.csv")

server <- function(input, output, session) {
  
  # Dynamically update the state selection based on the data, sorted alphabetically
  observe({
    states <- sort(unique(pm25_data$NAME))
    updateSelectInput(session, "selected_state", choices = states)
  })
  
  # Dynamically update the urban area selection based on selected states
  output$urban_area_selector <- renderUI({
    req(input$selected_state)
    
    filtered_data <- pm25_data %>% 
      filter(NAME %in% input$selected_state)
    
    urban_areas <- sort(unique(filtered_data$NAME20))
    
    selectInput("selected_urban_areas", 
                "Select Urban Area(s):", 
                choices = urban_areas, 
                selected = NULL,  # No urban areas selected by default
                multiple = TRUE)
  })
  
  # Generate the plot based on selected states and urban areas
  output$pm25_plot <- renderPlot({
    req(input$selected_urban_areas)
    
    filtered_data <- pm25_data %>% 
      filter(NAME %in% input$selected_state, 
             NAME20 %in% input$selected_urban_areas)
    
    # Prepare data for stacking
    filtered_data_long <- filtered_data %>%
      gather(key = "ConcentrationType", value = "Concentration", 
             rural_mean, urban_inc) %>%
      mutate(ConcentrationType = factor(ConcentrationType, 
                                        levels = c("urban_inc","rural_mean"),
                                        labels = c("Urban Increment","Rural Concentration")))
    
    ggplot(filtered_data_long, aes(x = NAME20, y = Concentration, fill = ConcentrationType)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Rural Concentration" = "lightblue", 
                                   "Urban Increment" = "darkblue")) +
      labs(x = NULL, y = "2022 Avg PM2.5 Concentration (ug/m3)", fill = "Concentration Type") +
      theme_minimal()
  })
  # Generate the table based on selected states and urban areas
  output$pm25_table <- renderDataTable({
    req(input$selected_urban_areas)
    
    filtered_data <- pm25_data %>% 
      filter(NAME %in% input$selected_state, 
             NAME20 %in% input$selected_urban_areas) %>%
      select(NAME, NAME20, rural_mean, urban_mean, urban_inc) %>%
      mutate(UrbanPercent = urban_inc / urban_mean * 100) %>%
      select(NAME, NAME20, rural_mean, urban_mean, urban_inc,UrbanPercent) %>%
      mutate(across(c(rural_mean, urban_mean, urban_inc,UrbanPercent), 
                    ~ round(.x, 2)))  # Round to 2 decimal places 
    
    datatable(filtered_data, 
              options = list(pageLength = 10), 
              rownames = FALSE,
              colnames = c("State Name", "Urban Area Name", "Rural Conc (ug/m3)", "Urban Conc (ug/m3)", "Urban Increment (ug/m3)","Urban Percentage (%)"))
  })
  
  # Download handler for the data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("2022_urban-increment_pm25_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(input$selected_urban_areas)
      
      filtered_data <- pm25_data %>% 
        filter(NAME %in% input$selected_state, 
               NAME20 %in% input$selected_urban_areas) %>%
        select(NAME, NAME20, rural_mean, urban_mean, urban_inc) %>%
        mutate(across(c(rural_mean, urban_mean, urban_inc), 
                      ~ round(.x, 2)))  # Round to 2 decimal places
      
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
}