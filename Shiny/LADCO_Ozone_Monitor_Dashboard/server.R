library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# Define the server logic
server <- function(input, output, session) {

  observeEvent(input$map_marker_click, {
    print("Map marker clicked!")
    print(input$map_marker_click$id)
  })
  
  sector_tags_reactive <- reactive({
    req(sector_tags)  # Ensure sector_tags is loaded
    sector_tags %>%
      mutate(monitor_id = as.character(monitor_id))  # Ensure monitor_id is character
  })
  
  region_tags_reactive <- reactive({
    req(region_tags)  # Ensure sector_tags is loaded
    region_tags %>%
      mutate(monitor_id = as.character(monitor_id))  # Ensure monitor_id is character
  })
  
  # Populate pollutant dropdown dynamically
  observe({
    req(emissions_data) # Ensure emissions_data is loaded
    unique_pollutants <- unique(emissions_data$Pollutant)
    updateSelectInput(session, "pollutant", choices = unique_pollutants, selected = unique_pollutants[1])
  })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet(monitor_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        layerId = ~monitor_id,
        radius = 5,
        color = "blue",
        fillOpacity = 0.7
      )
  })
  
  # Reactive expression to filter data based on the clicked site
  selected_data <- reactive({
    req(input$map_marker_click, input$pollutant)
    
    clicked_id <- input$map_marker_click$id
    
    clicked_fips <- monitor_data %>%
      filter(monitor_id == clicked_id) %>%
      pull(FIPS)
      
    req(clicked_fips)
    
    selected_monitor <- monitor_data %>%
      filter(monitor_id == clicked_id) %>%
      select(AQS_Site_ID, Local_Site_Name, CBSA)
    
    county_base_emissions <- emissions_data %>%
      filter(FIPS == clicked_fips & Pollutant == input$pollutant) %>%
      group_by(Sector) %>%
      summarize(Total_2022_Emissions = sum(e2022_tons, na.rm = TRUE)) %>%
      arrange(desc(Total_2022_Emissions))
    
    county_future_emissions <- emissions_data %>%
      filter(FIPS == clicked_fips & Pollutant == input$pollutant) %>%
      group_by(Sector) %>%
      summarize(Total_2026_Emissions = sum(e2026_tons, na.rm = TRUE)) %>%
      arrange(desc(Total_2026_Emissions))
    
    if (nrow(county_base_emissions) == 0) {
      county_base_emissions <- data.frame(Sector = character(), Total_2022_Emissions = numeric())
    }
    
    if (nrow(county_future_emissions) == 0) {
      county_future_emissions <- data.frame(Sector = character(), Total_2026_Emissions = numeric())
    }
    
    top_scc <- emissions_data %>%
      filter(FIPS == clicked_fips & Pollutant == input$pollutant) %>%
      group_by(SCC, Description, Sector) %>%
      summarize(Total_2022_Emissions = sum(e2022_tons, na.rm = TRUE),
                Total_2026_Emissions = sum(e2026_tons, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(Total_2022_Emissions)) %>%
      slice(1:25)
    
    if (nrow(top_scc) == 0) {
      top_scc <- data.frame(SCC = character(), Description = character(), Total_2022_Emissions = numeric(), Total_2026_Emissions = numeric())
    }
    
    list(monitor = selected_monitor, base_emissions = county_base_emissions, future_emissions = county_future_emissions, top_scc = top_scc)
  })
  
  # Collect the county name for emissions plot title 
  output$base_emissions_pie_title <- renderText({
    req(input$map_marker_click, monitor_data)  # Ensure a site is clicked and data is available
    
    print("🔍 Checking monitor_data for county name...")
    
    # Extract the county name based on the clicked monitor ID
    county_name <- monitor_data %>%
      filter(monitor_id == input$map_marker_click$id) %>%
      pull(County)  # Adjust column name if necessary
    
    # Debugging step to check extracted county name
    print(paste("Selected County:", county_name))
    
    # If a valid county name exists, format the title dynamically
    if (length(county_name) > 0 && !is.na(county_name)) {
      paste("2022v1 Annual County Emissions by Sector:", county_name)
    } else {
      "2022v1 Annual County Emissions by Sector"  # Default title if no county is found
    }
  })
  
  # Collect the county name for emissions plot title 
  output$future_emissions_pie_title <- renderText({
    req(input$map_marker_click, monitor_data)  # Ensure a site is clicked and data is available
    
    print("🔍 Checking monitor_data for county name...")
    
    # Extract the county name based on the clicked monitor ID
    county_name <- monitor_data %>%
      filter(monitor_id == input$map_marker_click$id) %>%
      pull(County)  # Adjust column name if necessary
    
    # Debugging step to check extracted county name
    print(paste("Selected County:", county_name))
    
    # If a valid county name exists, format the title dynamically
    if (length(county_name) > 0 && !is.na(county_name)) {
      paste("2026v1 Annual County Emissions by Sector:", county_name)
    } else {
      "2022v1 Annual County Emissions by Sector"  # Default title if no county is found
    }
  })
  
  # Collect the county name for emissions table title 
  output$emissions_table_title <- renderText({
    req(input$map_marker_click, monitor_data)  # Ensure a site is clicked and data is available
    
    print("🔍 Checking monitor_data for county name...")
    
    # Extract the county name based on the clicked monitor ID
    county_name <- monitor_data %>%
      filter(monitor_id == input$map_marker_click$id) %>%
      pull(County)  # Adjust column name if necessary
    
    # Debugging step to check extracted county name
    print(paste("Selected County:", county_name))
    
    # If a valid county name exists, format the title dynamically
    if (length(county_name) > 0 && !is.na(county_name)) {
      paste("Top 25 SCCs by annual county emissions in the 2022v1 inventory (tons/year):", county_name)
    } else {
      "Top 25 SCCs by annual county emissions in the 2022v1 inventory (tons/year):"  # Default title if no county is found
    }
  })
  
  # Render monitor details table
  output$monitor_details <- renderTable({
    selected_data()$monitor
  })
  
  # Render county base emissions pie chart
  output$base_emissions_pie <- renderPlotly({
    req(input$map_marker_click, selected_data()$base_emissions)
    
    print("Rendering Base Emissions Pie Chart...")  # Debugging message
    base_emissions_data <- selected_data()$base_emissions
    
    if (nrow(base_emissions_data) == 0) {
      print("🚨 No data available for this county!")
      return(plotly_empty())
    }
    
    # Calculate percentage for hover text
    base_emissions_data <- base_emissions_data %>%
      mutate(Percentage = (Total_2022_Emissions / sum(Total_2022_Emissions, na.rm = TRUE)) * 100,
             HoverText = paste0(Sector, ": ", round(Total_2022_Emissions,1), " tons/year (",round(Percentage, 1), "%)"))
    
    print("✅ Data for Interactive  Base Emissions Pie Chart:")
    print(head(base_emissions_data))
    
    # Create interactive pie chart
    plot_ly(base_emissions_data, labels = ~Sector, values = ~Total_2022_Emissions, type = "pie",
            textinfo = "none",  # Remove text from the chart
            hoverinfo = "text",  # Show only on hover
            text = ~HoverText,  # Define the hover text
            marker = list(line = list(color = "#FFFFFF", width = 1)))
  })
  
  # Render future county emissions pie chart
  output$future_emissions_pie <- renderPlotly({
    req(input$map_marker_click, selected_data()$future_emissions)
    
    print("Rendering Future Emissions Pie Chart...")  # Debugging message
    future_emissions_data <- selected_data()$future_emissions
    
    if (nrow(future_emissions_data) == 0) {
      print("🚨 No data available for this county!")
      return(plotly_empty())
    }
    
    # Calculate percentage for hover text
    future_emissions_data <- future_emissions_data %>%
      mutate(Percentage = (Total_2026_Emissions / sum(Total_2026_Emissions, na.rm = TRUE)) * 100,
             HoverText = paste0(Sector, ": ", round(Total_2026_Emissions,1), " tons/year (",round(Percentage, 1), "%)"))
    
    print("✅ Data for Interactive  FutureE missions Pie Chart:")
    print(head(future_emissions_data))
    
    # Create interactive pie chart
    plot_ly(future_emissions_data, labels = ~Sector, values = ~Total_2026_Emissions, type = "pie",
            textinfo = "none",  # Remove text from the chart
            hoverinfo = "text",  # Show only on hover
            text = ~HoverText,  # Define the hover text
            marker = list(line = list(color = "#FFFFFF", width = 1)))
  })
  
  # Render top SCCs table
  output$top_scc_table <- renderTable({
    data <- selected_data()$top_scc
    if (is.null(data) || nrow(data) == 0) {
      print("🚨 No SCC data available for this county!")  
      return(data.frame(SCC = character(), Description = character(), 
                        Sector = character(),
                        Total_2022_Emissions = numeric(), 
                        Total_2026_Emissions = numeric()))
    }
    
    print("✅ Displaying Top 25 SCCs Table:")
    print(head(data))

      data %>%
        select(SCC, Description, Sector,Total_2022_Emissions, Total_2026_Emissions) %>%
        arrange(desc(Total_2022_Emissions))
  })
  
   # Render the region tracer pie chart
  output$apca_region_pie <- renderPlotly({
    req(input$map_marker_click, region_tags)
    
    print("Rendering Region Pie Chart")  
    clicked_id <- as.character(input$map_marker_click$id)
    
    site_regiontags <- region_tags %>%
      filter(monitor_id == clicked_id) %>%
      pivot_longer(cols = 4:ncol(region_tags), names_to = "Tracer", values_to = "Concentration") %>%
      rename(site_id = 1) %>%
      filter(!is.na(Concentration) & is.finite(Concentration))
    
    if (nrow(site_regiontags) == 0) {
      print("🚨 No data found for this monitor in region_tags!")
      return(plotly_empty())
    }
    
    # Calculate percentage
    site_regiontags <- site_regiontags %>%
      mutate(Percentage = (Concentration / sum(Concentration, na.rm = TRUE)) * 100,
             HoverText = paste0(Tracer, ": ", round(Percentage, 1), "%"))
    
    print("✅ Data for Interactive Pie Chart:")
    print(head(site_regiontags))
    
    # Use plotly::plot_ly() to create an interactive pie chart
    plot_ly(site_regiontags, labels = ~Tracer, values = ~Concentration, type = "pie",
            textinfo = "none", hoverinfo = "text",
            text = ~HoverText, marker = list(line = list(color = "#FFFFFF", width = 1)))
  })
  
     # Render the sector tracer pie chart
  output$apca_sector_pie <- renderPlotly({
    req(input$map_marker_click, sector_tags)
    
    print("Rendering Sector Pie Chart")  
    clicked_id <- as.character(input$map_marker_click$id)
    
    site_sectortags <- sector_tags %>%
      filter(monitor_id == clicked_id) %>%
      pivot_longer(cols = 7:ncol(sector_tags), names_to = "Tracer", values_to = "Concentration") %>%
      rename(site_id = 1) %>%
      filter(!is.na(Concentration) & is.finite(Concentration))
    
    if (nrow(site_sectortags) == 0) {
      print("🚨 No data found for this monitor in sector_tags!")
      return(plotly_empty())
    }
    
    # Calculate percentage
    site_sectortags <- site_sectortags %>%
      mutate(Percentage = (Concentration / sum(Concentration, na.rm = TRUE)) * 100,
             HoverText = paste0(Tracer, ": ", round(Percentage, 1), "%"))
    
    print("✅ Data for Interactive Pie Chart:")
    print(head(site_sectortags))
    
    # Use plotly::plot_ly() to create an interactive pie chart
    plot_ly(site_sectortags, labels = ~Tracer, values = ~Concentration, type = "pie",
            textinfo = "none", hoverinfo = "text",
            text = ~HoverText, marker = list(line = list(color = "#FFFFFF", width = 1)))
  })
}
