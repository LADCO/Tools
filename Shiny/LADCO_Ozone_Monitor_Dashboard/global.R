# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)

# Load datasets
# Monitoring Data
monitor_data <- read.csv("./Ozone_monitors-LADCOv2.csv", 
                         stringsAsFactors = FALSE, 
                         na.strings = c("", "NA"),   # Treat empty strings and "NA" as NA
                         strip.white = TRUE)         # Remove leading/trailing white space

# Emissions Data
emissions_data <- read.csv("./2022v1_Emissions_EPA-County_LADCO_v2.csv")
emissions_data <- emissions_data %>%
  mutate(SCC = as.character(SCC))

scc_data <- read.csv("./SCCs.csv")
scc_data <- scc_data %>%
  mutate(SCC = as.character(SCC))

# Merge emissions data with SCC descriptions
emissions_data <- emissions_data %>%
  left_join(scc_data, by = "SCC")

# LADCO 2016 CAMx APCA Region Tags
region_tags <- read.csv("./LADCO_2016_APCA_Tracers_07Jun2023_RegionTags.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE, 
                        na.strings = c("", "NA"),   # Treat empty strings and "NA" as NA
                        strip.white = TRUE)         # Remove leading/trailing white space

# LADCO 2016 CAMx APCA Sector Tags
sector_tags <- read.csv("./LADCO_2016_APCA_Tracers_07Jun2023_SectorTags.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE, 
                        na.strings = c("", "NA"),   # Treat empty strings and "NA" as NA
                        strip.white = TRUE)         # Remove leading/trailing white space
sector_tags <- sector_tags %>%
  mutate(monitor_id = as.character(monitor_id))