#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI --------------------------------------------------------------------
rm(list = ls()) ;gc()

## LIBRARIES ----

# Shiny
library(shiny)
library(bslib)

library(shinydashboard)
library(shinydashboardPlus)

# Widgets
library(plotly)

# Core
library(tidyverse)
library(data.table)
library(sf)
library(DT)

dat <- fread("./predictions2020.csv")

countyDatMap  <- read_rds("./SimplifiedCountyDatMap.rds")

counties <- countyDatMap %>%
  distinct(stateCountyCode = substr( GEOID,1,5),
           County = sub("^([^,]+),\\s*([^,]+),.*", "\\2", NAME)) %>%
  arrange(County) %>%
  filter(!is.na(stateCountyCode))

dat %>%
  mutate(stateCountyCode = substr( GEOID,1,4),
         stateCountyCode = paste0(0,stateCountyCode)) %>%
  left_join(counties) -> dat

st_centroid_within_poly <- function (poly) {

  # check if centroid is in polygon
  centroid <- poly %>% st_centroid()
  in_poly <- st_within(centroid, poly, sparse = F)[[1]]

  # if it is, return that centroid
  if (in_poly) return(centroid)

  # if not, calculate a point on the surface and return that
  centroid_in_poly <- st_point_on_surface(poly)
  return(centroid_in_poly)
}

# countyDatMap <- countyDatMap  %>%
#   filter(!is.na(GEOID)) %>%
#   st_as_sf %>%
#   rmapshaper::ms_simplify(., keep = 0.1)


# Define UI for application that draws a histogram
fluidPage(

  theme = bs_theme(base_font =  c("sans-serif",
                                  "Roboto"),
                   # Controls the default grayscale palette
                   bg = "#202123", fg = "#B8BCC2",
                   # Controls the accent (e.g., hyperlink, button, etc) colors
                   primary = "#B8BCC2", secondary = "#48DAC6",
                   code_font = c("Courier", "monospace"),
                   heading_font = "'Helvetica Neue', Helvetica, sans-serif",
                   # Can also add lower-level customization
                   "input-border-color" = "#B8BCC2"),

  titlePanel("CalFresh Participation Estimate vs Predicted"),

  sidebarLayout(

    # Inputs
    sidebarPanel(
      width = 3,
      # Filter
      selectInput(inputId = "dataset_choice",
                  label = "Select County",
                  choices =  counties$County,
                  selected = ""),

      # Show data table
      checkboxInput(inputId = "Table",
                    label = "Show table",
                    value = TRUE), #TRUE table displays

      downloadButton("downloadData", "Download records") # download button

    ),

    # Output: Show plot
    mainPanel(
      plotlyOutput(outputId = "graph", height = 700),
      # Show data table
      dataTableOutput(outputId = "data")
    )
  )
)
