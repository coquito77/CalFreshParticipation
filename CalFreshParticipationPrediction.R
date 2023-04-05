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

dat <- fread("./data/predictions2020.csv")

countyDatMap  <- read_rds("./data/SimplifiedCountyDatMap.rds")

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

countyDatMap  <- read_rds("./data/SimplifiedCountyDatMap.rds")

ui <- fluidPage(

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

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {

  output$graph <- renderPlotly({

    pal_bg <-'#202123'
    pal_annotate <- '#B6B6B6'

    mapData <- countyDatMap %>%
      filter(grepl(input$dataset_choice, NAME)) %>%
      #filter(grepl("Ventura", NAME)) %>%
      left_join(dat %>%
                  filter(grepl(input$dataset_choice, County)) %>%
                  #filter(grepl("Ventura", County)) %>%
                  mutate(Diff_estimt_msn_ensemble = hhSNAP - .ensemblePred ) %>%
                  select(GEOID, Diff_estimt_msn_ensemble) %>%
                  mutate(GEOID = paste0(0, GEOID)))

    centroid_dat <- mapData %>%
      distinct(GEOID, Diff_estimt_msn_ensemble, geometry) %>%
      mutate(lon = map_dbl(geometry, ~st_centroid_within_poly(.x)[[1]]),
             lat = map_dbl(geometry, ~st_centroid_within_poly(.x)[[2]])) %>%
      st_set_geometry(., NULL)

    g <- mapData %>%
      ggplot +
      geom_sf(aes(fill = Diff_estimt_msn_ensemble), linewidth = .1) +
      geom_point(data = centroid_dat #%>%  filter(city_name != "Unincorporated")
                 ,aes(label = GEOID , y = lat, x = lon, color = Diff_estimt_msn_ensemble),
                 size =.1) +
      # geom_sf(data = centroid_dat,
      #         aes(label = GEOID, geometry = centroids, color = Diff_estimt_msn_ensemble),
      #         linewidth = .1, size = .1, show.legend = FALSE) +
      # geom_sf_text(aes(label = GEOID), alpha = .3, size = 1,
      #              check_overlap = TRUE) +
      scale_fill_gradient2(name =
                             "Difference between\nU.S. Census Estimate and\nEnsemble Prediction",
                           low = "#075AFF",
                           mid = "#FFFFCC",
                           high = "#FF0000") +
      scale_color_gradient2(name =
                             "Difference between\nU.S. Census Estimate and\nEnsemble Prediction",
                           low = "#075AFF",
                           mid = "#FFFFCC",
                           high = "#FF0000") +
    hrbrthemes::theme_ipsum_rc(plot_title_size = 15, subtitle_size = 12, caption_size = 10) +
      theme(panel.background = element_rect(fill = pal_bg, color=NA),
            plot.background = element_rect(fill = pal_bg),
            #axis.text.x = element_text(size = 10,hjust = -.2),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_blank(),  #remove x axis labels
            axis.ticks.x = element_blank(), #remove x axis ticks
            axis.text.y = element_blank(),  #remove y axis labels
            axis.ticks.y = element_blank() , #remove y axis ticks
            legend.key.width = unit(.85, "cm"),
            strip.text.x = element_text(size = 4),
            text = element_text(colour = pal_annotate)
            #axis.text = element_blank(),# element_text(angle = 00, hjust = -0.5, size = 10),
            ,panel.grid.major = element_blank() #,
            ,panel.grid.minor = element_blank()
            #,axis.ticks.x = element_line(size = .4)
            #,axis.ticks.length = unit(0.3, "cm")
            ,legend.position = "top"
            ,panel.spacing = unit(.5, "lines")
      )

    plotly::ggplotly(g)

  })

  # Print data table if checked
  output$data <- DT::renderDataTable({
    if(input$Table){
      DT::datatable(data = dat %>%
                      filter(grepl(input$dataset_choice, County)) %>%
                      #filter(grepl("Los Angeles", County)) %>%
                      select(-c(stateCountyCode, County)) %>%
                      select(GEOID, everything()) %>%
                      mutate(Diff_estimt_msn_ensemble = hhSNAP - .ensemblePred,
                             GEOID = as.character(GEOID)) %>%
                      bind_rows( summarise(., across(where(is.numeric), ~ sum(., na.rm = TRUE ) )),.) %>%
                      `[[<-`(1, 12, value = "County Total") %>%
                      rename("US_Census SNAP_Estimate" = hhSNAP,
                             "Difference Estimate minus Ensamble" = Diff_estimt_msn_ensemble),
                    options = list(pageLength = 10),
                    rownames = FALSE) %>%
        formatRound(c("US_Census SNAP_Estimate", ".pred_randFrs", ".sb_pred_randFrs",
                      ".pred_lm", ".lwr_lm_fit", ".upr_lm_fit", ".sbpred_lm",
                      ".lwr_lm_sb_fit", ".upr_lm_sb_fit", ".ensemblePred",
                      "Difference Estimate minus Ensamble"),
                    digits = 0)
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      fwrite(dat %>%
                  filter(grepl(input$dataset_choice, County)) %>%
                  #filter(grepl("Los Angeles", County)) %>%
                  #select(-c( County)) %>%
                  select(GEOID, everything()) %>%
                  mutate(Diff_estimt_msn_ensemble = hhSNAP - .ensemblePred,
                         GEOID = as.character(GEOID)) %>%
                  bind_rows( summarise(., across(where(is.numeric), ~ sum(., na.rm = TRUE ) )),.) %>%
                  `[[<-`(1, 12, value = "County Total") %>%
                  rename("US_Census SNAP_Estimate" = hhSNAP,
                         "Difference Estimate minus Ensamble" = Diff_estimt_msn_ensemble), file)
    }
  )

}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
