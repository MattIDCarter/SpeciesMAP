# ============================================================
# USER INTERFACE
# ============================================================


library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)


# ------------------------------------------------------------
# Logo helper
# ------------------------------------------------------------

logo_element <- function(
    logo
) {
  
  image <- tags$img(
    
    src = file.path(
      "images",
      logo$file
    ),
    
    alt = logo$alt,
    
    width = logo$width,
    
    style = "max-width: 100%; height: auto;"
    
  )
  
  
  if (!is.null(logo$url)) {
    
    tags$a(
      href = logo$url,
      target = "_blank",
      image
    )
    
  } else {
    
    image
    
  }
  
}


# ------------------------------------------------------------
# Sidebar
# ------------------------------------------------------------

app_sidebar <- function() {
  
  logo_elements <- list()
  
  
  # Application logo
  logo_elements <- append(
    logo_elements,
    list(
      tags$div(
        class = "sidebar-logo",
        style = "text-align: center; margin-top: 10px;",
        logo_element(LOGOS$app)
      )
    )
  )
  
  
  # Funder/institution logos
  if (length(LOGOS$funders) > 0) {
    
    for (logo in LOGOS$funders) {
      
      logo_elements <- append(
        logo_elements,
        list(
          tags$div(
            style = "text-align: center; margin-top: 10px;",
            class = "sidebar-logo",
            logo_element(logo)
          )
        )
        
      )
      
    }
    
  }
  
  
  dashboardSidebar(
    
    width = 250,
    
    
    sidebarMenu(
      
      id = "sidebarMenu",
      
      menuItem(
        "Interactive Maps",
        tabName = "dashboard",
        icon = icon("map-location-dot")
      ),
      
      menuItem(
        "Methods Overview",
        tabName = "methods",
        icon = icon("gear")
      ),
      
      menuItem(
        "Estimates for Area of Interest",
        tabName = "extraction",
        icon = icon("sliders")
      ),
      
      menuItem(
        "Download Data",
        tabName = "data",
        icon = icon("database")
      ),
      
      menuItem(
        "Further Resources",
        tabName = "pubs",
        icon = icon("book")
      ),
      
      menuItem(
        "Acknowledgements",
        tabName = "acknowledgements",
        icon = icon("pen")
      ),
      
      menuItem(
        "Contact",
        tabName = "contact",
        icon = icon("address-book")
      ),
      
      menuItem(
        "Frequently Asked Questions",
        tabName = "faqs",
        icon = icon("question")
      )
      
    ),
    
    
    tags$div(
      class = "sidebar-logos",
      logo_elements
    )
    
  )
  
}


# ------------------------------------------------------------
# Interactive maps tab
# ------------------------------------------------------------

dashboard_tab <- function(
    map_data
) {
  
  tabItem(
    
    tabName = "dashboard",
    
    
    fluidRow(
      
      box(
        
        width = 12,
        
        title = strong(APP_TITLE),
        
        includeHTML(
          "www/static/html/intro.html"
        ),
        
        downloadButton(
          "downloadDocsDashboard",
          "Download Documentation"
        )
        
      )
      
    ),
    
    
    fluidRow(
      
      box(
        
        width = 12,
        
        title = strong("Interactive Maps"),
        
        
        radioButtons(
          
          inputId = "selectSpecs",
          
          label = NULL,
          
          selected = map_data$code[1],
          
          inline = FALSE,
          
          width = "100%",
          
          choiceNames = map_data$choice,
          
          choiceValues = map_data$code
          
        ),
        
        
        tags$p(
          tags$em(
            MAP_DESCRIPTION
          )
        ),
        
        
        shinycssloaders::withSpinner(
          
          leafletOutput(
            "map",
            height = 700
          ),
          
          hide.ui = FALSE
          
        ),
        
        
        tags$small(
          "Note: The map may not load under certain institutional web access configurations."
        ),
        
        tags$footer(
          style="position: fixed; bottom: 0; width: 100%; background-color: #f1f1f1; text-align: left; padding: 7px;",
          p(style="display: inline;", "App built with the ", a(href="https://arts.st-andrews.ac.uk/shiny/smru/sealmap/", "SealMAP", target="_blank", style="display: inline;"), " framework")
        )
        
      )
      
    )
    
  )
  
}


# ------------------------------------------------------------
# Methods tab
# ------------------------------------------------------------

methods_tab <- function() {
  
  tabItem(
    
    tabName = "methods",
    
    fluidRow(
      
      box(
        
        width = 12,
        
        title = strong("Methods Overview"),
        
        includeHTML(
          "www/static/html/methods.html"
        )
        
      )
      
    )
    
  )
  
}


# ------------------------------------------------------------
# AOI estimation tab
# ------------------------------------------------------------

extraction_tab <- function(
    map_data
) {
  
  tabItem(
    
    tabName = "extraction",
    
    
    fluidRow(
      
      box(
        
        width = 12,
        
        title = strong(
          "Estimate values for an Area of Interest"
        ),
        
        
        includeHTML(
          "www/static/html/extraction.html"
        )
        ),
        
        box(
          
          width = 12,
          
          tags$ol(
            
            tags$li(
              
              h4("Define your Area of Interest"),
              
              radioButtons(
                
                inputId = "aoi_method",
                
                label = NULL,
                
                choices = c(
                  
                  "Upload a polygon" = "upload",
                  
                  "Draw a polygon" = "draw",
                  
                  "Enter coordinates" = "coordinates"
                  
                ),
                
                selected = "upload"
                
              )
              
            ),
            
            
            conditionalPanel(
              
              condition = "input.aoi_method == 'upload'",
              
              fileInput(
                
                inputId = "aoi_files",
                multiple = TRUE,
                label = "Upload AOI",
                accept = c(
                  ".shp",
                  ".shx",
                  ".prj",
                  ".zip",
                  ".geojson",
                  ".gpkg",
                  ".kml"
                )
                
              ),
              
              tags$small(
                "For a shapefile, select the .shp, .shx and .prj files together. GeoJSON, GeoPackage and KML files can be uploaded individually."
              )
              
            ),
            
            
            conditionalPanel(
              
              condition = "input.aoi_method == 'draw'",
              
              leafletOutput(
                
                "drawMap",
                
                height = 600
                
              )
              
            ),
            
            
            conditionalPanel(
              
              condition = "input.aoi_method == 'coordinates'",
              
              textAreaInput(
                
                inputId = "coordinates",
                
                label = "Enter coordinates",
                
                placeholder =
                  "Enter longitude, latitude pairs separated by new lines.\n\nExample:\n-3.0, 55.0\n-2.0, 55.0\n-2.0, 54.0\n-3.0, 55.0",
                
                rows = 8
                
              ),
              
              tags$small(
                
                "Coordinates should be entered as longitude, latitude pairs. Polygons must have at least 3 unique vertices."
                
              )
              
            ),
            
            
            tags$li(
              
              h4("Select Species"),
              
              radioButtons(
                
                inputId = "selectSpecsEstimate",
                
                label = NULL,
                
                selected = map_data$code[1],
                
                choiceNames = map_data$choice,
                
                choiceValues = map_data$code,
                
                inline = FALSE
                
              )
              
            ),
            
            
            tags$li(
              
              h4(
                "Would you like to add a buffer?"
              ),
              
              selectInput(
                
                inputId = "buffer_choice",
                
                label = NULL,
                
                choices = c(
                  "No",
                  "Yes"
                ),
                
                selected = "No"
                
              )
              
            ),
            
            
            conditionalPanel(
              
              condition =
                "input.buffer_choice == 'Yes'",
              
              numericInput(
                
                inputId = "buffer_dist",
                
                label = "Buffer distance (km)",
                
                value = AOI$default_buffer,
                
                min = AOI$buffer_min,
                
                max = AOI$buffer_max,
                
                step = 0.5
                
              )
              
            ),
            
            
            tags$li(
              
              actionButton(
                "calculate",
                "Calculate",
                icon = icon("calculator")
              ),
              
              actionButton(
                "reset",
                "Reset",
                icon = icon("rotate-left")
              )
              
            )
            
          )
        
      )
      
    ),
    
    
    fluidRow(
      
      box(
        
        width = 12,
        
        title = strong(
          "Calculated estimate"
        ),
        
        uiOutput(
          "estimate"
        ),
        
        includeHTML(
          "www/static/html/considerations.html"
        ),
        
        downloadButton(
          "downloadDocsAOI",
          "Download Documentation",
          class = "download-preparing"
        ),
        
        leafletOutput(
          "map2",
          height = 700
        )
        
      )
      
    )
    
  )
  
}


# ------------------------------------------------------------
# Download tab
# ------------------------------------------------------------

data_tab <- function() {
  
  tabItem(
    
    tabName = "data",
    
    fluidRow(
      
      box(
        
        width = 12,
        
        title = strong(
          "Download Data"
        ),
        
        tags$p(
          "Download the distribution data used by this application."
        ),
        
        downloadButton(
          "downloadData",
          "Download Data"
        )
        
      )
      
    )
    
  )
  
}


# ------------------------------------------------------------
# Static content tabs
# ------------------------------------------------------------

content_tab <- function(
    tab_name,
    title,
    file
) {
  
  tabItem(
    
    tabName = tab_name,
    
    fluidRow(
      
      box(
        
        width = 12,
        
        title = strong(title),
        
        includeHTML(file)
        
      )
      
    )
    
  )
  
}


# ------------------------------------------------------------
# Complete application UI
# ------------------------------------------------------------

app_ui <- function() {
  
  # Load map metadata so that UI choices can be constructed
  map_data <- load_map_data()
  
  dashboardPage(
    
    skin = APP_SCHEME,
    
    
    dashboardHeader(
      
      titleWidth = 250,
      
      title = APP_NAME
      
    ),
    
    
    app_sidebar(),
    
    
    dashboardBody(
      
      useShinyjs(),
      
      
      includeCSS(
        "www/static/css/styles.css"
      ),
      
      
      includeScript(
        "www/static/js/app.js"
      ),
      
      
      tabItems(
        
        dashboard_tab(
          map_data
        ),
        
        methods_tab(),
        
        extraction_tab(
          map_data
        ),
        
        data_tab(),
        
        content_tab(
          "pubs",
          "Further Resources",
          "www/static/html/resources.html"
        ),
        
        content_tab(
          "acknowledgements",
          "Acknowledgements",
          "www/static/html/acknowledgements.html"
        ),
        
        content_tab(
          "contact",
          "Contact",
          "www/static/html/contact.html"
        ),
        
        content_tab(
          "faqs",
          "Frequently Asked Questions",
          "www/static/html/faqs.html"
        )
        
      )
      
    )
    
  )
  
}