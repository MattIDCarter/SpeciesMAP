# ============================================================
# SERVER
# ============================================================


library(shiny)
library(sf)
library(terra)
library(raster)
library(exactextractr)
library(leaflet)
library(leafem)
library(leaflet.extras)


# ------------------------------------------------------------
# Helper: find selected map
# ------------------------------------------------------------

get_selected_map <- function(
    code,
    map_data
) {
  
  i <- which(
    map_data$code == code
  )
  
  
  if (length(i) != 1) {
    
    stop(
      "Could not identify the selected map: ",
      code
    )
    
  }
  
  
  map_data[i, , drop = FALSE]
  
}



# ------------------------------------------------------------
# Read uploaded AOI
# ------------------------------------------------------------

read_uploaded_aoi <- function(files) {
  
  req(files)
  
  extensions <- tolower(
    tools::file_ext(files$name)
  )
  
  
  # ----------------------------------------------------------
  # Check for unsupported files
  # ----------------------------------------------------------
  
  supported <- c(
    "shp",
    "shx",
    "prj",
    "geojson",
    "gpkg",
    "kml"
  )
  
  unsupported <- setdiff(
    extensions,
    supported
  )
  
  if (length(unsupported) > 0) {
    
    stop(
      "Unsupported file type: ",
      paste(
        paste0(".", unsupported),
        collapse = ", "
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Single-file formats
  # ----------------------------------------------------------
  
  single_file_formats <- c(
    "geojson",
    "gpkg",
    "kml"
  )
  
  
  single_files <- files[
    extensions %in% single_file_formats,
    ,
    drop = FALSE
  ]
  
  
  if (nrow(single_files) > 0) {
    
    if (nrow(single_files) > 1) {
      
      stop(
        "Please upload only one GeoJSON, GeoPackage or KML file."
      )
      
    }
    
    
    return(
      sf::st_read(
        single_files$datapath,
        quiet = TRUE
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Shapefile
  # ----------------------------------------------------------
  
  shp_extensions <- c(
    "shp",
    "shx",
    "prj"
  )
  
  
  shp_files <- files[
    extensions %in% shp_extensions,
    ,
    drop = FALSE
  ]
  
  
  # Required components
  missing <- setdiff(
    shp_extensions,
    extensions
  )
  
  
  if (length(missing) > 0) {
    
    stop(
      "The shapefile is incomplete. Please upload: ",
      paste(
        paste0(".", missing),
        collapse = ", "
      )
    )
    
  }
  
  
  # Exactly one of each
  if (
    sum(extensions == "shp") != 1 ||
    sum(extensions == "shx") != 1 ||
    sum(extensions == "prj") != 1
  ) {
    
    stop(
      "Please upload exactly one .shp, one .shx and one .prj file."
    )
    
  }
  
  
  shp_file <- shp_files[
    extensions[
      extensions %in% shp_extensions
    ] == "shp",
    ,
    drop = FALSE
  ]
  
  shx_file <- shp_files[
    extensions[
      extensions %in% shp_extensions
    ] == "shx",
    ,
    drop = FALSE
  ]
  
  prj_file <- shp_files[
    extensions[
      extensions %in% shp_extensions
    ] == "prj",
    ,
    drop = FALSE
  ]
  
  
  # ----------------------------------------------------------
  # Check matching filenames
  # ----------------------------------------------------------
  
  shp_base <- tools::file_path_sans_ext(
    basename(shp_file$name)
  )
  
  shx_base <- tools::file_path_sans_ext(
    basename(shx_file$name)
  )
  
  prj_base <- tools::file_path_sans_ext(
    basename(prj_file$name)
  )
  
  
  if (
    !identical(shp_base, shx_base) ||
    !identical(shp_base, prj_base)
  ) {
    
    stop(
      "The .shp, .shx and .prj files must have the same filename."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Create temporary directory
  # ----------------------------------------------------------
  
  tmp_dir <- tempfile(
    "aoi_shapefile_"
  )
  
  dir.create(
    tmp_dir
  )
  
  
  # ----------------------------------------------------------
  # Copy components into same directory
  # ----------------------------------------------------------
  
  file.copy(
    shp_file$datapath,
    file.path(
      tmp_dir,
      basename(shp_file$name)
    )
  )
  
  file.copy(
    shx_file$datapath,
    file.path(
      tmp_dir,
      basename(shx_file$name)
    )
  )
  
  file.copy(
    prj_file$datapath,
    file.path(
      tmp_dir,
      basename(prj_file$name)
    )
  )
  
  
  # ----------------------------------------------------------
  # Read shapefile
  # ----------------------------------------------------------
  
  sf::st_read(
    file.path(
      tmp_dir,
      basename(shp_file$name)
    ),
    quiet = TRUE
  )
  
}


# ------------------------------------------------------------
# Helper: convert uploaded/drawn AOI to polygon
# ------------------------------------------------------------

prepare_aoi <- function(
    aoi,
    buffer_choice = "No",
    buffer_dist = NULL
) {
  
  req(aoi)
  
  
  # Ensure sf
  aoi <- sf::st_as_sf(aoi)
  
  
  # Remove empty geometries
  aoi <- aoi[
    !sf::st_is_empty(aoi),
    ,
    drop = FALSE
  ]
  
  
  if (nrow(aoi) == 0) {
    
    stop(
      "The supplied AOI does not contain a valid geometry."
    )
    
  }
  
  
  # Ensure polygon geometry
  geometry_type <- unique(
    as.character(
      sf::st_geometry_type(aoi)
    )
  )
  
  
  if (!any(
    geometry_type %in%
    c(
      "POLYGON",
      "MULTIPOLYGON"
    )
  )) {
    
    stop(
      "The AOI must contain polygon geometry."
    )
    
  }
  
  
  # If CRS is missing, assume geographic coordinates
  if (is.na(sf::st_crs(aoi))) {
    
    sf::st_crs(aoi) <- CRS_MAP
    
  }
  
  
  # Transform to analysis CRS
  aoi <- sf::st_transform(
    aoi,
    CRS_ANALYSIS
  )
  
  
  # Combine multiple features
  aoi <- sf::st_union(
    aoi
  )
  
  
  # Make valid
  aoi <- sf::st_make_valid(
    aoi
  )
  
  # Store original polygon before buffering
  original_aoi <- aoi
  
  
  # Buffer
  if (
    buffer_choice == "Yes" &&
    !is.null(buffer_dist) &&
    is.finite(buffer_dist)
  ) {
    
    aoi <- sf::st_buffer(
      aoi,
      dist = buffer_dist * 1000
    )
    
  }
  
  # Return both geometries
  
  list(
    
    aoi =
      sf::st_as_sf(aoi),
    
    original_aoi =
      sf::st_as_sf(original_aoi)
    
  )
  
}


# ------------------------------------------------------------
# Helper: coordinates -> polygon
# ------------------------------------------------------------

coordinates_to_polygon <- function(
    text
) {
  
  req(text)
  
  
  lines <- strsplit(
    text,
    "\\r?\\n"
  )[[1]]
  
  
  lines <- trimws(
    lines
  )
  
  
  lines <- lines[
    nzchar(lines)
  ]
  
  
  if (length(lines) < 3) {
    
    stop(
      "At least three coordinates are required."
    )
    
  }
  
  
  coords <- do.call(
    rbind,
    lapply(
      lines,
      function(x) {
        
        values <- strsplit(
          x,
          "[,;[:space:]]+"
        )[[1]]
        
        values <- values[
          nzchar(values)
        ]
        
        if (length(values) < 2) {
          
          return(
            c(
              NA_real_,
              NA_real_
            )
          )
          
        }
        
        as.numeric(
          values[1:2]
        )
        
      }
    )
  )
  
  
  if (any(!is.finite(coords))) {
    
    stop(
      "One or more coordinates could not be interpreted."
    )
    
  }
  
  
  # Check coordinate limits
  if (
    any(
      coords[, 1] < AOI$longitude[1] |
      coords[, 1] > AOI$longitude[2]
    )
  ) {
    
    stop(
      "One or more longitude values are outside the allowed range."
    )
    
  }
  
  
  if (
    any(
      coords[, 2] < AOI$latitude[1] |
      coords[, 2] > AOI$latitude[2]
    )
  ) {
    
    stop(
      "One or more latitude values are outside the allowed range."
    )
    
  }
  
  
  # Remove duplicated coordinates
  coords <- unique(
    coords
  )
  
  
  if (nrow(coords) < 3) {
    
    stop(
      "At least three unique coordinate pairs are required."
    )
    
  }
  
  
  # Close polygon if necessary
  if (
    !all(
      coords[1, ] == coords[nrow(coords), ]
    )
  ) {
    
    coords <- rbind(
      coords,
      coords[1, ]
    )
    
  }
  
  
  polygon <- sf::st_polygon(
    list(
      coords
    )
  )
  
  
  sf::st_sf(
    geometry = sf::st_sfc(
      polygon,
      crs = CRS_MAP
    )
  )
  
}


# ------------------------------------------------------------
# Calculate mean raster value within AOI
# ------------------------------------------------------------

calculate_aoi_estimate <- function(
    aoi,
    map_info,
    unc_data = NULL
) {
  
  
  # ----------------------------------------------------------
  # Load raster
  # ----------------------------------------------------------
  
  raster <- load_map_raster(
    map_info
  )
  
  
  # ----------------------------------------------------------
  # Transform AOI to raster CRS
  # ----------------------------------------------------------
  
  aoi_raster <- sf::st_transform(
    aoi,
    terra::crs(raster)
  )
  
  
  # ----------------------------------------------------------
  # Calculate mean estimate
  # ----------------------------------------------------------
  
  if (AOI$coverage_weighted) {
    
    
    estimate <- exactextractr::exact_extract(
      
      raster,
      
      aoi_raster,
      
      function(
    values,
    coverage_fraction
      ) {
        
        valid <-
          is.finite(values) &
          is.finite(coverage_fraction)
        
        
        if (!any(valid)) {
          
          return(NA_real_)
          
        }
        
        
        sum(
          
          values[valid] *
            coverage_fraction[valid],
          
          na.rm = TRUE
          
        )
        
      }
    
    )
    
    
  } else {
    
    
    values <- terra::extract(
      
      raster,
      
      terra::vect(aoi_raster)
      
    )
    
    
    if (
      is.null(values) ||
      ncol(values) < 2
    ) {
      
      stop(
        "No raster values could be extracted from the AOI."
      )
      
    }
    
    
    x <- values[[2]]
    
    
    x <- x[
      is.finite(x)
    ]
    
    
    if (length(x) == 0) {
      
      stop(
        "The AOI does not overlap valid raster data."
      )
      
    }
    
    
    estimate <- sum(
      x,
      na.rm = TRUE
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Check estimate
  # ----------------------------------------------------------
  
  if (
    length(estimate) == 0 ||
    all(is.na(estimate))
  ) {
    
    stop(
      "The AOI does not overlap valid raster data."
    )
    
  }
  
  
  estimate <- sum(
    estimate,
    na.rm = TRUE
  )
  
  
  # ==========================================================
  # AREA-BASED UNCERTAINTY
  # ==========================================================
  
  lower_CI <- NA_real_
  
  upper_CI <- NA_real_
  
  
  if (AOI$area_CI) {
    
    
    # --------------------------------------------------------
    # Check uncertainty lookup table
    # --------------------------------------------------------
    
    if (is.null(unc_data)) {
      
      stop(
        "Area-based uncertainty is enabled, but no ",
        "uncertainty lookup table was supplied."
      )
      
    }
    
    
    # --------------------------------------------------------
    # Load bootstrap matrix for this raster
    # --------------------------------------------------------
    
    bootstrap <- load_uncertainty_matrix(
      map_info = map_info,
      unc_data = unc_data
    )
    
    
    # --------------------------------------------------------
    # Check number of rows
    # --------------------------------------------------------
    
    if (
      nrow(bootstrap) !=
      terra::ncell(raster)
    ) {
      
      stop(
        "The uncertainty matrix for ",
        map_info$code,
        " has ",
        nrow(bootstrap),
        " rows, but the raster has ",
        terra::ncell(raster),
        " cells."
      )
      
    }
    
    
    # --------------------------------------------------------
    # Extract cell numbers and coverage fractions
    # --------------------------------------------------------
    
    extracted <- exactextractr::exact_extract(
      
      raster,
      
      aoi_raster,
      
      include_cell = TRUE
      
    )
    
    
    cells <- extracted[[1]]
    
    
    cell_numbers <-
      cells$cell
    
    
    # --------------------------------------------------------
    # Remove cells with no valid bootstrap predictions
    # --------------------------------------------------------
    
    boot <- bootstrap[
      cell_numbers,
      ,
      drop = FALSE
    ]
    
    
    valid <- apply(
      boot,
      1,
      function(x) {
        any(
          is.finite(x)
        )
      }
    )
    
    
    boot <- boot[
      valid,
      ,
      drop = FALSE
    ]
    
    
    cell_numbers <-
      cell_numbers[
        valid
      ]
    
    
    # --------------------------------------------------------
    # Coverage weights
    # --------------------------------------------------------
    
    if (AOI$coverage_weighted) {
      
      weights <-
        cells$coverage_fraction[
          valid
        ]
      
    } else {
      
      weights <-
        rep(
          1,
          length(cell_numbers)
        )
      
    }
    
    
    # --------------------------------------------------------
    # Replace invalid bootstrap predictions with zero
    # --------------------------------------------------------
    
    boot[
      !is.finite(boot)
    ] <- 0
    
    
    # --------------------------------------------------------
    # Sum cells for each bootstrap
    # --------------------------------------------------------
    
    bootstrap_totals <-
      colSums(
        
        boot *
          weights
        
      )
    
    
    # --------------------------------------------------------
    # Calculate CI
    # --------------------------------------------------------
    
    alpha <-
      1 -
      AOI$CI_level
    
    
    lower_CI <-
      as.numeric(
        quantile(
          bootstrap_totals,
          probs = alpha / 2,
          na.rm = TRUE
        )
      )
    
    
    upper_CI <-
      as.numeric(
        quantile(
          bootstrap_totals,
          probs = 1 - alpha / 2,
          na.rm = TRUE
        )
      )
    
  }
  
  
  # ==========================================================
  # Return
  # ==========================================================
  
  list(
    
    sum =
      estimate,
    
    lower_CI =
      lower_CI,
    
    upper_CI =
      upper_CI,
    
    CI_level =
      if (AOI$area_CI)
        AOI$CI_level
    else
      NULL,
    
    raster =
      raster
    
  )
  
}

# ============================================================
# APPLICATION SERVER
# ============================================================

app_server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Load application data
  # ----------------------------------------------------------
  
  app_data <- load_application_data()
  
  map_data <- app_data$map_data
  
  unc_data <- app_data$unc_data
  
  background <- app_data$background
  
  
  # ----------------------------------------------------------
  # AOI reactive values
  # ----------------------------------------------------------
  
  aoi <- reactiveVal(NULL)
  
  estimate <- reactiveVal(NULL)
  
  
  # ----------------------------------------------------------
  # Interactive map
  # ----------------------------------------------------------
  
  output$map <- renderLeaflet({
    
    create_base_map(
      background = background
    ) %>%
      
      add_mouse_coordinates()
    
  })
  
  
  # ----------------------------------------------------------
  # Update main map when species/category changes
  # ----------------------------------------------------------
  
  observeEvent(
    input$selectData,
    {
      
      req(
        input$selectData
      )
      
      
      map_info <- get_selected_map(
        input$selectData,
        map_data
      )
      
      
      withProgress(
        
        message = "Loading map...",
        
        value = 0,
        
        {
          
          r <- load_map_raster(
            map_info
          )
          
          
          incProgress(
            0.4
          )
          
          
          r <- prepare_map_raster(
            r,
            rounding = map_info$rounding
          )
          
          
          incProgress(
            0.4
          )
          
          
          map_palette <- create_map_palette(
            
            values =
              as.numeric(
                terra::values(r)
              ),
            
            method =
              MAP_PALETTE$method,
            
            n =
              MAP_PALETTE$n,
            
            upper_quantile =
              MAP_PALETTE$upper_quantile,
            
            palette =
              MAP_PALETTE$palette
            
          )
          
          
          # ----------------------------------------------------
          # Remove previous raster and controls
          # ----------------------------------------------------
          
          leafletProxy(
            "map"
          ) %>%
            
            clearImages() %>%
            
            clearControls()
          
          
          # ----------------------------------------------------
          # Add raster
          # ----------------------------------------------------
          
          leafletProxy(
            "map"
          ) %>%
            
            addRasterImage(
              
              r,
              
              colors =
                map_palette$palette,
              
              opacity = 1,
              
              group = "Density",
              
              layerId = "Density",
              
              maxBytes =
                200000000,
              
              project = FALSE,
              
              options = leafletOptions(
                pane = "density"
              )
              
            )
          
          
          # ----------------------------------------------------
          # Add land mask if requested
          # ----------------------------------------------------
          
          if (
            MAP_ENV$land_layer == "mask"
          ) {
            
            add_land_layer(
              
              map =
                leafletProxy("map"),
              
              background =
                background,
              
              pane =
                "land_mask"
              
            )
            
          }
          
          
          # ----------------------------------------------------
          # Add image query
          # ----------------------------------------------------
          
          leafletProxy(
            "map"
          ) %>%
            
            leafem::addImageQuery(
              
              raster::raster(r),
              
              layerId = "Density",
              
              type = "mousemove",
              
              digits =
                map_info$rounding,
              
              position = "topright",
              
              prefix = "Mean"
              
            ) %>%
            
            addLegend(
              
              position = "bottomright",
              
              pal =
                map_palette$palette,
              
              values =
                map_palette$breaks,
              
              opacity = 1,
              
              title =
                "Mean density",
              
              labFormat =
                labelFormat(
                  digits =
                    map_info$rounding
                )
              
            )
          
          
          incProgress(
            0.2
          )
          
        }
        
      )
      
    },
    
    ignoreNULL = FALSE
    
  )
  
  
  # ----------------------------------------------------------
  # AOI drawing map
  # ----------------------------------------------------------
  
  output$drawMap <- renderLeaflet({
    
    create_base_map(
      background = background
    ) %>%
      
      add_mouse_coordinates() %>%
      
      leaflet.extras::addDrawToolbar(
        
        targetGroup = "drawn",
        
        polylineOptions = FALSE,
        
        rectangleOptions = FALSE,
        
        circleOptions = FALSE,
        
        circleMarkerOptions = FALSE,
        
        markerOptions = FALSE,
        
        polygonOptions =
          leaflet.extras::drawPolygonOptions(
            showArea = TRUE
          ),
        
        editOptions =
          leaflet.extras::editToolbarOptions(
            
            selectedPathOptions =
              leaflet.extras::selectedPathOptions()
            
          ),
        
        singleFeature = TRUE
        
      )
    
  })
  
  # ----------------------------------------------------------
  # Receive drawn polygon
  # ----------------------------------------------------------
  
  observeEvent(
    input$drawMap_draw_new_feature,
    {
      
      feature <-
        input$drawMap_draw_new_feature
      
      
      if (
        is.null(feature)
      ) {
        
        return()
        
      }
      
      
      tryCatch(
        
        {
          
          aoi_sf <-
            geojsonsf::geojson_sf(
              
              jsonlite::toJSON(
                
                feature,
                
                auto_unbox = TRUE
                
              )
              
            )
          
          
          # Store drawn AOI
          aoi(
            aoi_sf
          )
          
          
          showNotification(
            
            "AOI drawn successfully.",
            
            type = "message"
            
          )
          
        },
        
        error = function(e) {
          
          showNotification(
            
            paste(
              "Could not read drawn polygon:",
              e$message
            ),
            
            type = "error"
            
          )
          
        }
        
      )
      
    }
    
  )
  
  
  # ----------------------------------------------------------
  # Uploaded AOI
  # ----------------------------------------------------------
  
  observeEvent(
    input$aoi_files,
    {
      
      req(
        input$aoi_files
      )
      
      
      tryCatch(
        
        {
          
          uploaded <-
            read_uploaded_aoi(
              input$aoi_files
            )
          
          
          # Store uploaded geometry
          aoi(
            uploaded
          )
          
          
          showNotification(
            
            "AOI uploaded successfully.",
            
            type = "message"
            
          )
          
        },
        
        error = function(e) {
          
          showNotification(
            
            paste(
              "Could not read AOI:",
              e$message
            ),
            
            type = "error",
            
            duration = 10
            
          )
          
        }
        
      )
      
    },
    
    ignoreNULL = TRUE
    
  )
  
  
  # ----------------------------------------------------------
  # Coordinate AOI
  # ----------------------------------------------------------
  
  observeEvent(
    input$coordinates,
    {
      
      # Only act when coordinates are being entered
      req(
        input$aoi_method == "coordinates"
      )
      
      
      # Do not repeatedly try to interpret an empty box
      if (
        is.null(input$coordinates) ||
        !nzchar(
          trimws(
            input$coordinates
          )
        )
      ) {
        
        return()
        
      }
      
      
      tryCatch(
        
        {
          
          coordinate_aoi <-
            coordinates_to_polygon(
              input$coordinates
            )
          
          
          aoi(
            coordinate_aoi
          )
          
        },
        
        error = function(e) {
          
          # Do not display an error for every keystroke.
          # The error will be shown when Calculate is pressed.
          
        }
        
      )
      
    }
    
  )
  
  
  # ----------------------------------------------------------
  # Calculate AOI estimate
  # ----------------------------------------------------------
  
  observeEvent(
    input$calculate,
    {
      
      # --------------------------------------------------------
      # Show calculation modal
      # --------------------------------------------------------
      
      showModal(
        modalDialog(
          
          title = "Calculating estimate",
          
          tags$p(
            "Calculating estimate, please wait..."
          ),
          
          footer = NULL,
          
          easyClose = FALSE
          
        )
      )
      
      
      
      tryCatch(
        
        {
          
          # --------------------------------------------------
          # Determine source AOI
          # --------------------------------------------------
          
          current_aoi <- aoi()
          
          
          if (
            is.null(current_aoi)
          ) {
            
            stop(
              "Please define an Area of Interest first."
            )
            
          }
          
          
          # --------------------------------------------------
          # Coordinates entered manually
          # --------------------------------------------------
          
          if (
            input$aoi_method == "coordinates"
          ) {
            
            current_aoi <-
              coordinates_to_polygon(
                input$coordinates
              )
            
            aoi(
              current_aoi
            )
            
          }
          
          
          # --------------------------------------------------
          # Prepare AOI
          # --------------------------------------------------
          
          # Original AOI for display
          original_aoi <- sf::st_as_sf(current_aoi)
          
          if (is.na(sf::st_crs(original_aoi))) {
            
            sf::st_crs(original_aoi) <- CRS_MAP
            
          }
          
          # Buffered AOI for analysis
          prepared <-
            prepare_aoi(
              
              current_aoi,
              
              buffer_choice =
                input$buffer_choice,
              
              buffer_dist =
                input$buffer_dist
              
            )
          
          prepared_aoi <-
            prepared$aoi
          
          original_aoi <-
            prepared$original_aoi
          
          # --------------------------------------------------
          # Selected map/species
          # --------------------------------------------------
          
          map_info <-
            get_selected_map(
              
              input$selectDataEstimate,
              
              map_data
              
            )
          
          
          # --------------------------------------------------
          # Calculate
          # --------------------------------------------------
          
          result <-
            calculate_aoi_estimate(
              
              aoi =
                prepared_aoi,
              
              map_info =
                map_info,
              
              unc_data =
                unc_data
              
            )
          
          
          result$aoi <-
            prepared_aoi
          
          result$original_aoi <-
            original_aoi
          
          result$map_info <- 
            map_info
          
          result$choice <-
            map_info$choice
          
          result$code <-
            map_info$code
          
          result$buffer_choice <-
            input$buffer_choice
          
          result$buffer_dist <-
            input$buffer_dist
          
          
          estimate(
            result
          )
          
          removeModal()
          
          showNotification(
            "Estimate calculated successfully",
            type = "message"
          )
          
          # --------------------------------------------------
          # Display result
          # --------------------------------------------------
          
          output$estimate <-
            renderUI({
              
              req(
                estimate()
              )
              
              
              res <-
                estimate()
              
              
              tagList(
                
                h4(
                  res$choice
                ),
                
                
                tags$p(
                  
                  strong(
                    "Total density within AOI: "
                  ),
                  
                  format(
                    res$sum,
                    digits = 5
                  )
                  
                ),
                
                
                if (
                  AOI$area_CI &&
                  is.finite(res$lower_CI) &&
                  is.finite(res$upper_CI)
                ) {
                  
                  tags$p(
                    
                    strong(
                      paste0(
                        AOI$CI_level * 100,
                        "% CI: "
                      )
                    ),
                    
                    paste0(
                      
                      format(
                        res$lower_CI,
                        digits = 5
                      ),
                      
                      " – ",
                      
                      format(
                        res$upper_CI,
                        digits = 5
                      )
                      
                    )
                    
                  )
                  
                }
                
              )
              
            })
          
          
        },
        
        # ----------------------------------------------------
        # Error handling
        # ----------------------------------------------------
        
        error = function(e) {
          
          showNotification(
            
            paste(
              "Could not calculate estimate:",
              e$message
            ),
            
            type = "error",
            
            duration = 10
            
          )
          
        }
        
      )
      
    }
    
  )
  
  # ----------------------------------------------------------
  # AOI result map
  # ----------------------------------------------------------
  
  output$map2 <- renderLeaflet({
    
    res <- estimate()
    
    req(res)
    
    
    # --------------------------------------------------------
    # Retrieve map information
    # --------------------------------------------------------
    
    map_info <-
      res$map_info
    
    
    # --------------------------------------------------------
    # Prepare raster
    # --------------------------------------------------------
    
    r <-
      prepare_map_raster(
        
        res$raster,
        
        rounding =
          map_info$rounding
        
      )
    
    
    # --------------------------------------------------------
    # Create palette
    # --------------------------------------------------------
    
    map_palette <-
      create_map_palette(
        
        values =
          as.numeric(
            terra::values(r)
          ),
        
        method =
          MAP_PALETTE$method,
        
        n =
          MAP_PALETTE$n,
        
        upper_quantile =
          MAP_PALETTE$upper_quantile,
        
        palette =
          MAP_PALETTE$palette
        
      )
    
    
    # --------------------------------------------------------
    # Create base map
    # --------------------------------------------------------
    
    map <-
      create_base_map(
        background = background
      )
    
    
    # --------------------------------------------------------
    # Add raster
    # --------------------------------------------------------
    
    map <-
      map %>%
      
      addRasterImage(
        
        r,
        
        colors =
          map_palette$palette,
        
        opacity = 1,
        
        group = "Density",
        
        maxBytes =
          200000000,
        
        project = FALSE,
        
        options =
          leafletOptions(
            pane = "density"
          )
        
      )
    
    
    # --------------------------------------------------------
    # Add land mask
    # --------------------------------------------------------
    
    if (
      MAP_ENV$land_layer == "mask"
    ) {
      
      map <-
        add_land_layer(
          
          map =
            map,
          
          background =
            background,
          
          pane =
            "land_mask"
          
        )
      
    }
    
    
    # --------------------------------------------------------
    # Add AOI
    # --------------------------------------------------------
    
    map <-
      map %>%
      
      addPolygons(
        
        data =
          sf::st_transform(
            res$original_aoi,
            CRS_MAP
          ),
        
        color = "red",
        
        fillColor = "red",
        
        fillOpacity = 0.15,
        
        weight = 3,
        
        options =
          pathOptions(
            pane = "overlay"
          )
        
      )
    
    # --------------------------------------------------------
    # Add buffer boundary
    # --------------------------------------------------------
    
    if (
      res$buffer_choice == "Yes"
    ) {
      
      map <-
        map %>%
        
        addPolygons(
          
          data =
            sf::st_transform(
              res$aoi,
              CRS_MAP
            ),
          
          color = "white",
          
          fill = "white",
          
          fillOpacity = 0.1,
          
          weight = 3,
          
          options =
            pathOptions(
              pane = "overlay"
            )
          
        )
      
    }
    
    
    # --------------------------------------------------------
    # Add image query
    # --------------------------------------------------------
    
    map <-
      map %>%
      
      leafem::addImageQuery(
        
        r,
        
        layerId = "Density",
        
        type = "mousemove",
        
        digits =
          map_info$rounding,
        
        position = "topright",
        
        prefix = "Mean"
        
      )
    
    
    # --------------------------------------------------------
    # Add legend
    # --------------------------------------------------------
    
    map <-
      map %>%
      
      addLegend(
        
        position = "bottomright",
        
        pal =
          map_palette$palette,
        
        values =
          map_palette$breaks,
        
        opacity = 1,
        
        title =
          "Mean density",
        
        labFormat =
          labelFormat(
            digits =
              map_info$rounding
          )
        
      )
    
    
    map
    
  })
  
  
  # ----------------------------------------------------------
  # Reset AOI and estimate
  # ----------------------------------------------------------
  
  observeEvent(
    input$reset,
    {
      
      aoi(
        NULL
      )
      
      estimate(
        NULL
      )
      
      
      output$estimate <-
        renderUI({
          NULL
        })
      
      
      output$map2 <-
        renderLeaflet({
          
          create_base_map(
            background = background
          )
          
        })
      
      
      # Clear coordinate input
      updateTextAreaInput(
        
        session,
        
        "coordinates",
        
        value = ""
        
      )
      
      
      # Clear uploaded file
      # The fileInput cannot be reset directly, so the user
      # can simply choose another file.
      
      
      showNotification(
        
        "AOI and estimate reset.",
        
        type = "message"
        
      )
      
    }
    
  )
  
  
  # ----------------------------------------------------------
  # Download guidance document - dashboard
  # ----------------------------------------------------------
  
  output$downloadDocsDashboard <- downloadHandler(
    
    filename = function() {
      
      DOWNLOAD_DOCS$filename
      
    },
    
    content = function(file) {
      
      showModal(
        modalDialog(
          
          title = "Preparing download",
          
          tags$p(
            "Your documents are being prepared. Please wait..."
          ),
          
          footer = NULL,
          
          easyClose = FALSE
          
        )
      )
      
      
      files_to_zip <-
        list.files(
          DOWNLOAD_DOCS$directory,
          recursive = TRUE,
          full.names = TRUE
        )
      
      
      if (
        length(files_to_zip) == 0
      ) {
        
        removeModal()
        
        stop(
          "No documents were found."
        )
        
      }
      
      
      oldwd <-
        getwd()
      
      
      on.exit(
        
        {
          
          setwd(oldwd)
          
          removeModal()
          
        },
        
        add = TRUE
        
      )
      
      
      setwd(
        DOWNLOAD_DOCS$directory
      )
      
      
      relative_files <-
        list.files(
          
          ".",
          
          recursive = TRUE,
          
          full.names = TRUE
          
        )
      
      
      utils::zip(
        
        zipfile = file,
        
        files = relative_files
        
      )
      
    }
  )
  
  # ----------------------------------------------------------
  # Download guidance document - AOI
  # ----------------------------------------------------------
  
  output$downloadDocsAOI <-
    downloadHandler(
      
      filename = function() {
        
        DOWNLOAD_DOCS$filename
        
      },
      
      content = function(file) {
        
        showModal(
          modalDialog(
            
            title = "Preparing download",
            
            tags$p(
              "Your documents are being prepared. Please wait..."
            ),
            
            footer = NULL,
            
            easyClose = FALSE
            
          )
        )
        
        
        files_to_zip <-
          list.files(
            DOWNLOAD_DOCS$directory,
            recursive = TRUE,
            full.names = TRUE
          )
        
        
        if (
          length(files_to_zip) == 0
        ) {
          
          removeModal()
          
          stop(
            "No documents were found."
          )
          
        }
        
        
        oldwd <-
          getwd()
        
        
        on.exit(
          
          {
            
            setwd(oldwd)
            
            removeModal()
            
          },
          
          add = TRUE
          
        )
        
        
        setwd(
          DOWNLOAD_DOCS$directory
        )
        
        
        relative_files <-
          list.files(
            
            ".",
            
            recursive = TRUE,
            
            full.names = TRUE
            
          )
        
        
        utils::zip(
          
          zipfile = file,
          
          files = relative_files
          
        )
        
      }
      
    )
  
  
  # ----------------------------------------------------------
  # Download source data
  # ----------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      DOWNLOAD_DATA$filename
      
    },
    
    content = function(file) {
      
      # --------------------------------------------------------
      # Show progress modal
      # --------------------------------------------------------
      
      showModal(
        modalDialog(
          
          title = "Preparing download",
          
          tags$p(
            "Your download is being prepared. Please wait..."
          ),
          
          footer = NULL,
          
          easyClose = FALSE
          
        )
      )
      
      
      # --------------------------------------------------------
      # Create download
      # --------------------------------------------------------
      
      files_to_zip <-
        list.files(
          DOWNLOAD_DATA$directory,
          recursive = TRUE,
          full.names = TRUE
        )
      
      
      if (
        length(files_to_zip) == 0
      ) {
        
        removeModal()
        
        stop(
          "No data files were found."
        )
        
      }
      
      
      oldwd <-
        getwd()
      
      
      on.exit(
        
        {
          
          setwd(oldwd)
          
          removeModal()
          
        },
        
        add = TRUE
        
      )
      
      
      setwd(
        DOWNLOAD_DATA$directory
      )
      
      
      relative_files <-
        list.files(
          
          ".",
          
          recursive = TRUE,
          
          full.names = TRUE
          
        )
      
      
      utils::zip(
        
        zipfile = file,
        
        files = relative_files
        
      )
      
    }
  )
  
}