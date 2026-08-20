# ============================================================
# MAP FUNCTIONS
# ============================================================


library(leaflet)
library(leafem)
library(terra)


# ------------------------------------------------------------
# Create a base Leaflet map
# ------------------------------------------------------------

# ------------------------------------------------------------
# Create base map
# ------------------------------------------------------------

# ------------------------------------------------------------
# Create base map
# ------------------------------------------------------------

create_base_map <- function(
    background = NULL,
    view = MAP_VIEW,
    land_layer = MAP_ENV$land_layer
) {
  
  
  # ----------------------------------------------------------
  # Check land-layer option
  # ----------------------------------------------------------
  
  if (
    !land_layer %in% c(
      "none",
      "under",
      "mask"
    )
  ) {
    
    stop(
      "MAP_ENV$land_layer must be one of: ",
      "'none', 'under', or 'mask'."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Create map
  # ----------------------------------------------------------
  
  map <- leaflet(
    
    options = leafletOptions(
      
      minZoom = view$min_zoom,
      
      maxZoom = view$max_zoom,
      
      maxBounds = list(
        
        c(
          view$bounds$ymin,
          view$bounds$xmin
        ),
        
        c(
          view$bounds$ymax,
          view$bounds$xmax
        )
        
      ),
      
      maxBoundsViscosity = 1
      
    )
    
  ) %>%
    
    addProviderTiles(
      view$tiles
    ) %>%
    
    setView(
      lng = view$lng,
      lat = view$lat,
      zoom = view$zoom
    ) %>%
    
    # --------------------------------------------------------
  # Explicit layer panes
  # --------------------------------------------------------
  
  addMapPane(
    "land_under",
    zIndex = 300
  ) %>%
    
    addMapPane(
      "density",
      zIndex = 400
    ) %>%
    
    addMapPane(
      "land_mask",
      zIndex = 500
    ) %>%
    
    addMapPane(
      "overlay",
      zIndex = 600
    )
  
  
  # ----------------------------------------------------------
  # Add land underneath raster
  # ----------------------------------------------------------
  
  if (
    land_layer == "under" &&
    !is.null(background)
  ) {
    
    map <- add_land_layer(
      
      map = map,
      
      background = background,
      
      pane = "land_under"
      
    )
    
  }
  
  
  map
  
}


# ------------------------------------------------------------
# Add land layer
# ------------------------------------------------------------

add_land_layer <- function(
    map,
    background,
    pane = "overlay"
) {
  
  map %>%
    
    addPolygons(
      
      data = background,
      
      color = "white",
      
      fillColor = "grey",
      
      fill = TRUE,
      
      fillOpacity = 0.8,
      
      weight = 1,
      
      smoothFactor = 1,
      
      options = pathOptions(
        pane = pane
      )
      
    )
  
}

# ------------------------------------------------------------
# Add mouse coordinate display
# ------------------------------------------------------------

add_mouse_coordinates <- function(
    map
) {
  
  map %>%
    
    addControl(
      
      html = paste0(
        "<div id='mousecoords'>",
        "Lat: --<br>Lon: --",
        "</div>"
      ),
      
      position = "topright"
      
    ) %>%
    
    htmlwidgets::onRender(
      
      "
      function(el, x) {

        var map = this;

        map.on('mousemove', function(e) {

          var box = document.getElementById('mousecoords');

          if (box) {

            box.innerHTML =
              'Lat: ' + e.latlng.lat.toFixed(5) +
              '<br>Lon: ' + e.latlng.lng.toFixed(5);

          }

        });

        map.on('mouseout', function(e) {

          var box = document.getElementById('mousecoords');

          if (box) {

            box.innerHTML =
              'Lat: --<br>Lon: --';

          }

        });

      }
      "
      
    )
  
}


# ------------------------------------------------------------
# Create colour palette
# ------------------------------------------------------------

create_map_palette <- function(
    values,
    method = "adaptive",
    n = 8,
    upper_quantile = 0.98,
    palette = "viridis"
) {
  
  
  # ----------------------------------------------------------
  # Check valid method supplied
  # ----------------------------------------------------------
  
  if (!method %in% c(
    "continuous",
    "regular",
    "pretty",
    "quantile",
    "log",
    "adaptive"
  )) {
    
    stop(
      "Unknown map palette method: ",
      method
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Remove missing and infinite values
  # ----------------------------------------------------------
  
  values <- values[
    is.finite(values)
  ]
  
  
  if (length(values) == 0) {
    
    stop(
      "No valid values available for colour palette."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Maximum value
  # ----------------------------------------------------------
  
  max_value <- max(
    values,
    na.rm = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Continuous scale
  # ----------------------------------------------------------
  
  if (method == "continuous") {
    
    pal <- leaflet::colorNumeric(
      
      palette = palette,
      
      domain = c(
        0,
        max_value
      ),
      
      na.color = "transparent"
      
    )
    
    return(
      list(
        palette = pal,
        breaks = c(
          0,
          max_value
        )
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Regular breaks
  # ----------------------------------------------------------
  
  if (method == "regular") {
    
    bins <- seq(
      0,
      max_value,
      length.out = n + 1
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Pretty breaks
  # ----------------------------------------------------------
  
  if (method == "pretty") {
    
    bins <- pretty(
      c(
        0,
        max_value
      ),
      n = n
    )
    
    bins <- bins[
      bins >= 0
    ]
    
  }
  
  
  # ----------------------------------------------------------
  # Quantile breaks
  # ----------------------------------------------------------
  
  if (method == "quantile") {
    
    bins <- quantile(
      values,
      probs = seq(
        0,
        1,
        length.out = n + 1
      ),
      na.rm = TRUE
    )
    
    bins <- unique(
      bins
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Logarithmic breaks
  # ----------------------------------------------------------
  
  if (method == "log") {
    
    bins <- expm1(
      seq(
        0,
        log1p(max_value),
        length.out = n + 1
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Adaptive breaks
  # ----------------------------------------------------------
  
  if (method == "adaptive") {
    
    # Define the upper end of the core distribution
    upper <- quantile(
      values,
      upper_quantile,
      na.rm = TRUE
    )
    
    
    # Generate pretty breaks across the core distribution
    bins <- pretty(
      c(
        0,
        upper
      ),
      n = n
    )
    
    
    bins <- bins[
      bins >= 0
    ]
    
    
    # Ensure the maximum value is still represented
    bins <- unique(
      c(
        bins,
        max_value
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Clean breaks
  # ----------------------------------------------------------
  
  bins <- unique(
    bins[
      is.finite(bins)
    ]
  )
  
  bins <- sort(
    bins
  )
  
  
  if (length(bins) < 2) {
    
    stop(
      "Unable to create suitable colour breaks."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Create binned palette and breaks for legend
  # ----------------------------------------------------------
  
  pal <- leaflet::colorBin(
    
    palette = palette,
    
    domain = c(
      0,
      max_value
    ),
    
    bins = bins,
    
    na.color = "transparent"
    
  )
  
  
  list(
    
    palette = pal,
    
    breaks = bins
    
  )
  
}
  

# ------------------------------------------------------------
# Add a raster to a map
# ------------------------------------------------------------

add_density_layer <- function(
    map,
    raster,
    max_value,
    rounding = 4
) {
  
  map %>%
    
    clearImages() %>%
    
    clearControls() %>%
    
    addRasterImage(
      
      raster,
      
      colors = pal,
      
      opacity = 1,
      
      group = "Density",
      
      maxBytes = 200000000,
      
      project = FALSE
      
    ) %>%
    
    leafem::addImageQuery(
      
      raster,
      
      layerId = "Density",
      
      type = "mousemove",
      
      digits = rounding,
      
      position = "topright",
      
      prefix = "Mean"
      
    ) %>%
    
    addLegend(
      
      position = "bottomright",
      
      pal = pal,
      
      values = c(
        0,
        max_value
      ),
      
      opacity = 1,
      
      title = "Mean density",
      
      labFormat = labelFormat(
        digits = rounding
      )
      
    )
  
}


# ------------------------------------------------------------
# Add an AOI to a map
# ------------------------------------------------------------

add_aoi <- function(
    map,
    aoi
) {
  
  req <- requireNamespace(
    "sf",
    quietly = TRUE
  )
  
  if (!req) {
    stop("Package 'sf' is required.")
  }
  
  
  map %>%
    
    addPolygons(
      
      data = aoi,
      
      color = "red",
      
      fillColor = "red",
      
      fillOpacity = 0.15,
      
      weight = 3,
      
      group = "AOI"
      
    )
  
}