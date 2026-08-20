# ============================================================
# APPLICATION CONFIGURATION
# ============================================================
#
# This file contains settings that should normally be changed
# when adapting the application to a new dataset.
#
# The main application framework is contained in the R/
# directory and should not normally require modification.
#
# ============================================================


# ------------------------------------------------------------
# Application information
# ------------------------------------------------------------

APP_NAME <- "SpeciesMAP"

APP_TITLE <- "Species Mapping & Analysis Platform"


# ------------------------------------------------------------
# Application colour scheme
# ------------------------------------------------------------
#
# Options: blue, black, purple, green, red, yellow
#
# ------------------------------------------------------------
APP_SCHEME <- "purple"


# ------------------------------------------------------------
# Coordinate reference systems
# ------------------------------------------------------------
#
# ANALYSIS CRS:
#   CRS used by the source rasters / spatial analysis.
#
# MAP CRS:
#   Geographic CRS used by Leaflet.
#
# WEB CRS:
#   Web Mercator CRS used internally by Leaflet.
#
# ------------------------------------------------------------

CRS_ANALYSIS <- "EPSG:3035"

CRS_MAP <- "EPSG:4326"

CRS_WEB <- "EPSG:3857"


# ------------------------------------------------------------
# Map background
# ------------------------------------------------------------
#
# Natural Earth data are used for the map background.
#
# Examples:
#
#   "Europe"
#   "North America"
#   "South America"
#   "Africa"
#   "Asia"
#
# Include multiple continents as follows: c("Europe", "North America", "Africa")
# See ?rnaturalearth::ne_countries for further options.
#
# MAP_SCALE options: one of '110', '50', '10' or 'small', 'medium', 'large'.
# '10' gives the finest resolution
#
# ------------------------------------------------------------

MAP_CONTINENT <- "Europe"

MAP_SCALE <- 10


# ----------------------------------------------------------
# Land layer plotting
# ----------------------------------------------------------
#
# Whether to display the land layer on top of the raster
#
# Options:
# "none"  = do not display land
# "under" = land is beneath the distribution raster (preferable for terrestrial datasets)
# "mask"  = land is above the distribution raster (preferable for marine datasets)

MAP_ENV <- list(
  land_layer = "mask"
)


# ------------------------------------------------------------
# Default map view
# ------------------------------------------------------------

MAP_VIEW <- list(
  
  # Default map centre location
  lat = 55, lng = 5,
  
  # Default zoom level
  zoom = 5,
  min_zoom = 5,
  max_zoom = 10,
  
  # Control maximum map panning limits
  bounds = list(
    xmin = -5,
    xmax = 10,
    ymin = 50,
    ymax = 62
  ),
  
  # Basemap
  tiles = "CartoDB.PositronNoLabels"
  
)


# ------------------------------------------------------------
# Area of interest settings
# ------------------------------------------------------------

AOI <- list(
  
  # Valid coordinate range for manually entered coordinates
  longitude = c(-15, 10),
  latitude = c(45, 62),
  
  # Buffer limits in kilometres
  buffer_min = 0.5,
  buffer_max = 100,
  
  # Default buffer distance (if selected)
  default_buffer = 1,
  
  # ----------------------------------------------------------
  # AOI raster extraction
  # ----------------------------------------------------------
  # cell covered by the AOI?
  # TRUE: Partial cells are weighted by their coverage fraction.
  # FALSE: Any cell intersecting the AOI is included in full.
  # Note that if density values are per unit area, then partial overlap may not be appropriate
  coverage_weighted = TRUE,
  
  # ----------------------------------------------------------
  # Area-based uncertainty
  # ----------------------------------------------------------
  # Should Confidence Intervals around the mean be calculated by aggregating
  # bootstrap predictions across the AOI?
  # this requires a matrix object with the same number of rows as cells in the mean raster to be supplied as a .RDS file
  # the row indexing should exactly follow the cell indexing in the raster
  # this should be saved in data/uncertainty/ and the file paths inputed into data/csv/uncertainty.csv
  # options: TRUE/FALSE
  area_CI = TRUE,
  
  # Confidence level for area-based CIs.
  #
  # Examples:
  # 0.95 = 95% CI
  # 0.99 = 99% CI
  # 0.90 = 90% CI
  CI_level = 0.95
  
)


# ------------------------------------------------------------
# Input data (mean rasters)
# ------------------------------------------------------------
#
# data/csv/maps.csv contains one row per raster
#
# Required columns:
#
#   code      unique identifier (e.g. Species1, Species2)
#   choice    label displayed to the user (e.g. Species 1, Species 2)
#   file      raster file path (e.g. data/Species1_mean.tiff, data/Species2_mean.tiff)
#   rounding  number of decimal places to round map density values
#   maxval    maximum value used for the legend (set to max density value per raster)
#
# ------------------------------------------------------------

MAP_DATA_FILE <- "data/csv/maps.csv"


# ------------------------------------------------------------
# Input data (uncertainty matrices)
# ------------------------------------------------------------
#
# data/csv/uncertainty.csv contains one row per map.
#
# Required columns:
#
#   code      unique identifier (e.g. Species1, Species2) should match maps.csv
#   file      raster file path (e.g. data/Species1_mean.tiff, data/Species2_mean.tiff)
#
# If uncertainty is not required, set to NULL
# ------------------------------------------------------------
UNC_DATA_FILE <- "data/csv/uncertainty.csv"


# ------------------------------------------------------------
# Map colour scale
# ------------------------------------------------------------

MAP_PALETTE <- list(
  
  # Options:
  # "continuous"
  # "regular"
  # "pretty"
  # "quantile"
  # "log"
  # "adaptive"
  
  method = "adaptive",
  
  # Number of colour classes for binned scales
  n = 8,
  
  # Upper quantile used by the adaptive scale
  #
  # For example:
  # 0.95 = ignore upper 5% when defining breaks
  # 0.98 = ignore upper 2%
  # 0.99 = ignore upper 1%
  
  upper_quantile = 0.98,
  
  # Colour palette
  palette = "viridis"
  
)


# ------------------------------------------------------------
# Logos
# ------------------------------------------------------------
#
# Put logo files in:
#
#   www/images/
#
# The application logo is displayed at the top of the
# sidebar. Funder/institution logos are displayed underneath.
#
# Set url = NULL if a logo should not be clickable.
#
# ------------------------------------------------------------

LOGOS <- list(
  
  app = list(
    file = "logo.png",
    alt = APP_NAME,
    width = "180px",
    url = NULL
  ),
  
  funders = list(
    
    list(
      file = "institution.png",
      alt = "Institution Logo",
      width = "180px",
      url = NULL
    ),
    
    list(
      file = "funder.png",
      alt = "Funder Logo",
      width = "180px",
      url = NULL
    )
    
  )
  
)


# ------------------------------------------------------------
# Text shown on the Interactive Maps page
# ------------------------------------------------------------
#
# This is deliberately kept here rather than generating
# dataset-specific explanatory text in R.
#
# More substantial text should be placed in the HTML files
# in www/static/html/.
#
# ------------------------------------------------------------

MAP_DESCRIPTION <- paste0(
  "Maps show the estimated mean density represented by each raster cell for the selected category."
)


# ------------------------------------------------------------
# Download settings
# ------------------------------------------------------------

DOWNLOAD_DATA <- list(
  
  # Whether to show the download tab
  enabled = TRUE,
  
  directory = "data/rasters",
  
  filename = "distribution_data.zip"
  
)


DOWNLOAD_DOCS <- list(
  
  # Whether to show the download tab
  enabled = TRUE,
  
  directory = "data/docs",
  
  filename = "distribution_data_guidelines.zip"
  
)