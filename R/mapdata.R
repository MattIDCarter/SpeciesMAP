# ============================================================
# MAP DATA FUNCTIONS
# ============================================================


# Required packages
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthhires)


# ------------------------------------------------------------
# Load map metadata
# ------------------------------------------------------------

load_map_data <- function(
    file = MAP_DATA_FILE
) {
  
  if (!file.exists(file)) {
    
    stop(
      "Map data file not found: ",
      file
    )
    
  }
  
  dat <- read.csv(
    file,
    stringsAsFactors = FALSE
  )
  
  
  # Required columns
  required_columns <- c(
    "code",
    "choice",
    "file",
    "rounding",
    "maxval"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(dat)
  )
  
  if (length(missing_columns) > 0) {
    
    stop(
      "The following required columns are missing from ",
      file,
      ": ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
    
  }
  
  
  # Check that map codes are unique
  if (anyDuplicated(dat$code)) {
    
    stop(
      "Values in the 'code' column of maps.csv ",
      "must be unique."
    )
    
  }
  
  
  # Check that raster files exist
  missing_files <- dat$file[
    !file.exists(dat$file)
  ]
  
  if (length(missing_files) > 0) {
    
    stop(
      "The following raster files listed in maps.csv ",
      "could not be found:\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
    
  }
  
  
  dat
  
}


# ------------------------------------------------------------
# Load uncertainty metadata
# ------------------------------------------------------------

load_UNC_data <- function(
    file = UNC_DATA_FILE
) {
  
  # ----------------------------------------------------------
  # No uncertainty data supplied
  # ----------------------------------------------------------
  
  if (is.null(file)) {
    
    return(NULL)
    
  }
  
  
  # ----------------------------------------------------------
  # Check file exists
  # ----------------------------------------------------------
  
  if (!file.exists(file)) {
    
    stop(
      "Uncertainty data file not found: ",
      file
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Read uncertainty data
  # ----------------------------------------------------------
  
  dat <- read.csv(
    file,
    stringsAsFactors = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Required columns
  # ----------------------------------------------------------
  
  required_columns <- c(
    "code",
    "file"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(dat)
  )
  
  if (length(missing_columns) > 0) {
    
    stop(
      "The following required columns are missing from ",
      file,
      ": ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Check that codes are unique
  # ----------------------------------------------------------
  
  if (anyDuplicated(dat$code)) {
    
    stop(
      "Values in the 'code' column of uncertainty.csv ",
      "must be unique."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Check that .RDS files exist
  # ----------------------------------------------------------
  
  missing_files <- dat$file[
    !file.exists(dat$file)
  ]
  
  if (length(missing_files) > 0) {
    
    stop(
      "The following .RDS files listed in uncertainty.csv ",
      "could not be found:\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
    
  }
  
  
  dat
  
}



# ------------------------------------------------------------
# Load uncertainty matrix for selected map
# ------------------------------------------------------------

load_uncertainty_matrix <- function(
    map_info,
    unc_data
) {
  
  
  # ----------------------------------------------------------
  # Check uncertainty data available
  # ----------------------------------------------------------
  
  if (is.null(unc_data)) {
    
    stop(
      "No uncertainty data are available."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Find matching map code
  # ----------------------------------------------------------
  
  match <- unc_data[
    unc_data$code == map_info$code,
    ,
    drop = FALSE
  ]
  
  
  if (nrow(match) == 0) {
    
    stop(
      "No uncertainty data were found for map code: ",
      map_info$code
    )
    
  }
  
  
  if (nrow(match) > 1) {
    
    stop(
      "Multiple uncertainty files were found for map code: ",
      map_info$code
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Load RDS object
  # ----------------------------------------------------------
  
  bootstrap <- readRDS(
    match$file
  )
  
  
  # ----------------------------------------------------------
  # Check that object is a matrix
  # ----------------------------------------------------------
  
  if (!is.matrix(bootstrap)) {
    
    stop(
      "The uncertainty file for ",
      map_info$code,
      " does not contain a matrix."
    )
    
  }
  
  
  bootstrap
  
}


# ------------------------------------------------------------
# Load Natural Earth background
# ------------------------------------------------------------

load_map_background <- function(
    continent = MAP_CONTINENT,
    scale = MAP_SCALE
) {
  
  rnaturalearth::ne_countries(
    continent = continent,
    scale = scale,
    returnclass = "sf"
  )
  
}


# ------------------------------------------------------------
# Load a raster associated with a map choice
# ------------------------------------------------------------

load_map_raster <- function(
    map_info
) {
  
  r <- terra::rast(
    map_info$file
  )
  
  # If the raster contains multiple layers, flag an error
  if (terra::nlyr(r) != 1) {
    
    stop(
      "The supplied raster must contain exactly one layer ",
      "(the mean estimate). ",
      "The selected raster contains ",
      terra::nlyr(r),
      " layers."
    )
  }
  
  r
  
}


# ------------------------------------------------------------
# Prepare raster for display
# ------------------------------------------------------------

prepare_map_raster <- function(
    r,
    rounding = 4
) {
  
  # Reproject to Web Mercator for Leaflet
  r <- terra::project(
    r,
    CRS_WEB,
    method = "bilinear"
  )
  
  
  # Round values for display
  r <- round(
    r,
    rounding
  )
  
  
  # Replace NaN with proper NA
  r <- terra::app(
    r,
    fun = function(x) {
      x[is.nan(x)] <- NA
      x
    }
  )
  
  return(r)
  
}


# ------------------------------------------------------------
# Load all data required by the application
# ------------------------------------------------------------

load_application_data <- function() {
  
  list(
    
    map_data = load_map_data(),
    
    unc_data = load_UNC_data(),
    
    background = load_map_background()
    
  )
  
}