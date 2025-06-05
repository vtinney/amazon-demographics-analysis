# Trajetorias Dataset Collection - R Version
# 
# Purpose: Download and process the Trajetorias dataset - environmental, 
#          epidemiological, and economic indicators for Brazilian Legal Amazon municipalities
# Time Required: 15-20 minutes
# Data Source: Zenodo repository (DOI: 10.5281/zenodo.7098053)
# Deliverables: 
#   - Multidimensional Poverty Index (MPI) data
#   - Vector-borne disease incidence data
#   - Environmental indicators (deforestation, land use)
#   - Population and socioeconomic indicators

# ====================================================================
# SECTION 1: Package Installation and Setup
# ====================================================================

# Install required packages
required_packages <- c("httr", "jsonlite", "dplyr", "tidyr", "readr", 
                      "ggplot2", "viridis", "patchwork", "lubridate",
                      "stringr", "purrr")

# Install packages if not already installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(viridis)
library(patchwork)
library(lubridate)
library(stringr)
library(purrr)

cat("📚 Libraries imported successfully!\n")
cat("🔥 R version:", R.version.string, "\n")

# ====================================================================
# SECTION 2: Configuration and Helper Functions
# ====================================================================

# Configuration list
config <- list(
  amazon_states = c('AC', 'AP', 'AM', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO'),
  zenodo_base_url = "https://zenodo.org/records/7098053/files/",
  
  # Dataset components
  components = list(
    'population' = 'TRAJETORIAS_DATASET_Population_indicators.csv',
    'socioeconomic' = 'TRAJETORIAS_DATASET_Socio-Economic_dimension-indicators.csv',
    'epidemiological' = 'TRAJETORIAS_DATASET_Epidemiological_dimension_indicators.csv',
    'environmental' = 'TRAJETORIAS_DATASET_Environmental_dimension_indicators.csv'
  ),
  
  # Key variables for PM2.5 analysis
  key_variables = list(
    'socioeconomic' = c(
      'mpi_rural', 'mpi_urban', 'mpi_general',
      'poverty_incidence', 'poverty_intensity',
      'income', 'education', 'employment'
    ),
    'epidemiological' = c(
      'malaria', 'dengue', 'leishmaniasis',
      'respiratory', 'diarrhea',
      'health_access', 'hospital_density'
    ),
    'environmental' = c(
      'deforestation', 'forest_cover',
      'agriculture', 'pasture', 'urban_area',
      'precipitation', 'temperature',
      'road_density', 'mining'
    ),
    'population' = c(
      'population_density', 'urban_population',
      'migration', 'age_structure'
    )
  )
)

# Helper functions
create_directories <- function() {
  directories <- c(
    'data/raw/trajetorias',
    'data/processed',
    'quality_checks',
    'outputs/tables'
  )
  
  for (directory in directories) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }
  
  cat("📁 Directory structure created\n")
}

standardize_muni_code <- function(code) {
  if (any(is.na(code))) {
    code[is.na(code)] <- NA
  }
  sprintf("%07d", as.numeric(code))
}

save_data_with_metadata <- function(df, filename, description = "") {
  # Create directory if needed
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  
  # Save the data
  write_csv(df, filename)
  
  # Create metadata
  metadata <- list(
    filename = basename(filename),
    created = as.character(now()),
    rows = nrow(df),
    columns = ncol(df),
    description = description,
    column_names = names(df)
  )
  
  # Save metadata
  metadata_file <- gsub("\\.csv$", "_metadata.json", filename)
  write_json(metadata, metadata_file, pretty = TRUE)
  
  cat("💾 Saved:", filename, "(", nrow(df), "rows)\n")
}

# Setup directories
create_directories()

cat("✅ Configuration ready\n")
cat("🌿 Amazon states:", paste(config$amazon_states, collapse = ", "), "\n")
cat("📊 Dataset components:", length(config$components), "\n")

# ====================================================================
# SECTION 3: Download Trajetorias Dataset Files
# ====================================================================

download_file <- function(url, filename) {
  tryCatch({
    cat("⬇️ Downloading:", basename(filename), "... ")
    
    # Create directory if needed
    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    
    # Download file
    response <- GET(url, 
                   add_headers(`User-Agent` = "Mozilla/5.0 (compatible; TrajetoriasAnalysis/1.0)"),
                   timeout(300))
    
    if (status_code(response) != 200) {
      stop("HTTP error: ", status_code(response))
    }
    
    # Save file
    writeBin(content(response, "raw"), filename)
    
    # Check file size
    file_size <- file.info(filename)$size
    cat("✅ Success (", round(file_size / 1024 / 1024, 1), "MB)\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Failed:", e$message, "\n")
    return(FALSE)
  })
}

download_all_components <- function() {
  cat("📥 Downloading Trajetorias dataset components...\n\n")
  
  download_results <- list()
  
  # Download main data files
  for (component in names(config$components)) {
    filename <- config$components[[component]]
    url <- paste0(config$zenodo_base_url, filename)
    local_path <- file.path("data/raw/trajetorias", paste0(component, "_indicators.csv"))
    
    success <- download_file(url, local_path)
    download_results[[component]] <- success
  }
  
  return(download_results)
}

# Download files
download_results <- download_all_components()

# Summary
successful_downloads <- sum(unlist(download_results))
total_downloads <- length(download_results)

cat("\n📊 Download Summary:\n")
cat("   Successful:", successful_downloads, "/", total_downloads, "main files\n")

if (successful_downloads == total_downloads) {
  cat("✅ All downloads completed successfully!\n")
} else {
  cat("⚠️ Some downloads failed. Proceeding with available data...\n")
  for (component in names(download_results)) {
    status <- if(download_results[[component]]) "✅" else "❌"
    cat("  ", status, component, "\n")
  }
}

# ====================================================================
# SECTION 4: Load and Explore Dataset Structure
# ====================================================================

load_component <- function(component_name) {
  filename <- file.path("data/raw/trajetorias", paste0(component_name, "_indicators.csv"))
  
  if (!file.exists(filename)) {
    cat("❌", component_name, "file not found\n")
    return(NULL)
  }
  
  cat("\n===", toupper(component_name), "COMPONENT ===\n")
  
  tryCatch({
    # Load data
    data <- read_csv(filename, show_col_types = FALSE)
    
    # Basic information
    cat("📊 Dimensions:", nrow(data), "rows ×", ncol(data), "columns\n")
    
    # Check for key columns
    if ("code_muni" %in% names(data)) {
      unique_munis <- length(unique(data$code_muni))
      cat("🏘️ Municipalities:", unique_munis, "\n")
    }
    
    if ("year" %in% names(data)) {
      years <- sort(unique(data$year))
      if (length(years) <= 10) {
        cat("📅 Years available:", paste(years, collapse = ", "), "\n")
      } else {
        cat("📅 Years available:", min(years), "-", max(years), "(", length(years), "years)\n")
      }
    }
    
    # Show column preview
    cat("📋 Columns (first 10):\n")
    for (i in 1:min(10, ncol(data))) {
      cat("   -", names(data)[i], "\n")
    }
    
    if (ncol(data) > 10) {
      cat("   ... and", ncol(data) - 10, "more columns\n")
    }
    
    # Standardize municipal codes if present
    if ("code_muni" %in% names(data)) {
      data$code_muni <- standardize_muni_code(data$code_muni)
      cat("🔍 Municipal codes standardized\n")
    }
    
    return(data)
    
  }, error = function(e) {
    cat("❌ Error loading", component_name, ":", e$message, "\n")
    return(NULL)
  })
}

load_all_components <- function() {
  cat("🔍 Exploring Trajetorias dataset structure...\n")
  
  trajetorias_data <- list()
  
  for (component in names(config$components)) {
    data <- load_component(component)
    if (!is.null(data)) {
      trajetorias_data[[component]] <- data
    }
  }
  
  return(trajetorias_data)
}

# Load all components
trajetorias_data <- load_all_components()

cat("\n📊 Loaded", length(trajetorias_data), "components successfully\n")
cat("\n📋 Available components:\n")
for (component in names(trajetorias_data)) {
  data <- trajetorias_data[[component]]
  if (!is.null(data)) {
    cat("   ✅", component, ":", nrow(data), "records,", ncol(data), "variables\n")
  } else {
    cat("   ❌", component, ": Failed to load\n")
  }
}

# ====================================================================
# SECTION 5: Filter for Amazon Region and Process Data
# ====================================================================

cat("🗺️ Loading municipality-state mappings...\n")

# Load state mapping from census data
state_mapping <- NULL
census_file <- "data/raw/census/amazon_population_2022.csv"

if (file.exists(census_file)) {
  tryCatch({
    census_data <- read_csv(census_file, show_col_types = FALSE)
    state_mapping <- census_data %>%
      select(code_muni, abbrev_state, name_muni) %>%
      mutate(code_muni = standardize_muni_code(code_muni))
    
    cat("✅ Loaded state mapping for", nrow(state_mapping), "Amazon municipalities\n")
  }, error = function(e) {
    cat("⚠️ Could not load census data:", e$message, "\n")
    cat("Will process all municipalities.\n")
  })
} else {
  cat("⚠️ Census data not available. Will process all municipalities.\n")
}

process_component <- function(data, component_name, state_mapping = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    cat("⚠️ No data available for", component_name, "\n")
    return(NULL)
  }
  
  cat("\n🔄 Processing", component_name, "component...\n")
  
  processed_data <- data
  
  # Filter for Amazon municipalities if we have state mapping
  if (!is.null(state_mapping) && "code_muni" %in% names(processed_data)) {
    amazon_munis <- unique(state_mapping$code_muni)
    
    # Filter data
    before_count <- nrow(processed_data)
    processed_data <- processed_data %>%
      filter(code_muni %in% amazon_munis)
    after_count <- nrow(processed_data)
    
    cat("🌿 Filtered from", before_count, "to", after_count, "Amazon municipality records\n")
  }
  
  # Get most recent year if multiple years available
  if ("year" %in% names(processed_data) && length(unique(processed_data$year)) > 1) {
    latest_year <- max(processed_data$year, na.rm = TRUE)
    before_year_filter <- nrow(processed_data)
    processed_data <- processed_data %>%
      filter(year == latest_year)
    after_year_filter <- nrow(processed_data)
    
    cat("📅 Using most recent year:", latest_year, "(", after_year_filter, "records)\n")
  }
  
  # Add state information if available
  if (!is.null(state_mapping) && "code_muni" %in% names(processed_data)) {
    processed_data <- processed_data %>%
      left_join(
        state_mapping %>% select(code_muni, abbrev_state),
        by = "code_muni"
      )
    cat("🏛️ Added state information\n")
  }
  
  return(processed_data)
}

# Process all components
processed_trajetorias <- list()

for (component_name in names(trajetorias_data)) {
  data <- trajetorias_data[[component_name]]
  processed_data <- process_component(data, component_name, state_mapping)
  
  if (!is.null(processed_data) && nrow(processed_data) > 0) {
    processed_trajetorias[[component_name]] <- processed_data
    
    # Save processed data
    filename <- file.path("data/processed", paste0("trajetorias_", component_name, "_amazon.csv"))
    save_data_with_metadata(
      processed_data,
      filename,
      paste("Processed", component_name, "indicators from Trajetorias dataset for Amazon region")
    )
  }
}

cat("\n✅ Processed", length(processed_trajetorias), "components for Amazon region\n")

# Summary by component
for (component in names(processed_trajetorias)) {
  data <- processed_trajetorias[[component]]
  cat("   📊", component, ":", nrow(data), "records,", ncol(data), "variables\n")
  if ("abbrev_state" %in% names(data)) {
    states <- length(unique(data$abbrev_state))
    cat("      🏛️ Covers", states, "states\n")
  }
  if ("code_muni" %in% names(data)) {
    munis <- length(unique(data$code_muni))
    cat("      🏘️ Covers", munis, "municipalities\n")
  }
}

# ====================================================================
# SECTION 6: Extract Key Variables and Create Final Summary
# ====================================================================

extract_key_variables <- function(data, component_name, key_vars) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  cat("\n🔍 Extracting key variables from", component_name, "...\n")
  
  # Get available columns
  available_cols <- names(data)
  
  # Find matching variables (case-insensitive partial matching)
  found_vars <- c()
  for (key_var in key_vars) {
    # Look for columns containing the key variable name
    matches <- available_cols[grepl(key_var, available_cols, ignore.case = TRUE)]
    
    if (length(matches) > 0) {
      found_var <- matches[1]  # Take first match
      found_vars <- c(found_vars, found_var)
      cat("   ✅ Found:", key_var, "→", found_var, "\n")
    }
  }
  
  # Essential columns to always include
  essential_cols <- c('code_muni', 'year', 'abbrev_state', 'name_muni')
  essential_available <- intersect(essential_cols, available_cols)
  
  # Combine essential and found variables
  selected_cols <- unique(c(essential_available, found_vars))
  
  if (length(found_vars) > 0) {
    extracted_data <- data %>% select(all_of(selected_cols))
    cat("📋 Extracted", length(found_vars), "key variables +", length(essential_available), "essential columns\n")
    return(extracted_data)
  } else {
    cat("⚠️ No matching key variables found for", component_name, "\n")
    return(NULL)
  }
}

# Extract key variables from each component
cat("🎯 Extracting key variables for PM2.5 analysis...\n")

key_indicators <- list()

for (component_name in names(processed_trajetorias)) {
  if (component_name %in% names(config$key_variables)) {
    data <- processed_trajetorias[[component_name]]
    key_vars <- config$key_variables[[component_name]]
    
    extracted <- extract_key_variables(data, component_name, key_vars)
    
    if (!is.null(extracted)) {
      key_indicators[[component_name]] <- extracted
      
      # Save key indicators
      filename <- file.path("data/processed", paste0("key_indicators_", component_name, ".csv"))
      save_data_with_metadata(
        extracted,
        filename,
        paste("Key", component_name, "indicators for PM2.5 analysis from Trajetorias dataset")
      )
    }
  }
}

cat("\n✅ Extracted key indicators from", length(key_indicators), "components\n")

# Final summary
cat("\n", rep("=", 50), "\n", sep = "")
cat("🎯 TRAJETORIAS DATA COLLECTION COMPLETE!\n")
cat(rep("=", 50), "\n", sep = "")

# Summary of key indicators
for (component in names(key_indicators)) {
  data <- key_indicators[[component]]
  # Count non-essential columns
  essential_cols <- c('code_muni', 'year', 'abbrev_state', 'name_muni')
  indicator_cols <- setdiff(names(data), essential_cols)
  
  cat("   📊", component, ":", length(indicator_cols), "indicators,", nrow(data), "records\n")
  if ("abbrev_state" %in% names(data)) {
    states <- length(unique(data$abbrev_state))
    cat("      🏛️", states, "states covered\n")
  }
}

# Integration readiness
cat("\n🔗 Integration Readiness:\n")
census_ready <- file.exists("data/raw/census/amazon_population_2022.csv")
trajetorias_ready <- length(processed_trajetorias) > 0
fire_ready <- file.exists("data/processed/amazon_fire_by_municipality.csv")

cat("   📊 Census data:", if(census_ready) "✅ Ready" else "❌ Missing", "\n")
cat("   🌿 Trajetorias data:", if(trajetorias_ready) "✅ Ready" else "❌ Missing", "\n")
cat("   🔥 Fire data:", if(fire_ready) "✅ Ready" else "⏳ Next step", "\n")

cat("\n🎉 Trajetorias data collection pipeline complete!\n")
cat("Ready for data integration and analysis!\n")
