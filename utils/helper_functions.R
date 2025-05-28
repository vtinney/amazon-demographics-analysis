# Helper Functions for Amazon Demographics Analysis
# ================================================

# Standardize municipal codes to 7 digits
standardize_muni_code <- function(code) {
  sprintf("%07d", as.numeric(code))
}

# Save data with automatic metadata
save_data_with_metadata <- function(data, filename, description = "") {
  # Ensure directory exists
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  
  # Save the data
  write_csv(data, filename)
  
  # Create metadata
  metadata <- list(
    filename = basename(filename),
    created = Sys.time(),
    rows = nrow(data),
    columns = ncol(data),
    description = description,
    column_names = names(data)
  )
  
  # Save metadata as JSON
  metadata_file <- paste0(tools::file_path_sans_ext(filename), "_metadata.json")
  write(jsonlite::toJSON(metadata, pretty = TRUE), metadata_file)
  
  cat("💾 Saved:", filename, "(", nrow(data), "rows x", ncol(data), "columns)\n")
}

# Check if required files exist
check_required_files <- function(file_list) {
  missing_files <- file_list[!file.exists(file_list)]
  
  if (length(missing_files) > 0) {
    cat("⚠️ Missing files:\n")
    cat(paste("  -", missing_files, collapse = "\n"), "\n")
    return(FALSE)
  } else {
    cat("✅ All required files present\n")
    return(TRUE)
  }
}

# Create interactive leaflet map
create_choropleth_map <- function(data, column, title, color_palette = "YlOrRd") {
  if (!column %in% names(data)) {
    stop("Column '", column, "' not found in data")
  }
  
  # Remove missing values
  data_clean <- data[!is.na(data[[column]]), ]
  
  if (nrow(data_clean) == 0) {
    warning("No valid data for column: ", column)
    return(NULL)
  }
  
  # Create color palette
  pal <- colorNumeric(palette = color_palette, domain = data_clean[[column]])
  
  # Create map
  leaflet(data_clean) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(data_clean[[column]]),
      weight = 1,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      popup = ~paste(
        name_muni, "<br>", 
        title, ":", round(data_clean[[column]], 2)
      )
    ) %>%
    addLegend(
      pal = pal, 
      values = data_clean[[column]],
      title = title,
      position = "bottomright"
    ) %>%
    setView(-60, -5, zoom = 5)
}
