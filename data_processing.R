library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(tibble)

# Function to load and process UN population data (1950-2100)
process_un_population_data <- function(un_csv_path) {
    # Define all variants we want to include
    all_variants <- c(
        # Standard variants
        "IHME reference pace", "IHME sustainable development goals (SDG) pace", "Low", "Medium", "High", 
        # Special variants
        "Constant fertility", "Instant replacement", "Zero migration", 
        "Constant mortality", "Instant replacement zero migration"
    )
    
    un_data <- read_csv(un_csv_path, show_col_types = FALSE) %>%
        as_tibble() %>%
        # Filter out rows without ISO codes and regional aggregates
        dplyr::filter(!is.na(ISO3_code)) %>%
        dplyr::filter(LocTypeID == 4) %>%
        dplyr::filter(Variant %in% all_variants) %>%
        dplyr::filter(Time %in% c(-1000, 0, 500, 1000, 1500, 1900, 1950, 2025, 2050, 2100)) %>%
        dplyr::select(ISO3_code, Time, Variant, PopTotal, Location) %>%
        # Pivot to get variants as columns
        tidyr::pivot_wider(
            names_from = c(Time, Variant),
            values_from = PopTotal,
            names_prefix = "year_"
        )
    return(un_data)
}

# Function to load and process historical data
process_historical_data <- function(historical_paths) {
    # historical_paths should be a list with paths for years 1, 1000, 1500, 1900
    historical_data_list <- list()
    
    for (year in names(historical_paths)) {
        historical_data_list[[year]] <- read_csv(historical_paths[[year]]) %>%
            select(ISO3_code, population) %>%
            rename(!!paste0("year_", year) := population)
    }
    
    # Combine all historical data
    historical_combined <- Reduce(function(x, y) {
        full_join(x, y, by = "ISO3_code")
    }, historical_data_list)
    
    return(historical_combined)
}

# Function to match country codes between population data and map data
harmonize_country_codes <- function(population_data, map_data) {
    # Fix known ISO code issues - only fix what's needed
    map_data$iso_a3[map_data$name == "France"] <- "FRA"
    map_data$iso_a3[map_data$name == "Norway"] <- "NOR"
    map_data$iso_a3[map_data$name == "United States of America"] <- "USA"
    map_data$iso_a3[map_data$name == "United States"] <- "USA"
    
    # Project to Mollweide projection
    map_data <- st_transform(map_data, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
    
    # Fix invalid geometries and remove empty geometries
    map_data <- st_make_valid(map_data)
    map_data <- map_data[!st_is_empty(map_data), ]
    
    # Add continent information
    map_data$continent <- as.factor(map_data$continent)
    
    # Join with population data
    matched_data <- dplyr::left_join(map_data, population_data, 
                                    by = c("iso_a3" = "ISO3_code"))
    
    # Remove rows with missing population data
    matched_data <- matched_data[!is.na(matched_data[[grep("year_", names(matched_data), value = TRUE)[1]]]), ]
    
    # Ensure geometry validity
    matched_data <- st_make_valid(matched_data)
    
    return(matched_data)
} 