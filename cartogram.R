# Required libraries
library(sf)
library(cartogram)
library(tmap)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Process UN data only for now
un_data <- process_un_population_data("modified_UN_population_data_1950_to_2100.csv")

# Merge with spatial data and project
world_with_pop <- harmonize_country_codes(un_data, world)

# Create cartogram transformation for 2025 (as example)
world_cartogram <- cartogram_cont(world_with_pop, "year_2025_Medium")

# Visualization with continent-based coloring
tm_shape(world_cartogram) +
    tm_polygons("continent", 
                palette = "Set2",
                title = "Continents") +
    tm_layout(title = "World Population Cartogram (2025)")

# Alternative visualization with population-based coloring (commented out)
# tm_shape(world_cartogram) +
#     tm_polygons("year_2025_Medium", 
#                 style = "quantile",
#                 palette = "viridis",
#                 title = "Population (2025)") +
#     tm_layout(title = "World Population Cartogram") 