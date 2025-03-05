# Set PROJ_LIB environment variable
Sys.setenv(PROJ_LIB = "/opt/homebrew/opt/proj/share/proj")

# Load required packages
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(sf)
library(cartogram)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(viridis)

# Set tmap options
tmap_options(check.and.fix = TRUE)
tmap_mode("plot")  # Changed to plot mode for static maps
# Disable s2 geometry globally for this session
sf::sf_use_s2(FALSE)

# Load data once at startup
world <- ne_countries(scale = "medium", returnclass = "sf")
un_data <- process_un_population_data("modified_UN_population_data_1950_to_2100.csv")
world_with_pop <- harmonize_country_codes(un_data, world)

# After the package loading section, add these color definitions
continent_colors <- c(
    "North America" = "#E41A1C",
    "South America" = "#377EB8",
    "Europe" = "#4DAF4A",
    "Africa" = "#984EA3",
    "Asia" = "#FF7F00",
    "Oceania" = "#FFFF33",
    "Antarctica" = "#A65628"
)

# Transform to Mollweide projection for normal map and Mercator for cartogram
world_moll <- st_transform(world_with_pop, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
world_merc <- st_transform(world_with_pop, 3857)  # Keep Mercator for cartogram

# UI
ui <- fluidPage(
    titlePanel("World Population Cartogram"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("year", "Select Year:",
                       choices = c("1950", "2025", "2050", "2100"),
                       selected = "2025"),
            selectInput("variant", "Population Projection:",
                       choices = c("Low", "Medium", "High"),
                       selected = "Medium"),
            selectInput("view", "View Type:",
                       choices = c("Normal Map", "Population Cartogram"),
                       selected = "Normal Map"),
            hr(),
            helpText("The size of each country is proportional to its population"),
            helpText("1950 data is historical, other years are projections")
        ),
        mainPanel(
            plotOutput("map", height = "600px"),
            verbatimTextOutput("population_info")
        )
    )
)

# Server logic
server <- function(input, output) {
    
    # Generate map
    output$map <- renderPlot({
        # Get population column for selected year and variant
        pop_col <- paste0("year_", input$year, "_", input$variant)
        
        if(input$view == "Normal Map") {
            current_map <- world_moll
        } else {
            tryCatch({
                temp_data <- world_merc
                temp_data[[pop_col]] <- pmax(temp_data[[pop_col]], 0.1)
                
                current_map <- cartogram_cont(temp_data, 
                                           weight = pop_col,
                                           itermax = 15,
                                           threshold = 0.01)
                
                current_map <- st_make_valid(current_map)
                
            }, error = function(e) {
                message("Error in cartogram generation: ", e$message)
                current_map <- world_moll
            })
        }
        
        # Create plot
        ggplot(current_map) +
            geom_sf(aes(fill = !!sym(pop_col)), color = "white", linewidth = 0.1) +
            scale_fill_viridis_c(
                name = "Population (thousands)",
                labels = scales::comma,
                guide = guide_colorbar(
                    barwidth = 15,
                    barheight = 0.5,
                    title.position = "top",
                    title.hjust = 0.5
                )
            ) +
            theme_void() +
            labs(
                title = paste("World Population -", input$year, 
                            if(input$year != "1950") paste0(" (", input$variant, " projection)") else ""),
                subtitle = if(input$view == "Population Cartogram") 
                    "Country sizes are proportional to population" else "Equal-area projection"
            ) +
            theme(
                plot.background = element_rect(fill = "#f5f5f4", color = NA),
                panel.background = element_rect(fill = "#f5f5f4", color = NA),
                legend.background = element_rect(fill = "#f5f5f4", color = NA),
                plot.title = element_text(
                    size = 16,
                    hjust = 0.5,
                    margin = margin(b = 5)
                ),
                plot.subtitle = element_text(
                    size = 12,
                    hjust = 0.5,
                    margin = margin(b = 10)
                ),
                legend.position = "bottom"
            )
    })
    
    # Update population info to include variant
    output$population_info <- renderText({
        pop_col <- paste0("year_", input$year, "_", input$variant)
        
        major_countries <- c("United States of America", "China", "India", 
                           "France", "Brazil", "Nigeria")
        
        pop_data <- st_drop_geometry(world_merc)
        pop_data <- pop_data[pop_data$name %in% major_countries, ]
        pop_data <- data.frame(
            name = pop_data$name,
            population = pop_data[[pop_col]]
        )
        pop_data <- pop_data[order(pop_data$population, decreasing = TRUE), ]
        
        # Format output with variant info
        pop_text <- paste0("Population (thousands) in ", input$year)
        if(input$year != "1950") {
            pop_text <- paste0(pop_text, " (", input$variant, " projection)")
        }
        pop_text <- paste0(pop_text, ":\n")
        
        for(i in 1:nrow(pop_data)) {
            pop_text <- paste0(pop_text, 
                             pop_data$name[i], ": ", 
                             format(pop_data$population[i], big.mark=","), 
                             "\n")
        }
        pop_text
    })
}

# Run the app
shinyApp(ui = ui, server = server) 