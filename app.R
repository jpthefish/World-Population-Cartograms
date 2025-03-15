# Remove commented out PROJ_LIB line since it's not needed on the server
# Sys.setenv(PROJ_LIB = "/opt/homebrew/opt/proj/share/proj")

# Optimize package loading - only load what's actually needed
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(sf)
library(cartogram)
library(ggplot2)
library(viridis)
# tmap is only used for tmap_options and tmap_mode, consider if you need it
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)

# Source the data processing functions
source("data_processing.R")

# Set tmap options
tmap_options(check.and.fix = TRUE)
tmap_mode("plot")
sf::sf_use_s2(FALSE)

# Remove unused continent_colors variable
# continent_colors <- c(...) - This isn't used anywhere

# Load data once at startup
world <- ne_countries(scale = "medium", returnclass = "sf")
un_data <- process_un_population_data("mergedHistoricalAndProjectionData.csv")
world_with_pop <- harmonize_country_codes(un_data, world)

# Update the region colors with the new name
region_colors <- c(
  "Europe" = "#5B7DB1",                  # Medium blue (primary)
  "East Asia" = "#C25D5D",               # Medium red (primary)
  "South Asia" = "#BEA264",              # Medium gold (primary)
  "Southeast Asia" = "#C27F5D",          # Terracotta (mix of red and yellow)
  "Central Asia" = "#8A7CA0",            # Muted purple (mix of red and blue)
  "North Africa & West Asia" = "#8A9A78", # Sage green (mix of yellow and blue)
  "Sub-Saharan Africa" = "#7D6E4F",      # Warmer brown with more yellow/gold undertones
  "Latin America" = "#70919c",           # More saturated blue-purple (more distinct from Europe)
  "Northern America" = "#597183",        # Darker blue (more distinct from Europe)
  "Oceania" = "#8470A3",                 # Lavender (purple with blue undertones)
  "Other" = "#CCCCCC"                    # Gray
)

# Update the region definitions
define_regions <- function(world_data) {
  # Create a new column for regions
  world_data$region <- "Other"
  
  # Print unique country names to diagnose issues
  message("Unique country names in data:")
  print(sort(unique(world_data$name)))
  
  # Europe (including Turkey, Russia, Georgia, Azerbaijan, Armenia)
  european_countries <- c(
    "Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herz.", 
    "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", 
    "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", 
    "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", 
    "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", 
    "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", 
    "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom", "Vatican",
    "Turkey", "Georgia", "Azerbaijan", "Armenia"
  )
  world_data$region[world_data$name %in% european_countries] <- "Europe"
  
  # Northern America (renamed from North America)
  northern_american_countries <- c(
    "United States of America", "Canada", "Greenland"
  )
  world_data$region[world_data$name %in% northern_american_countries] <- "Northern America"
  
  # Check for South Sudan exact name
  if("S. Sudan" %in% world_data$name) {
    message("Found 'S. Sudan' in data")
    world_data$region[world_data$name == "S. Sudan"] <- "Sub-Saharan Africa"
  } else {
    message("'S. Sudan' not found in data")
  }
  
  # Check for Somaliland exact name
  if("Somaliland" %in% world_data$name) {
    message("Found 'Somaliland' in data")
    # If Somalia exists, use its geometry and remove Somaliland
    if("Somalia" %in% world_data$name) {
      message("Attempting to merge Somaliland with Somalia")
      # Just assign the region and we'll handle it later if needed
      world_data$region[world_data$name == "Somaliland"] <- "Sub-Saharan Africa"
    }
  }
  
  # East Asia
  east_asian_countries <- c(
    "China", "Japan", "North Korea", "South Korea", "Mongolia", "Taiwan"
  )
  world_data$region[world_data$name %in% east_asian_countries] <- "East Asia"
  
  # South Asia (Indian subcontinent)
  south_asian_countries <- c(
    "India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal", "Bhutan", "Maldives"
  )
  world_data$region[world_data$name %in% south_asian_countries] <- "South Asia"
  
  # Southeast Asia
  southeast_asian_countries <- c(
    "Indonesia", "Malaysia", "Philippines", "Singapore", "Thailand", "Vietnam", 
    "Myanmar", "Cambodia", "Laos", "Brunei", "Timor-Leste"
  )
  world_data$region[world_data$name %in% southeast_asian_countries] <- "Southeast Asia"
  
  # Central Asia (now including Tajikistan)
  central_asian_countries <- c(
    "Kazakhstan", "Uzbekistan", "Kyrgyzstan", "Turkmenistan", "Tajikistan"
  )
  world_data$region[world_data$name %in% central_asian_countries] <- "Central Asia"
  
  # North Africa & West Asia (now without Tajikistan)
  nafr_westasia_countries <- c(
    "Morocco", "Algeria", "Tunisia", "Libya", "Egypt", 
    "Israel", "Palestine", "Jordan", "Lebanon", "Syria", "Iraq", "Iran", 
    "Saudi Arabia", "Yemen", "Oman", "United Arab Emirates", "Qatar", "Bahrain", "Kuwait",
    "Afghanistan"
  )
  world_data$region[world_data$name %in% nafr_westasia_countries] <- "North Africa & West Asia"
  
  # Sub-Saharan Africa
  subsaharan_countries <- c(
    "Nigeria", "Ethiopia", "South Africa", "Kenya", "Tanzania", "Ghana", "Cameroon",
    "Côte d'Ivoire", "Angola", "Sudan", "Mauritania", "Senegal", "Mali", "Niger",
    "Chad", "Somalia", "Djibouti", "Eritrea", "Uganda", "Rwanda", 
    "Burundi", "Congo", "Dem. Rep. Congo", "Gabon", "Eq. Guinea", "Central African Rep.",
    "Benin", "Togo", "Burkina Faso", "Guinea", "Guinea-Bissau", "Sierra Leone", 
    "Liberia", "Gambia", "Zambia", "Zimbabwe", "Malawi", "Mozambique", "Botswana", 
    "Namibia", "Lesotho", "eSwatini", "Madagascar", "Comoros", "Mauritius", "Seychelles",
    "Cabo Verde", "São Tomé and Principe"
  )
  world_data$region[world_data$name %in% subsaharan_countries] <- "Sub-Saharan Africa"
  
  # Latin America
  latin_american_countries <- c(
    "Mexico", "Brazil", "Colombia", "Argentina", "Peru", "Venezuela", "Chile", 
    "Ecuador", "Guatemala", "Cuba", "Haiti", "Dominican Rep.", "Bolivia", "Honduras", 
    "Paraguay", "El Salvador", "Nicaragua", "Costa Rica", "Panama", "Puerto Rico", 
    "Uruguay", "Jamaica", "Trinidad and Tobago", "Guyana", "Suriname", "Belize", 
    "Bahamas", "Barbados", "Saint Lucia", "Grenada", "St. Vin. and Gren.", 
    "Antigua and Barb.", "Dominica", "St. Kitts and Nevis"
  )
  world_data$region[world_data$name %in% latin_american_countries] <- "Latin America"
  
  # Oceania
  oceania_countries <- c(
    "Australia", "New Zealand", "Papua New Guinea", "Fiji", "Solomon Is.", 
    "Vanuatu", "Samoa", "Kiribati", "Micronesia", "Tonga", "Marshall Is.", 
    "Palau", "Tuvalu", "Nauru"
  )
  world_data$region[world_data$name %in% oceania_countries] <- "Oceania"
  
  # Print diagnostic info about regions
  message("Region counts:")
  print(table(world_data$region))
  
  # Convert to factor with levels in the order of region_colors
  world_data$region <- factor(world_data$region, levels = names(region_colors))
  
  return(world_data)
}

# Transform to Mollweide projection for normal map and Mercator for cartogram
world_moll <- st_transform(world_with_pop, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
world_merc <- st_transform(world_with_pop, 3857)  # Keep Mercator for cartogram

# Apply the region definitions to both map projections
world_moll <- define_regions(world_moll)
world_merc <- define_regions(world_merc)

# UI
ui <- fluidPage(
    # Add viewport meta tag for proper mobile scaling
    tags$head(
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
        tags$style(HTML("
            @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500&family=Roboto+Mono:wght@400&display=swap');
            
            body, h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {
                font-family: 'Roboto', sans-serif;
            }
            
            .main-title {
                font-weight: 500;
                margin-bottom: 25px;
                padding-bottom: 15px;
                border-bottom: 1px solid #e0e0e0;
            }
            
            .section-title {
                font-weight: 500;
                color: #333;
                margin-top: 20px;
                margin-bottom: 15px;
                border-left: 4px solid #4890c6;
                padding-left: 10px;
            }
            
            /* Improved styling for statistics boxes */
            pre {
                font-family: 'JetBrains Mono', 'Roboto Mono', 'Consolas', monospace;
                font-size: 13px;
                line-height: 1.5;
                background-color: #f8f9fa;
                border-radius: 6px;
                padding: 10px;
                border: 1px solid #e9ecef;
                box-shadow: 0 1px 3px rgba(0,0,0,0.05);
                white-space: pre-wrap;
                max-height: 500px;
                overflow-y: auto;
            }
            
            /* Import JetBrains Mono for better monospace display */
            @import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500&display=swap');
            
            /* Highlight the world total */
            pre:first-line {
                font-weight: 500;
                color: #333;
                font-size: 14px;
            }
            
            /* Layout styles */
            .map-container {
                min-height: 400px;
                padding: 0;
                margin: 0;
                border-radius: 6px;
                overflow: hidden;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
                width: 100%;
            }
            
            .stats-container {
                display: flex;
                flex-wrap: wrap;
                margin-top: 20px;
                width: 100%;
                gap: 20px;
            }
            
            .stats-box {
                flex: 1;
                min-width: 300px;
                background-color: white;
                border-radius: 6px;
                padding: 15px;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            }
            
            /* Responsive layout styles */
            @media (max-width: 768px) {
                .main-layout {
                    flex-direction: column;
                }
                
                .sidebar-column, .map-column {
                    width: 100% !important;
                    max-width: 100% !important;
                    flex: 0 0 100% !important;
                }
                
                .map-container {
                    height: 400px !important;
                    margin-bottom: 20px;
                }
                
                .stats-box {
                    min-width: 100%;
                }
                
                h1 {
                    font-size: 20px !important;
                }
                
                .well {
                    margin-bottom: 20px;
                }
            }
            
            /* Alignment styles */
            .main-layout {
                display: flex;
                flex-wrap: wrap;
                align-items: flex-start;
                margin: 0 -15px;
            }
            
            .sidebar-column, .map-column {
                padding: 0 15px;
                box-sizing: border-box;
            }
            
            .well {
                margin-top: 0;
                border-radius: 6px;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
                padding: 15px;
            }
            
            /* Full-width container for stats */
            .full-width-container {
                width: 100%;
                margin-top: 30px;
                padding: 0 15px;
            }
            
            /* Enhance the select inputs */
            .selectize-input {
                border-radius: 4px;
            }
            
            /* Style the radio buttons */
            .radio label {
                padding: 8px 15px;
                margin-right: 5px;
                background-color: #f8f9fa;
                border-radius: 4px;
                transition: background-color 0.2s;
            }
            
            .radio label:hover {
                background-color: #e9ecef;
            }
            
            /* Add a subtle background to the page */
            body {
                background-color: #f5f7fa;
            }
            
            /* Fix map height issues */
            #map {
                width: 100% !important;
                height: 100% !important;
            }
            
            /* Adjust map height based on viewport width */
            @media (min-width: 992px) {
                .map-container {
                    height: 650px !important;
                }
            }
            
            @media (min-width: 768px) and (max-width: 991px) {
                .map-container {
                    height: 500px !important;
                }
            }
            
            /* Ensure minimum dimensions for the plot */
            .map-container {
                min-height: 300px !important;
                min-width: 200px !important;
            }
            
            /* Responsive font size for statistics */
            @media (max-width: 480px) {
                .main-title h1 {
                    font-size: 18px !important;
                }
                
                .section-title {
                    font-size: 16px !important;
                }
                
                .selectize-input {
                    font-size: 14px;
                }
                
                .stats-box {
                    padding: 10px;
                }
                
                pre {
                    font-size: 11px;
                }
            }
            
            /* Fix for plot rendering */
            .shiny-plot-output {
                min-height: 300px;
                min-width: 200px;
            }
            
            /* Copyright footer styling */
            .footer {
                text-align: center;
                padding: 20px 0;
                margin-top: 30px;
                color: #666;
                font-size: 12px;
                border-top: 1px solid #e0e0e0;
            }
            
            /* Add more bottom space to the stats container */
            .stats-container {
                margin-bottom: 20px;
            }
        "))
    ),
    
    # Use custom class for title
    div(class = "main-title",
        h1("World Population Cartograms: Visualizing Humanity from 1000 BC into the Future", 
           align = "center", style = "font-size: 24px;"),
        p(HTML("For more information on the datasets and methods used in this application, please visit the <a href='https://github.com/jpthefish/World-Population-Cartograms/' target='_blank'>documentation page on GitHub</a>."), 
          align = "center", 
          style = "margin-top: 10px; color: #666; font-size: 14px;")
    ),
    
    # Main layout with sidebar and map - using custom class for responsive layout
    fluidRow(
        # Sidebar panel - will stack on mobile
        column(width = 12, class = "col-md-3 col-sm-12",
            div(class = "well",
                selectInput("year", "Select Year:",
                           choices = c(
                               # Historical years
                               "1000 B.C." = "-1000", # 1000 BC
                               "1 A.D." = "0",        # 0 AD
                               "500 A.D." = "500",    # 500 AD
                               "1000 A.D." = "1000",  # 1000 AD
                               "1500 A.D." = "1500",  # 1500 AD
                               "1900 A.D." = "1900",  # 1900 AD
                               # Modern years
                               "1950 A.D." = "1950",
                               "2025" = "2025",
                               "2050 (Projection)" = "2050",
                               "2100 (Projection)" = "2100"
                           ),
                           selected = "2025"),
                
                # Only show variant selector for years after 1950
                conditionalPanel(
                    condition = "parseInt(input.year) > 2025",
                    selectInput("variant", "Population Projection:",
                              choices = c(
                                  # Standard variants
                                  "IHME reference pace", "IHME sustainable development goals (SDG) pace", "Low", "Medium", "High", 
                                  # Special variants
                                  "Constant fertility", "Instant replacement", "Zero migration", 
                                  "Constant mortality", "Instant replacement zero migration"
                              ),
                              selected = "Medium"),
                    helpText("Different projection variants available from the UN Population Division (2024) and IHME Population Forecasts (2020)")
                ),
                
                selectInput("view", "View Type:",
                           choices = c("Normal Map", "Population Cartogram"),
                           selected = "Normal Map"),
                radioButtons("color_by", "Color countries by:",
                            choices = c("Population" = "population", 
                                       "Region" = "region"),
                            selected = "population"),
                hr(),
                conditionalPanel(
                    condition = "parseInt(input.year) <= 1950",
                    helpText("Historical data shown for this year (estimates from HYDE, v 3.3)")
                ),
                conditionalPanel(
                    condition = "parseInt(input.year) > 2025",
                    helpText("Projection data shown for this year")
                )
            )
        ),
        
        # Map panel
        column(width = 12, class = "col-md-9 col-sm-12",
            div(class = "map-container",
                plotOutput("map", 
                          height = "500px", # Fixed height instead of 100%
                          width = "100%",
                          hover = hoverOpts(id = "plot_hover", delayType = "debounce", delay = 100)),
                # Remove the conditional panel restriction so hover works on both map types
                uiOutput("hover_info")
            )
        )
    ),
    
    # Full-width container for statistics
    div(class = "full-width-container",
        hr(),
        h3("Population Statistics", align = "center", class = "section-title", 
           style = "border-left: none; border-bottom: 2px solid #4890c6; padding-bottom: 10px;"),
        
        # Side-by-side layout for statistics
        div(class = "stats-container",
            # Left column: Regional totals
            div(class = "stats-box",
                h4("Regional Population Totals (thousands)", class = "section-title"),
                verbatimTextOutput("region_totals")
            ),
            
            # Right column: Top 20 countries
            div(class = "stats-box",
                h4("Top 20 Most Populous Countries (thousands)", class = "section-title"),
                verbatimTextOutput("top_countries")
            )
        ),
        
        # Add copyright footer
        div(class = "footer",
            HTML(paste0("© ", format(Sys.Date(), "%Y"), " Joey Paul Eli Haynes"))
        )
    )
)

# Server logic
server <- function(input, output) {
    
    # Create a reactive cache for cartograms
    cartogram_cache <- reactiveVal(list())

    # Optimize cartogram generation parameters
    get_cartogram <- function(year, variant, color_by) {
        # Create a unique key for this cartogram
        cache_key <- paste(year, variant, sep = "_")
        
        # Check if we already have this cartogram in cache
        cached_cartograms <- cartogram_cache()
        if (cache_key %in% names(cached_cartograms)) {
            message("Using cached cartogram for ", cache_key)
            return(cached_cartograms[[cache_key]])
        }
        
        # If not in cache, generate the cartogram
        message("Generating new cartogram for ", cache_key)
        
        # Get population column
        pop_col <- paste0("year_", year, "_", variant)
        if(!pop_col %in% names(world_merc)) {
            pop_col <- paste0("year_", year, "_Medium")
        }
        
        # Generate the cartogram
        temp_data <- world_merc
        temp_data[[pop_col]] <- pmax(temp_data[[pop_col]], 1)
        
        tryCatch({
            # Create cartogram with optimized parameters
            cart <- cartogram_cont(temp_data, 
                                 weight = pop_col,
                                 itermax = 38,
                                 maxSizeError = 0.07,
                                 prepare = "adjust",
                                 threshold = 0.007)
            
            # Simplify geometry for better performance
            cart <- st_simplify(cart, preserveTopology = TRUE, dTolerance = 1000)
            cart <- st_make_valid(cart)
            
            # Add to cache
            cached_cartograms <- cartogram_cache()
            cached_cartograms[[cache_key]] <- cart
            cartogram_cache(cached_cartograms)
            
            return(cart)
        }, error = function(e) {
            message("Error generating cartogram: ", e$message)
            return(world_moll)
        })
    }
    
    # Get population column name with fallback
    get_pop_col <- reactive({
        year <- input$year
        year_num <- as.numeric(year)
        
        # For years up to 1950, only use historical data (no variants)
        if(year_num <= 1950) {
            variant <- "Medium"  # Use Medium as default for historical data
        } else {
            # Use the selected variant if available, otherwise default to Medium
            variant <- if(!is.null(input$variant)) input$variant else "Medium"
        }
        
        col_name <- paste0("year_", year, "_", variant)
        
        # Check if the column exists, if not fall back to Medium
        if(!col_name %in% names(world_merc)) {
            message("Variant ", variant, " not available for year ", year, ", falling back to Medium")
            col_name <- paste0("year_", year, "_Medium")
            
            # If still not found, this might be a historical year without the data loaded yet
            if(!col_name %in% names(world_merc)) {
                message("Year ", year, " not found in data, using 1950 as fallback")
                col_name <- "year_1950_Medium"
            }
        }
        
        return(col_name)
    })
    
    # Modify the hover_info renderUI function to work with both map types
    output$hover_info <- renderUI({
        hover <- input$plot_hover
        if(is.null(hover)) return(NULL)
        
        # Use the appropriate map data based on the view type
        if(input$view == "Normal Map") {
            current_map <- world_moll
        } else {
            # For cartogram view, use the cached cartogram
            current_map <- get_cartogram_with_disk_cache(
                input$year, 
                if(as.numeric(input$year) > 1950) input$variant else "Medium",
                input$color_by
            )
        }
        
        # Find the country at the hover point
        point <- st_point(c(hover$x, hover$y))
        point_sf <- st_sfc(point, crs = st_crs(current_map))
        
        # Find which country contains this point
        country_idx <- st_nearest_feature(point_sf, current_map)
        
        if(length(country_idx) > 0) {
            country <- current_map[country_idx, ]
            pop_col <- get_pop_col()
            
            # Format the population with commas
            pop_value <- format(country[[pop_col]], big.mark = ",")
            
            # Calculate position relative to cursor
            left_pos <- hover$coords_css$x + 15
            top_pos <- hover$coords_css$y - 10
            
            # Create a tooltip with country name and population that follows the cursor
            div(
                style = paste0(
                    "position: absolute; background-color: white; padding: 8px; ",
                    "border-radius: 4px; box-shadow: 0 0 10px rgba(0,0,0,0.3); ",
                    "z-index: 1000; font-size: 12px; pointer-events: none; ",
                    "left: ", left_pos, "px; ",
                    "top: ", top_pos, "px;"
                ),
                strong(country$name),
                tags$br(),
                paste0("Population: ", pop_value, " (thousands)")
            )
        }
    })
    
    # Update the map rendering to use disk caching
    output$map <- renderPlot({
        # Get population column for selected year and variant
        pop_col <- get_pop_col()
        
        if(input$view == "Normal Map") {
            current_map <- world_moll
        } else {
            # Get cartogram from disk cache or generate and cache it
            current_map <- get_cartogram_with_disk_cache(
                input$year, 
                if(as.numeric(input$year) > 1950) input$variant else "Medium",
                input$color_by
            )
        }
        
        # Calculate total world population
        world_total <- sum(current_map[[pop_col]], na.rm = TRUE)
        formatted_total <- format(world_total, big.mark = ",")
        
        # Format year display
        year_num <- as.numeric(input$year)
        if(year_num < 0) {
            year_display <- paste0(abs(year_num), " BC")
        } else {
            year_display <- input$year
        }
        
        # Create plot with different fill based on color_by selection
        p <- ggplot(current_map)
        
        if(input$color_by == "population") {
            # Color by population
            p <- p + 
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
                )
        } else {
            # Color by region
            p <- p + 
                geom_sf(aes(fill = region), color = "white", linewidth = 0.1) +
                scale_fill_manual(
                    name = "Region",
                    values = region_colors,
                    guide = guide_legend(
                        title.position = "top",
                        title.hjust = 0.5
                    )
                )
        }
        
        # Create title with world population total
        if(year_num <= 1950) {
            # Historical years
            title_text <- paste0("World Population, ", year_display, ": ", formatted_total, " (thousands)")
        } else {
            # Projection years
            title_text <- paste0("World Population, ", year_display, ": ", formatted_total, 
                               " (thousands, ", input$variant, " projection)")
        }
        
        # Add common theme elements
        p <- p + theme_void() +
            labs(
                title = title_text,
                subtitle = if(input$view == "Population Cartogram") 
                    "Country sizes are proportional to population" else "Mollweide equal-area projection"
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
        
        # Important: Add coord_sf() to maintain the coordinate system for hover functionality
        p + coord_sf()
    })
    
    # Remove the population_info output and add top_countries output
    output$top_countries <- renderText({
        pop_col <- get_pop_col()
        year_num <- as.numeric(input$year)
        
        # Get all countries and their populations
        country_data <- st_drop_geometry(world_merc)
        country_data <- data.frame(
            name = country_data$name,
            population = country_data[[pop_col]]
        )
        
        # Rename "United States of America" to "United States"
        country_data$name <- gsub("United States of America", "United States", country_data$name)
        
        # Calculate world total for percentages
        world_total <- sum(country_data$population, na.rm = TRUE)
        
        # Sort by population and take top 20
        country_data <- country_data[order(country_data$population, decreasing = TRUE), ]
        top_20 <- head(country_data, 20)
        
        # Add percentage column
        top_20$percentage <- (top_20$population / world_total) * 100
        
        # Format output with year info
        if(year_num < 0) {
            # BC years
            title_text <- paste0("TOP 20 COUNTRIES IN ", abs(year_num), " BC\n",
                               "═════════════════════════════════════\n\n")
        } else if(year_num <= 1950) {
            # Historical years AD
            title_text <- paste0("TOP 20 COUNTRIES IN ", year_num, " AD\n",
                               "═════════════════════════════════════\n\n")
        } else {
            # Projection years
            title_text <- paste0("TOP 20 COUNTRIES IN ", year_num, " (", input$variant, ")\n",
                               "═════════════════════════════════════\n\n")
        }
        
        # Add each country with its rank and percentage, better formatted for responsive design
        country_text <- ""
        for(i in 1:nrow(top_20)) {
            # Format with fixed-width for better alignment
            rank_num <- sprintf("%2d. ", i)
            
            # Determine if we need to use a shorter country name format for small screens
            country_name <- top_20$name[i]
            if(nchar(country_name) > 20) {
                # Truncate very long names
                country_name <- paste0(substr(country_name, 1, 18), "...")
            }
            
            # Format the country name with colon
            country_name_formatted <- sprintf("%-22s", paste0(country_name, ":"))
            
            # Format population with commas
            pop_value <- format(top_20$population[i], big.mark=",")
            
            # Format percentage on a new line for better responsive layout
            pct_value <- sprintf("\n    (%5.1f%%)", top_20$percentage[i])
            
            country_text <- paste0(
                country_text,
                rank_num, 
                country_name_formatted, " ", 
                pop_value, 
                pct_value,
                "\n\n"
            )
        }
        
        return(paste0(title_text, country_text))
    })
    
    # Region totals output
    output$region_totals <- renderText({
        pop_col <- get_pop_col()
        
        # Calculate population totals by region
        region_data <- st_drop_geometry(world_merc)
        
        # Group by region and sum population
        region_totals <- region_data %>%
            group_by(region) %>%
            summarize(
                total_population = sum(!!sym(pop_col), na.rm = TRUE),
                .groups = "drop"
            ) %>%
            arrange(desc(total_population))
        
        # Calculate world total
        world_total <- sum(region_totals$total_population)
        
        # Format output with better spacing and alignment
        totals_text <- paste0("WORLD TOTAL: ", format(world_total, big.mark=","), "\n", 
                            "═════════════════════════════════════\n\n")
        
        for(i in 1:nrow(region_totals)) {
            # Calculate percentage of world total
            percentage <- (region_totals$total_population[i] / world_total) * 100
            
            # Format with fixed-width for better alignment
            region_name <- sprintf("%-22s", paste0(region_totals$region[i], ":"))
            pop_value <- format(region_totals$total_population[i], big.mark=",")
            
            # Put percentage on a new line for better responsive layout
            pct_value <- sprintf("\n    (%5.1f%%)", percentage)
            
            totals_text <- paste0(
                totals_text,
                region_name, " ", 
                pop_value, 
                pct_value,
                "\n\n"
            )
        }
        
        return(totals_text)
    })

    # Create cache directory if it doesn't exist
    if(!dir.exists("cartogram_cache")) {
        dir.create("cartogram_cache")
    }

    # Function to get cartogram with disk caching
    get_cartogram_with_disk_cache <- function(year, variant, color_by) {
        # Create a unique key for this cartogram
        cache_key <- paste(year, variant, sep = "_")
        cache_file <- file.path("cartogram_cache", paste0(cache_key, ".rds"))
        
        # Check if we have this cartogram on disk
        if(file.exists(cache_file)) {
            message("Loading cartogram from disk: ", cache_file)
            return(readRDS(cache_file))
        }
        
        # Generate the cartogram
        cart <- get_cartogram(year, variant, color_by)
        
        # Save to disk for future use
        message("Saving cartogram to disk: ", cache_file)
        saveRDS(cart, cache_file)
        
        return(cart)
    }

    # Add this near the beginning of your server function
    options(shiny.useragg = TRUE)  # Use ragg for rendering instead of Quartz
    options(shiny.minwidth = 200)  # Set minimum width for plots
}

# Run the app
shinyApp(ui = ui, server = server) 