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
  
  # Central Asia (now without Tajikistan)
  central_asian_countries <- c(
    "Kazakhstan", "Uzbekistan", "Kyrgyzstan", "Turkmenistan"
  )
  world_data$region[world_data$name %in% central_asian_countries] <- "Central Asia"
  
  # North Africa & West Asia (renamed from North Africa & Middle East)
  nafr_westasia_countries <- c(
    "Morocco", "Algeria", "Tunisia", "Libya", "Egypt", 
    "Israel", "Palestine", "Jordan", "Lebanon", "Syria", "Iraq", "Iran", 
    "Saudi Arabia", "Yemen", "Oman", "United Arab Emirates", "Qatar", "Bahrain", "Kuwait",
    "Afghanistan", "Tajikistan"
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
    # Keep the existing CSS but add some layout improvements
    tags$head(
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
                font-family: 'Roboto Mono', monospace;
                font-size: 13px;
                line-height: 1.5;
                background-color: #f8f9fa;
                border-radius: 6px;
                padding: 15px;
                border: 1px solid #e9ecef;
                box-shadow: 0 1px 3px rgba(0,0,0,0.05);
                white-space: pre-wrap;
                max-height: 500px;
                overflow-y: auto;
            }
            
            /* Highlight the world total */
            pre:first-line {
                font-weight: bold;
                color: #333;
                font-size: 14px;
            }
            
            /* Layout styles */
            .map-container {
                min-height: 600px;
                padding-top: 0;
                margin-top: 0;
                border-radius: 6px;
                overflow: hidden;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
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
            
            /* Alignment styles */
            .row {
                display: flex;
                align-items: flex-start;
            }
            
            .sidebar-column, .map-column {
                margin-top: 0;
                padding-top: 0;
            }
            
            .well {
                margin-top: 0;
                border-radius: 6px;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
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
        "))
    ),
    
    # Use custom class for title
    div(class = "main-title",
        h1("World Population Cartograms: Visualizing Humanity from 1000 BC into the Future", 
           align = "center", style = "font-size: 24px;")
    ),
    
    # Main layout with sidebar and map
    fluidRow(
        # Sidebar panel
        column(width = 3, class = "sidebar-column",
            div(class = "well", style = "margin-top: 0; padding-top: 15px;",
                selectInput("year", "Select Year:",
                           choices = c(
                               # Historical years
                               "-1000" = "-1000", # 1000 BC
                               "0" = "0",         # 0 AD
                               "500" = "500",     # 500 AD
                               "1000" = "1000",   # 1000 AD
                               "1500" = "1500",   # 1500 AD
                               "1900" = "1900",   # 1900 AD
                               # Modern years
                               "1950" = "1950",
                               "2025" = "2025",
                               "2050" = "2050",
                               "2100" = "2100"
                           ),
                           selected = "2025"),
                
                # Only show variant selector for years after 1950
                conditionalPanel(
                    condition = "parseInt(input.year) > 1950",
                    selectInput("variant", "Population Projection:",
                              choices = c(
                                  # Standard variants
                                  "Low", "Medium", "High", 
                                  # Special variants
                                  "Constant fertility", "Instant replacement", "Zero migration", 
                                  "Constant mortality", "No change", "Momentum",
                                  "Instant replacement zero migration", "No fertility below age 18",
                                  "Accelerated ABR decline", "Accelerated ABR decline w/rec.", 
                                  # Probabilistic intervals
                                  "Median PI", "Upper 80 PI", "Lower 80 PI", "Upper 95 PI", "Lower 95 PI"
                              ),
                              selected = "Medium"),
                    helpText("Different projection variants available for future years")
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
                    helpText("Historical data shown for this year")
                ),
                conditionalPanel(
                    condition = "parseInt(input.year) > 1950",
                    helpText("Projection data shown for this year")
                )
            )
        ),
        
        # Map panel
        column(width = 9, class = "map-column",
            div(class = "map-container", style = "margin-top: 0; padding-top: 0;",
                plotOutput("map", height = "650px")
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
        )
    )
)

# Server logic
server <- function(input, output) {
    
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
    
    # Generate map
    output$map <- renderPlot({
        # Get population column for selected year and variant
        pop_col <- get_pop_col()
        
        if(input$view == "Normal Map") {
            current_map <- world_moll
        } else {
            tryCatch({
                temp_data <- world_merc
                temp_data[[pop_col]] <- pmax(temp_data[[pop_col]], 0.1)
                
                current_map <- cartogram_cont(temp_data, 
                                           weight = pop_col,
                                           itermax = 38,
                                           threshold = 0.006)
                
                current_map <- st_make_valid(current_map)
                
            }, error = function(e) {
                message("Error in cartogram generation: ", e$message)
                current_map <- world_moll
            })
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
                    # Use a more sensitive transformation
                    # trans = "log10",
                    # Adjust breaks for better distribution
                    # breaks = scales::breaks_log(n = 3),
                    # Use the plasma variant for better differentiation
                    # option = "plasma",
                    # Adjust limits to focus on the meaningful range
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
        
        # Add common theme elements
        p + theme_void() +
            labs(
                title = paste("World Population -", input$year, 
                            if(input$year != "1950") paste0(" (", input$variant, " projection)") else ""),
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
        
        # Add each country with its rank and percentage, better formatted
        country_text <- ""
        for(i in 1:nrow(top_20)) {
            # Format with fixed-width for better alignment
            rank_num <- sprintf("%2d. ", i)
            country_name <- sprintf("%-25s", paste0(top_20$name[i], ":"))
            pop_value <- format(top_20$population[i], big.mark=",")
            pct_value <- sprintf("(%5.1f%%)", top_20$percentage[i])
            
            country_text <- paste0(
                country_text,
                rank_num, 
                country_name, " ", 
                pop_value, " ", 
                pct_value,
                "\n"
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
            region_name <- sprintf("%-25s", paste0(region_totals$region[i], ":"))
            pop_value <- format(region_totals$total_population[i], big.mark=",")
            pct_value <- sprintf("(%5.1f%%)", percentage)
            
            totals_text <- paste0(
                totals_text,
                region_name, " ", 
                pop_value, " ", 
                pct_value,
                "\n"
            )
        }
        
        return(totals_text)
    })
}

# Run the app
shinyApp(ui = ui, server = server) 