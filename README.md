# 🌍 World Population Cartograms

### Interact with the project on [jpthefish.shinyapps.io](https://jpthefish.shinyapps.io/cartograms/)

An interactive visualization showing how our world's population has changed (and is projected to change) from 1000 BC to 2100 AD. This project uses cartograms to represent countries with sizes proportional to their population, offering an intuitive way to understand global demographic events and elucidate how countries are progressing through modern demographic transitions. 

## 🚀 Features

- **Interactive Time Travel**: Navigate through population data from 1000 BC to 2100 AD
- **Multiple Projections**: Explore Low, Medium, High, and other UN population projection variants
- **Dual View Modes**: 
  - Equal-area map projection for accurate geographical representation
  - Population-weighted cartogram where country sizes reflect population
- **Real-time Population Data**: View exact population figures for countries and world regions
- **Intuitive Visualization**: Countries grow and shrink based on their population, creating an immediate visual impact

## 📊 Data Sources

This visualization combines data from:
- Historical estimates by HYDE (v3.3)
- UN Population Division (1950-2100 projections)
- Natural Earth (geographical data)

## 🛠️ Installation

1. Clone the repository:
bash
git clone https://github.com/jpthefish/world-population-cartogram.git
cd world-population-cartogram

2. Install dependencies:
R
install.packages(c(
"shiny",
"sf",
"cartogram",
"ggplot2",
"viridis",
"dplyr",
"tidyr",
"rnaturalearth",
"rnaturalearthdata"
))


## 🎮 Usage

Run the Shiny app:
R
shiny::runApp()

## 🖥️ Preview

![Screenshot 2025-03-06 at 3 15 11 AM](https://github.com/user-attachments/assets/4ad38575-770b-4bf7-ba42-3f83890abf36)

In the present day, demographic transitions are largely characterized by migration as well as 'a shift from high birth rates and high death rates to lower birth rates and death rates as societies attain more technology, education (especially of women), and economic development,' as hypothesized by theories such as the Demographic Transition Model (DTM) (https://en.wikipedia.org/wiki/Demographic_transition).

## 🔧 Technical Details

The visualization uses several sophisticated techniques:
- **Cartogram Generation**: Density-equalizing algorithm to maintain topology while adjusting areas
- **Equal-area Projection**: Mollweide projection for accurate area representation
- **Dynamic Rendering**: Real-time cartogram updates as parameters change

## 📚 Dependencies

- R >= 4.0.0
- Shiny
- sf (Simple Features for R)
- cartogram
- ggplot2
- Additional packages listed in installation section

## 🤝 Contributing

Contributions are welcome! Feel free to:
- Open issues
- Submit Pull Requests
- Suggest new features
- Improve documentation

## 🌟 Acknowledgments

- Comprehensive population data (HYDE (2023); Gapminder (2022); UN WPP (2024) – with major processing by Our World in Data. “Population” [dataset]. PBL Netherlands Environmental Assessment Agency, “History Database of the Global Environment 3.3”; Gapminder, “Population v7”; United Nations, “World Population Prospects”; Gapminder, “Systema Globalis” [original data]. Retrieved March 6, 2025 from https://ourworldindata.org/grapher/population)
- Natural Earth for detailed geographical data
- R Spatial community for excellent tools and documentation
