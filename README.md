# 🌍 World Population Cartogram Visualization

An interactive visualization showing how our world's population has changed and is projected to change from 1950 to 2100. This project uses cartograms to represent countries with sizes proportional to their population, offering an intuitive way to understand global demographic shifts.

## 🚀 Features

- **Interactive Time Travel**: Navigate through population data from 1950 to 2100
- **Multiple Projections**: Explore Low, Medium, and High population projection variants
- **Dual View Modes**: 
  - Equal-area map projection for accurate geographical representation
  - Population-weighted cartogram where country sizes reflect population
- **Real-time Population Data**: View exact population figures for major countries
- **Intuitive Visualization**: Countries grow and shrink based on their population, creating an immediate visual impact

## 📊 Data Sources

This visualization combines data from:
- UN Population Division (1950-2100 projections)
- Natural Earth (geographical data)

## 🛠️ Installation

1. Clone the repository:
bash
git clone https://github.com/yourusername/world-population-cartogram.git
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

[Consider adding screenshots or GIFs of your visualization here]

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

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🌟 Acknowledgments

- UN Population Division for comprehensive population data
- Natural Earth for detailed geographical data
- R Spatial community for excellent tools and documentation