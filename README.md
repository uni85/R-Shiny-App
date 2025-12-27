# R Shiny App

Interactive R Shiny application for data exploration and visualization.

## Table of contents
- [About](#about)
- [Features](#features)
- [Demo](#demo)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Run the app](#run-the-app)
- [Deployment](#deployment)
- [Development](#development)
- [Contributing](#contributing)
- [License](#license)
- [Contact](#contact)

## About
This repository contains a Shiny app that lets users upload or use sample datasets, interactively filter data, view summary statistics, and create common visualizations (scatter, histogram, line, bar). It is structured to be easy to run locally, extend, and deploy.

## Features
- Upload CSV files or use included sample data
- Interactive filtering and column selection
- Multiple plot types with ggplot2
- Data table view with sorting and paging (DT)
- Download filtered data as CSV

## Demo
If you deploy to a hosting service (shinyapps.io, RStudio Connect), paste the demo link here.

## Prerequisites
- R >= 4.0
- Recommended packages: shiny, tidyverse (or at least dplyr, ggplot2, readr), DT, shinyWidgets

Install packages:
```r
install.packages(c("shiny", "tidyverse", "DT", "shinyWidgets"))
```

## Installation
Clone the repo and open in RStudio:
```bash
git clone https://github.com/uni85/R-Shiny-App.git
cd R-Shiny-App
```

## Run the app
From the project root in R:
```r
# If app.R is at the root
shiny::runApp()

# Or if app/ contains ui.R and server.R
shiny::runApp("app")
```
Or open `app.R` (or `ui.R`/`server.R`) in RStudio and click "Run App".

## Deployment
- shinyapps.io: use the `rsconnect` package
```r
install.packages("rsconnect")
rsconnect::deployApp()
```
- Docker: use a rocker/shiny base image and copy app files into `/srv/shiny-server/`.

## Development
- Keep UI in `ui.R` or `app/ui.R` and server logic in `server.R` or `app/server.R`.
- Add tests for data transformation functions with `testthat`.
- Document reusable functions with roxygen2 if packaging.

## Contributing
Contributions welcome. Please open an issue to discuss major changes and submit pull requests with clear descriptions and tests where applicable.

## License
This project is available under the MIT License. See LICENSE for details.

## Contact
Open an issue or contact the repository owner: [uni85](https://github.com/uni85).
