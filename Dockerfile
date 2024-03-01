# Use the rocker/shiny base image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    pandoc \
    pandoc-citeproc \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
# Customize this list based on the packages your app uses
RUN R -e "install.packages(c('shiny', 'tidyverse', 'plotly', 'scales', 'reactable', 'rsconnect', 'packrat', 'htmltools', 'EpiEstim', 'readxl', 'readr', 'zoo', 'lubridate', 'arrow', 'httr', 'jsonlite', 'curl', 'MMWRweek', 'viridis', 'rmarkdown', 'knitr'), repos='https://cran.rstudio.com/')"

# Copy your Shiny app's files into the container
COPY . /srv/shiny-server/

# Make the ShinyApp available at port 3838
EXPOSE 3838

# Serve the R Markdown document as a Shiny app
CMD ["R", "-e", "rmarkdown::run('/srv/shiny-server/dashboard.Rmd', shiny_args = list(port = 3838, host = '0.0.0.0'))"]
