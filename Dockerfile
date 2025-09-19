ARG RVERSION=4.4.2
FROM rocker/r-ver:$RVERSION

# Install various libraries required for R packages
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
    curl \
    libfontconfig1-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libpng-dev \
    binutils \
    libproj-dev \
    libgdal-dev \
    libudunits2-dev \
    gdal-bin \
    libgit2-dev \
    git \
    gnupg \
    pandoc \
    sudo \
    texlive-latex-base \
    texlive-fonts-extra \
    texlive-fonts-recommended \
    texlive-latex-extra

# Install brave-browser required for shinytest2
RUN curl -fsS https://dl.brave.com/install.sh | sh

# Run application as 'app' user.
RUN addgroup --system app && adduser --system --ingroup app app
RUN mkdir /home/app
RUN chown app:app /home/app
ENV HOME=/home/app
WORKDIR /home/app

# Install packages required for HealthDataScotland
RUN git clone https://github.com/jsleight1/HealthDataScotland.git .
RUN git config --global --add safe.directory /home/app
RUN git checkout -b "3-use-external-data-storage" "origin/3-use-external-data-storage"
RUN rm -rf .Rprofile renv
RUN Rscript -e "install.packages('renv')"
RUN Rscript -e "renv::restore()"

# Expose port and run shiny application
USER app
EXPOSE 9002
CMD ["R", "-e", "shiny::runApp()"]
