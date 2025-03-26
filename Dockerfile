ARG RVERSION=4.4.2
FROM rocker/r-ver:$RVERSION

# Install various libraries required for R packages
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
    curl \
    # libz-dev \
    # libcurl4-openssl-dev \
    # libssl-dev \
    # libfontconfig1-dev \
    # libfribidi-dev \
    # libharfbuzz-dev \
    # libfreetype6-dev \
    libpng-dev \
    # libtiff5-dev \
    # libjpeg-dev \
    # libxml2-dev \
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
# RUN addgroup --system app && adduser --system --ingroup app app
# RUN mkdir /home/app
# RUN chown app:app /home/app
# ENV HOME=/home/app
# WORKDIR /home/app

# Install packages required for LGBF
RUN git clone https://github.com/jsleight1/HealthDataScotland.git \
    && cd HealthDataScotland \
    && git fetch origin \
    && git checkout -b '9-dockerise-app' 'origin/9-dockerise-app' \
    && rm -rf .Rprofile renv \
    && Rscript -e "install.packages('renv')" \
    && R -e "renv::restore()"

# Expose port and run shiny application
# USER app
# EXPOSE 9002
# CMD ["R", "-e", "shiny::runApp()"]
