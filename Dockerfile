ARG RVERSION=4.4.2
FROM rocker/r-ver:$RVERSION
ARG TARGETPLATFORM

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
    gdebi-core \
    libgit2-dev \
    git \
    gnupg \
    pandoc \
    rsync \
    sudo \
    texlive-latex-base \
    texlive-fonts-extra \
    texlive-fonts-recommended \
    texlive-latex-extra

# Install quarto
RUN target=$(echo "$TARGETPLATFORM" | sed "s/\//-/") \
    && quarto_file=$(echo "https://github.com/quarto-dev/quarto-cli/releases/download/v1.8.24/quarto-1.8.24-$target.deb") \
    && curl -LO "$quarto_file" && \
    gdebi --non-interactive quarto-1.8.24-$target.deb

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
RUN git checkout -b "23-investigate-production-and-development-configs" "origin/23-investigate-production-and-development-configs"
RUN rm -rf .Rprofile renv
RUN Rscript -e "install.packages('renv')"
RUN Rscript -e "renv::restore()"
RUN Rscript -e "devtools::install()"

# Expose port and run shiny application
USER app
EXPOSE 9002
CMD ["R", "-e", "shiny::runApp()"]
