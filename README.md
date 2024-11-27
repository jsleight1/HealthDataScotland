
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HealthDataScotland

<!-- badges: start -->

[![R-CMD-check](https://github.com/jsleight1/HealthDataScotland/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jsleight1/HealthDataScotland/actions/workflows/check-standard.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jsleight1/HealthDataScotland/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jsleight1/HealthDataScotland/?branch=main)
<!-- badges: end -->

This repo contains an R package to create the
[HealthDataScotland](https://jack-sleight.shinyapps.io/healthdatascotland/)
shiny application. The shiny application generates interactive maps
showing the locations of GP practices and hospitals across Scotland and
presents associated demographic and bed capacity data. These maps can be
used to select individual GP practices and hospitals to interogate the
data for a single centre. Conversely comparisons can be made across
centres by using the map draw function to selected multiple health
centres. The processed data is also presented in a table format, which
can be filtered and downloaded.

## Installation

You can install the development version of HealthDataScotland from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
devtools::install_github("jsleight1/HealthDataScotland")
```

HealthDataScotland also contains a `renv.lock` file which can be used to
create a reproducible environment using
[renv](https://rstudio.github.io/renv/articles/renv.html).

## Example

The shiny application can be launched locally using:

``` r
library(HealthDataScotland)
health_data_scotland()
```

## Data and references.

GP and Hospital is downloaded from the [Scottish Health and Social Care
Open Data platform](https://www.opendata.nhs.scot/). Spatial data for
interactive maps are obtained from the [Spatial Hub
Scotland](https://data.spatialhub.scot/) and from data.gov.uk. For
further information please refer to these databases.
