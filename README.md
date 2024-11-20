
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HealthDataScotland

<!-- badges: start -->

[![R-CMD-check](https://github.com/jsleight1/HealthDataScotland/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jsleight1/HealthDataScotland/actions/workflows/check-standard.yaml)
<!-- badges: end -->

HealthDataScotland contains a shiny application for the presentation of
general practice (GP) demographic and hospital bed capacity data. The
shiny application presents interactive maps of the locations of GP
practices and hospital across Scotland. Indiviual GP practices and
hospitals can be clicked to interogate the data was single centre.
Conversely comparisons can be made across centres by using the map draw
function to selected multiple health centres. The processed data is also
presented in a table format. This can be downlaoded in full or can be
filtered to downloaded a subset of the data set.

## Installation

You can install the development version of HealthDataScotland from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jsleight1/HealthDataScotland")
```

You can also install HealthDataScotland using
[renv](https://rstudio.github.io/renv/articles/renv.html) to create a
reproducible environment for development.

## Example

The shiny application can be launched locally using:

``` r
library(HealthDataScotland)
health_data_scotland()
```

## Data and references.

GP and Hospital is downloaded from the [Scottish Health and Social Care
Open Data platform](https://www.opendata.nhs.scot/) using the
[phsopendata](https://github.com/Public-Health-Scotland/phsopendata/) R
package. Spatial data for the generation of interactive maps are
obtained from the [Spatial Hub Scotland](https://data.spatialhub.scot/)
and from data.gov.uk. For further information please refer to these
databases.
