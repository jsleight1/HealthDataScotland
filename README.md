
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HealthDataScotland

<!-- badges: start -->

[![R-CMD-check](https://github.com/jsleight1/HealthDataScotland/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jsleight1/HealthDataScotland/actions/workflows/check-standard.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jsleight1/HealthDataScotland/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jsleight1/HealthDataScotland/?branch=main)
<!-- badges: end -->

This repository contains an R package for the
<a href="https://www.healthdatascotland.co.uk/"
target="_blank">HealthDataScotland</a> shiny application. This shiny
application downloads, processes and presents demographic and bed
capacity data related to GP practices and hospitals across Scotland,
respectively.

## Installation

You can install HealthDataScotland from [GitHub](https://github.com/)
with:

``` r
devtools::install_github("jsleight1/HealthDataScotland")
```

## Examples

### R6 objects

``` r
gp_unit <- example_gp_unit()
gp_unit$metadata()
gp_unit$plot(type = "population_pyramid")
```

``` r
hospital_unit <- example_hospital_unit()
hospital_unit$metadata()
hospital_unit$plot(type = "specialty_line")
```

``` r
map_unit <- example_map_unit()
map_unit$plot(type = "interactive_map")
```

### Local shiny usage

``` r
Sys.setenv("R_CONFIG_ACTIVE" = "development")
save_processed_data()
health_data_scotland()
```

Please refer to the
[documentation](https://jsleight1.github.io/HealthDataScotland/) for
further details regarding the design and usage of this package.

# Disclaimer

This R package is licensed using a GNU General Public License and
contains data from a variety of sources licensed under the Open
Government License (OGL). As this application is primarily a hobby
project, the author is likely to make no financial gain from this
application and therefore accepts no liability, responsibility or
guarantees as to how the package is designed or how the package
downloads, processes and presents the data. The original published data
sets should always be consulted when using this application.
