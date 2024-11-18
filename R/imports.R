#' @import ggplot2 dplyr shiny shinydashboard leaflet
NULL 

#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that
#' @importFrom readr read_csv
#' @importFrom purrr imap_dfr map map_chr map_dfr reduce set_names
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom rlang arg_match
#' @importFrom shinyWidgets addSpinner pickerInput
#' @importFrom tidyr pivot_longer unite
#' @importFrom phsopendata get_dataset
#' @importFrom reactable reactable reactableOutput renderReactable
NULL

utils::globalVariables(".")