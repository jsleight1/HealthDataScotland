#' @import ggplot2 dplyr shiny shinydashboard leaflet
NULL

#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that
#' @importFrom readr read_csv
#' @importFrom purrr imap_dfr map map_chr map_dfr reduce
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom rlang arg_match
#' @importFrom shinyWidgets addSpinner pickerInput show_alert
#' @importFrom tidyr pivot_longer unite
#' @importFrom reactable reactable reactableOutput renderReactable
#' @importFrom httr2 request req_retry req_perform resp_body_json resp_body_string
#' @importFrom glue glue
#' @importFrom tibble as_tibble deframe
NULL

utils::globalVariables(".")