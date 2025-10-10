#' @import dplyr shiny leaflet bslib echarts4r
NULL

#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that
#' @importFrom bsicons bs_icon
#' @importFrom readr read_csv
#' @importFrom purrr map map_chr reduce set_names pmap
#' @importFrom rlang arg_match
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @importFrom httr2 request req_retry req_perform resp_body_json resp_body_string
#' @importFrom glue glue
#' @importFrom tibble as_tibble deframe
#' @importFrom logger log_info
#' @importFrom shinycssloaders withSpinner
#' @importFrom AzureStor storage_endpoint storage_container storage_save_rds storage_load_rds
#' @importFrom DT datatable renderDT DTOutput
NULL

utils::globalVariables(".")
