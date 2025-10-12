#' R6 class storing health statistics for a single hospital.
#'
#' This R6 class is designed to store bed occupancy statistics for a single
#' hospital location. This class can be used to plot summary statistics and
#' create shiny UI/server objects.
#'
#' @examples
#' library(dplyr)
#' meta <- HealthDataScotland::example_hospital_metadata |>
#'   rename("ID" = "HospitalCode", "HBName" = "HealthBoard") |>
#'   filter(.data[["ID"]] == "A101H")
#' data <- HealthDataScotland::example_hospital_data |>
#'   rename("ID" = "Location") |>
#'   filter(.data[["ID"]] == "A101H")
#' x <- hospital[["new"]](meta, data)
#' x[["ID"]]()
#' x[["title"]]()
#' x[["address"]]()
#' x[["health_board"]]()
#' x[["metadata"]]()
#' x[["data"]]()
#' x[["plot"]](type = "specialty_line")
#' x[["plot_data"]](type = "specialty_line")
#' x[["plot_info"]](type = "specialty_line")
#' x[["summary"]](type = "specialty_summary")
#' x[["summary_info"]](type = "specialty_summary")
#' \dontrun{
#' x[["ui"]]()
#' x[["server"]]()
#' }
#' @export
hospital <- R6Class("hospital",
  inherit = health_unit,
  private = list(
    title_col = function() {
      "HospitalName"
    },
    required_metadata_cols = function() {
      c(
        "HospitalName", "Postcode", "HBName", "AddressLine1", "AddressLine2",
        "AddressLine3", "AddressLine4"
      )
    },
    required_data_cols = function() {
      c(
        "FinancialYear", "SpecialtyName", "SpecialtyNameQF", "AllStaffedBeds",
        "TotalOccupiedBeds", "AverageAvailableStaffedBeds", "AverageOccupiedBeds",
        "PercentageOccupancy", "PercentageOccupancyQF"
      )
    },
    specialty_line = function(...) {
      data <- self[["plot_data"]](type = "specialty_line", ...)
      data |>
        group_split(.data[["SpecialtyName"]]) |>
        map(function(i) {
          i |>
            group_by(name) |>
            e_trend("FinancialYear", "value") |>
            e_title(subtext = unique(i[["SpecialtyName"]]))
        }) |>
        append(c(rows = length(unique(data[["SpecialtyName"]])), cols = 1)) %>%
        do.call(e_arrange, .)
    },
    specialty_line_data = function(data_type = c("annual", "daily"),
                                   specialties = "All Specialties") {
      cols <- switch(arg_match(data_type),
        "annual" = private[["annual_cols"]],
        "daily" = private[["daily_cols"]]
      )()
      specialties <- arg_match(
        specialties,
        values = private[["specialty_choices"]](),
        multiple = TRUE
      )
      self[["combine_data"]]() |>
        filter(.data[["SpecialtyName"]] %in% specialties) |>
        select(
          "ID",
          "FinancialYear",
          "HospitalName",
          "SpecialtyName",
          "PercentageOccupancy",
          all_of(cols)
        ) |>
        pivot_longer(cols = names(cols))
    },
    annual_cols = function() {
      c(
        "Annual number of available staffed beds" = "AllStaffedBeds",
        "Annual number of occupied beds" = "TotalOccupiedBeds"
      )
    },
    daily_cols = function() {
      c(
        "Daily average number of available staffed beds" = "AverageAvailableStaffedBeds",
        "Daily average number of occupied beds" = "AverageOccupiedBeds"
      )
    },
    specialty_choices = function() {
      sort(unique(self[["data"]]()[["SpecialtyName"]]))
    },
    specialty_line_info = function(data_type = c("annual", "daily")) {
      switch(arg_match(data_type),
        "annual" = "This line chart shows the annual number of
                    available staffed beds and the number occupied (y-axis)
                    across time (x-axis). Settings can be used to show data for
                    different specialties (default is all specialities).",
        "daily" = "This line chart shows the daily average number of
                    available staffed beds and the number occupied (y-axis)
                    across time (x-axis). Settings can be used to show data for
                    different specialties (default is all specialities).",
      )
    },
    specialty_summary = function(...) {
      self[["plot_data"]](type = "specialty_line", data_type = "annual", ...) |>
        pivot_wider() |>
        rename(
          "Financial Year" = "FinancialYear",
          "Specialty" = "SpecialtyName",
          "Percentage occupancy" = "PercentageOccupancy",
        ) |>
        select(-"ID", -"HospitalName")
    },
    specialty_summary_info = function() {
      "This summary table presents the annual number of available staffed beds,
        the number of annual beds occupied, and the percentage occupancy for
        the selected hospital"
    }
  ),
  public = list(
    #' @description
    #' Get character vector of plot types for hospital grp
    plot_types = function() {
      c("specialty_line")
    },
    #' @description
    #' Plot hospital unit.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `plot_types` for options.
    #' @param ... Passed to plot functions.
    #' @examples
    #' x <- example_hospital_unit()
    #' x[["plot"]](type = "specialty_line", data_type = "annual")
    plot = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "specialty_line" = private[["specialty_line"]]
      )(...)
    },
    #' @description
    #' Generate plot data for hospital unit.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `plot_types` for options.
    #' @param ... Passed to plot data functions.
    #' @examples
    #' x <- example_hospital_unit()
    #' x[["plot_data"]](type = "specialty_line", data_type = "annual")
    plot_data = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "specialty_line" = private[["specialty_line_data"]]
      )(...)
    },
    #' @description
    #' Get plot info for hospital unit.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `plot_types` for options.
    #' @param ... Passed to plot info functions.
    #' @examples
    #' x <- example_hospital_unit()
    #' x[["plot_info"]](type = "specialty_line", data_type = "annual")
    plot_info = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "specialty_line" = private[["specialty_line_info"]]
      )(...)
    },
    #' @description
    #' Get character vector of summary types for hospital unit.
    summary_types = function() {
      "specialty_summary"
    },
    #' @description
    #' Summarise hospital data.
    #' @param type (character(1))\cr
    #'   Character specifying summary type. See `summary_types` for options.
    #' @param ... Passed to method.
    #' @examples
    #' x <- example_hospital_unit()
    #' x[["summary"]](type = "specialty_summary")
    summary = function(type, ...) {
      type <- arg_match(type, values = self[["summary_types"]]())
      switch(type,
        "specialty_summary" = private[["specialty_summary"]]
      )(...)
    },
    #' @description
    #' Get summary info for hospital unit.
    #' @param type (character(1))\cr
    #'   Character specifying summary type. See `summary_types` for options.
    #' @param ... Passed to summary info functions.
    #' @examples
    #' x <- example_hospital_unit()
    #' x[["summary_info"]](type = "specialty_summary")
    summary_info = function(type, ...) {
      type <- arg_match(type, values = self[["summary_types"]]())
      switch(type,
        "specialty_summary" = private[["specialty_summary_info"]]
      )(...)
    },
    #' @description
    #' Create UI for hospital object.
    #' @param ns
    #'     Namespace of shiny application page.
    ui = function(ns) {
      ns <- NS(ns(self[["ID"]]()))
      navset_tab(
        nav_panel(
          title = "Visualisation",
          card(
            card_header(paste(self[["title"]](), "-", self[["ID"]]())),
            div(glue("Address: {self[['address']]()}")),
            div(glue("Health Board: {self[['health_board']]()}")),
            card(
              full_screen = TRUE,
              card_header(
                "Annually Available Staffed Beds",
                help_popover(
                  id = ns("annual_beds_help"),
                  self[["plot_info"]]("specialty_line", "annual")
                ),
                settings_popover(
                  id = ns("annual_beds_settings"),
                  virtual_select_input(
                    ns("specialty_annual_select"),
                    label = "Select specialty",
                    choices = private[["specialty_choices"]](),
                    multiple = TRUE,
                    selected = "All Specialties"
                  )
                )
              ),
              withSpinner(uiOutput(ns("annual_beds")))
            ),
            card(
              full_screen = TRUE,
              card_header(
                "Daily Average Available Staffed Beds",
                help_popover(
                  id = ns("daily_beds_help"),
                  self[["plot_info"]]("specialty_line", "daily")
                ),
                settings_popover(
                  id = ns("daily_beds_settings"),
                  virtual_select_input(
                    ns("specialty_daily_select"),
                    label = "Select specialty",
                    choices = private[["specialty_choices"]](),
                    multiple = TRUE,
                    selected = "All Specialties"
                  )
                )
              ),
              withSpinner(uiOutput(ns("daily_beds")))
            ),
            card(downloadButton(ns("download"), "Download all statistics"))
          )
        ),
        nav_panel(
          title = "Summary",
          card(
            full_screen = TRUE,
            card_header(
              help_popover(
                id = ns("dt_help"),
                self[["summary_info"]]("specialty_summary")
              )
            ),
            withSpinner(DTOutput(ns("summary")))
          )
        )
      )
    },
    #' @description
    #' Create server for hospital object.
    server = function() {
      moduleServer(
        self[["ID"]](),
        function(input, output, session) {
          output[["annual_beds"]] <- renderUI({
            log_info("Creating hospital annual specialty line plot")
            self[["plot"]](
              type = "specialty_line",
              data_type = "annual",
              specialties = req(input[["specialty_annual_select"]])
            )
          })
          output[["daily_beds"]] <- renderUI({
            log_info("Creating hospital daily specialty line plot")
            self[["plot"]](
              type = "specialty_line",
              data_type = "daily",
              specialties = req(input[["specialty_daily_select"]])
            )
          })
          output[["download"]] <- downloadHandler(
            filename = function() "hospital_data.csv",
            content = function(con) {
              write.csv(self[["data"]](), con)
            }
          )
          output[["summary"]] <- renderDT({
            log_info("Creating hospital specialty summary table")
            self[["datatable"]]("specialty_summary")
          })
        }
      )
    }
  )
)

#' Get example hospital health unit object.
#' @param id Character ID of Hospital to get. Default is "A101H".
#' @examples
#' example_hospital_unit()
#' @export
example_hospital_unit <- function(id = "A101H") {
  meta <- HealthDataScotland::example_hospital_metadata |>
    rename("ID" = "HospitalCode", "HBName" = "HealthBoard") |>
    filter(.data[["ID"]] == id)
  data <- HealthDataScotland::example_hospital_data |>
    rename("ID" = "Location") |>
    filter(.data[["ID"]] == id)
  hospital[["new"]](meta, data)
}
