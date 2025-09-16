#' R6 class storing health statistics for a Hospital.
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
    specialty_data = function(specialties = "All Specialties") {
      self[["combine_data"]]() |>
        filter(.data[["SpecialtyName"]] %in% specialties)
    },
    specialty_line_data = function(data_type = c("annual", "daily"),
                                   ...) {
      cols <- switch(arg_match(data_type),
        "annual" = private[["annual_cols"]],
        "daily" = private[["daily_cols"]]
      )()
      private[["specialty_data"]](...) |>
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
    }
  ),
  public = list(
    #' @description
    #' Get character vector of available plots for hospital grp
    available_plots = function() {
      c("specialty_line")
    },
    #' @description
    #' Plot hospital unit.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `available_plots` for options.
    #' @param ... Passed to plot functions.
    plot = function(type, ...) {
      type <- arg_match(type, values = self[["available_plots"]]())
      switch(type,
        "specialty_line" = private[["specialty_line"]]
      )(...)
    },
    #' @description
    #' Generate plot data for hospital unit.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `available_plots` for options.
    #' @param ... Passed to plot data functions.
    plot_data = function(type, ...) {
      type <- arg_match(type, values = self[["available_plots"]]())
      switch(type,
        "specialty_line" = private[["specialty_line_data"]]
      )(...)
    },
    #' @description
    #' Get plot info for hospital unit.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `available_plots` for options.
    #' @param ... Passed to plot info functions.
    plot_info = function(type, ...) {
      type <- arg_match(type, values = self[["available_plots"]]())
      switch(type,
        "specialty_line" = private[["specialty_line_info"]]
      )(...)
    },
    #' @description
    #' Create UI for hospital object.
    #' @param ns
    #'     Namespace of shiny application page.
    ui = function(ns) {
      ns <- NS(ns(self[["ID"]]()))
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
        card(downloadButton(ns("download")))
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
        }
      )
    }
  )
)

#' Get example hospital health unit object.
#' @param id Character ID of Hospital to get. Default is "A101H".
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
