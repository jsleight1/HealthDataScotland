#' R6 class storing health statistics for a list of hospital health units.
#'
#' This R6 class is designed to store bed occupancy statistics for multiple
#' hospitals. This class can be used to plot summary statistics and
#' create shiny UI/server objects.
#'
#' @examples
#' gps <- lapply(c("A101H", "A201H"), example_hospital_unit)
#' x <- hospital_grp[["new"]](gps, .id = "gp")
#' x[["ID"]]()
#' x[["IDs"]]()
#' x[["titles"]]()
#' x[["metadata"]]()
#' x[["data"]]()
#' x[["subset"]](id = "A201H")
#' x[["plot"]](type = "hospital_bar")
#' x[["plot_data"]](type = "hospital_bar")
#' x[["plot_info"]](type = "hospital_bar")
#' \dontrun{
#' x[["ui"]]()
#' x[["server"]]()
#' }
#' @export
hospital_grp <- R6Class("hospital_grp",
  inherit = health_unitgrp,
  private = list(
    specialty_choices = function() {
      self[["data"]]() |>
        map(~ .x[["data"]]()) |>
        map(pull, "SpecialtyName") |>
        unlist() |>
        unique() |>
        sort()
    },
    national_trend = function(...) {
      self[["plot_data"]](type = "national_trend", ...) |>
        private[["trend_echart"]]()
    },
    national_trend_data = function(specialties = "All Specialties") {
      specialties <- arg_match(
        specialties,
        values = private[["specialty_choices"]](),
        multiple = TRUE
      )
      self[["combine_data"]]() |>
        filter(.data[["SpecialtyName"]] %in% specialties) |>
        private[["trend_data"]](groups = c("FinancialYear", "SpecialtyName")) |>
        group_by(.data[["SpecialtyName"]])
    },
    health_board_trend = function(...) {
      data <- self[["plot_data"]](type = "health_board_trend", ...)
      data |>
        private[["trend_echart"]]() |>
        e_title(unique(data[["SpecialtyName"]]), left = "center")
    },
    health_board_bar = function(...) {
      self[["plot_data"]](type = "health_board_bar", ...) |>
        private[["bar_echart"]]() |>
        e_x_axis(axisLabel = list(rotate = 35))
    },
    health_board_trend_data = function(specialties = "All Specialties",
                                       health_boards = private[["health_board_choices"]]()) {
      specialties <- arg_match(
        specialties,
        values = private[["specialty_choices"]]()
      )
      health_boards <- arg_match(health_boards, multiple = TRUE)
      private[["health_board_data"]](specialties, health_boards) |>
        private[["trend_data"]](
          groups = c("FinancialYear", "HBName", "SpecialtyName")
        ) |>
        group_by(.data[["HBName"]])
    },
    health_board_bar_data = function(specialties = "All Specialties",
                                     health_boards = private[["health_board_choices"]]()) {
      specialties <- arg_match(
        specialties,
        values = private[["specialty_choices"]](),
        multiple = TRUE
      )
      health_boards <- arg_match(health_boards, multiple = TRUE)
      private[["health_board_data"]](specialties, health_boards) |>
        private[["bar_data"]](
          groups = c("FinancialYear", "HBName", "SpecialtyName")
        )
    },
    health_board_data = function(specialties, health_boards) {
      self[["combine_data"]]() |>
        filter(.data[["SpecialtyName"]] %in% specialties) |>
        filter(.data[["HBName"]] %in% health_boards) |>
        distinct()
    },
    hospital_trend = function(...) {
      data <- self[["plot_data"]](type = "hospital_trend", ...)
      data |>
        private[["trend_echart"]]() |>
        e_title(unique(data[["SpecialtyName"]]), left = "center")
    },
    hospital_bar = function(...) {
      self[["plot_data"]](type = "hospital_bar", ...) |>
        private[["bar_echart"]]("ID", "Hospital") |>
        e_x_axis(axisLabel = list(rotate = 15))
    },
    hospital_trend_data = function(specialties = "All Specialties",
                                   hospitals = private[["unit_choices"]]()) {
      specialties <- arg_match(
        specialties,
        values = private[["specialty_choices"]]()
      )
      hospitals <- arg_match(hospitals, multiple = TRUE)
      private[["hospital_data"]](specialties, hospitals) |>
        private[["trend_data"]]() |>
        group_by(.data[["ID"]])
    },
    hospital_bar_data = function(specialties = private[["specialty_choices"]](),
                                 hospitals = private[["unit_choices"]]()) {
      specialties <- arg_match(specialties, multiple = TRUE)
      hospitals <- arg_match(hospitals, multiple = TRUE)
      private[["hospital_data"]](specialties, hospitals) |>
        private[["bar_data"]]()
    },
    hospital_data = function(specialties, hospitals) {
      self[["combine_data"]]() |>
        mutate(ID = paste(.data[["ID"]], "-", .data[["HospitalName"]])) |>
        filter(
          .data[["SpecialtyName"]] %in% specialties,
          .data[["ID"]] %in% hospitals
        )
    },
    trend_echart = function(x) {
      super$trend_echart(x, "FinancialYear", "PercentageOccupancy") |>
        e_y_axis(min = 0, max = 100)
    },
    bar_echart = function(x,
                          group = "HBName",
                          x_axis = "",
                          y_axis = "PercentageOccupancy") {
      super$bar_echart(x, group, x_axis, y_axis) |>
        e_y_axis(min = 0, max = 100)
    },
    trend_data = function(x, ...) {
      x |>
        select(all_of(private[["req_plot_cols"]]())) |>
        distinct() |>
        private[["summarise_percentage_occupancy"]](...)
    },
    bar_data = function(x, ...) {
      x |>
        private[["trend_data"]](...) |>
        pivot_wider(
          names_from = "SpecialtyName",
          values_from = "PercentageOccupancy"
        ) |>
        select(-where(~ all(is.na(.)))) |>
        group_by(.data[["FinancialYear"]])
    },
    req_plot_cols = function() {
      c(
        "FinancialYear",
        "ID",
        "HBName",
        "SpecialtyName",
        "PercentageOccupancy"
      )
    },
    summarise_percentage_occupancy = function(x, groups = colnames(x)) {
      # Remove 'PercentageOccupancyQF' == "z" as these are not applicable
      # See https://www.opendata.nhs.scot/dataset/statistical-qualifiers/resource/b80f9af0-b115-4245-b591-fb22775226c4
      x |>
        filter(.data[["PercentageOccupancy"]] != "z") |>
        group_by_at(groups) |>
        summarise(
          PercentageOccupancy = mean(
            .data[["PercentageOccupancy"]],
            na.rm = TRUE
          )
        )
    },
    national_trend_info = function() {
      "This line chart shows the national average hospital bed
        occupancy (y-axis) across time (x-axis) for the selected
        specialty. Specifically, this plot combines data for all available
        hospital locations across Scotland and calculates the average
        percentage bed occupancy for the selected specialty. Settings
        can be used to show data for different specialties (default is
        all specialities)."
    },
    health_board_trend_info = function() {
      "This line chart shows the average hospital bed occupancy (y-axis)
        across time (x-axis) per health board (colour) for the selected
        specialty. Specifically, this plot combines data for all
        available hospital locations for the selected health boards
        and specialty, and calculates the average percentage bed occupancy.
        Settings can be used to show data for different
        specialties (default is all specialties) and health boards."
    },
    health_board_bar_info = function() {
      "This bar chart shows the average hospital bed occupancy (y-axis)
        across health boards (x-axis) for each specialty (colour).
        Specifically, this plot combines data for all
        available hospital locations for the selected health boards and
        speciality, and calculates the average percentage bed occupancy.
        Settings can be used to show data for different
        specialties (default is all specialties) and health boards."
    },
    hospital_trend_info = function() {
      "This line chart shows the average hospital bed occupancy (y-axis)
        across time (x-axis) per hospital (colour) for the selected
        specialty. Settings can be used to show data for different
        specialties (default is all specialties) and hospitals."
    },
    hospital_bar_info = function() {
      "This bar chart shows the average hospital bed occupancy (y-axis)
        per hospital (x-axis) for each specialty (colour). Seetings can be
        used to show data for different specialties (default is all
        specialties) and hospitals."
    },
    lookup = function(...) {
      data.frame(
        Title = self[["titles"]](),
        ID = self[["IDs"]](),
        Address = self[["addresses"]](),
        `Health board` = self[["health_boards"]](),
        check.names = FALSE,
        ...
      )
    },
    lookup_info = function(...) {
      "This lookup table presents data for all available hospitals
      in the data set. This table can be searched, filtered and
      the 'Plot' column allows the user to view statistics for a
      selected hospital."
    }
  ),
  public = list(
    #' @description
    #' Get character vector of plot types for hospital grp.
    plot_types = function() {
      c(
        "national_trend", "health_board_trend", "health_board_bar",
        "hospital_trend", "hospital_bar"
      )
    },
    #' @description
    #' Plot hospital grp.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `plot_types` for options.
    #' @param ... Passed to plot functions.
    #' @examples
    #' x <- example_hospital_grp_unit()
    #' x[["plot"]](type = "hospital_bar")
    plot = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "national_trend" = private[["national_trend"]],
        "health_board_trend" = private[["health_board_trend"]],
        "health_board_bar" = private[["health_board_bar"]],
        "hospital_trend" = private[["hospital_trend"]],
        "hospital_bar" = private[["hospital_bar"]]
      )(...)
    },
    #' @description
    #' Generate plot data for hospital grp.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `plot_types` for options.
    #' @param ... Passed to plot data functions.
    #' @examples
    #' x <- example_hospital_grp_unit()
    #' x[["plot_data"]](type = "hospital_bar")
    plot_data = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "national_trend" = private[["national_trend_data"]],
        "health_board_trend" = private[["health_board_trend_data"]],
        "health_board_bar" = private[["health_board_bar_data"]],
        "hospital_trend" = private[["hospital_trend_data"]],
        "hospital_bar" = private[["hospital_bar_data"]]
      )(...)
    },
    #' @description
    #' Get plot info for hospital grp.
    #' @param type (character(1))\cr
    #'   Character specifying plot type. See `plot_types` for options.
    #' @param ... Passed to plot info functions.
    #' @examples
    #' x <- example_hospital_grp_unit()
    #' x[["plot_info"]](type = "hospital_bar")
    plot_info = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "national_trend" = private[["national_trend_info"]],
        "health_board_trend" = private[["health_board_trend_info"]],
        "health_board_bar" = private[["health_board_bar_info"]],
        "hospital_trend" = private[["hospital_trend_info"]],
        "hospital_bar" = private[["hospital_bar_info"]]
      )(...)
    },
    #' @description
    #' Create UI for hospital group object.
    #' @param ... Passed to functions.
    ui = function(...) {
      ns <- NS(self[["ID"]]())
      nav_panel(
        title = "Hospital",
        class = "overflow-auto",
        navset_tab(
          nav_panel(
            title = "Visualisation",
            div(
              card(
                full_screen = TRUE,
                card_header(
                  "National average bed occupancy per specialty",
                  help_popover(
                    id = ns("national_help"),
                    self[["plot_info"]]("national_trend")
                  ),
                  settings_popover(
                    id = ns("national_settings"),
                    virtual_select_input(
                      ns("select_national_specialty"),
                      label = "Select specialty",
                      choices = private[["specialty_choices"]](),
                      multiple = TRUE,
                      selected = "All Specialties"
                    )
                  )
                ),
                e_output_spinner(ns("national_trend"))
              ),
              card(
                full_screen = TRUE,
                card_header("Health board summary"),
                layout_column_wrap(
                  card(
                    full_screen = TRUE,
                    card_header(
                      "Average bed occupancy per health board for selected specialty",
                      help_popover(
                        id = ns("hb_trend_help"),
                        self[["plot_info"]]("health_board_trend")
                      ),
                      settings_popover(
                        id = ns("hb_trend_settings"),
                        virtual_select_input(
                          ns("select_hb_trend_specialty"),
                          label = "Select specialty",
                          choices = private[["specialty_choices"]](),
                          selected = "All Specialties"
                        ),
                        virtual_select_input(
                          inputId = ns("select_hb_trend_hb"),
                          label = "Select health boards",
                          multiple = TRUE,
                          choices = private[["health_board_choices"]](),
                          selected = private[["health_board_choices"]]()
                        )
                      )
                    ),
                    e_output_spinner(ns("hb_trend"))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header(
                      "Average bed occupancy per health board and specialty",
                      help_popover(
                        id = ns("hb_bar_help"),
                        self[["plot_info"]]("health_board_bar")
                      ),
                      settings_popover(
                        id = ns("hb_bar_settings"),
                        virtual_select_input(
                          ns("select_hb_bar_specialty"),
                          label = "Select specialty",
                          choices = private[["specialty_choices"]](),
                          multiple = TRUE,
                          selected = "All Specialties"
                        ),
                        virtual_select_input(
                          inputId = ns("select_hb_bar_hb"),
                          label = "Select health boards",
                          multiple = TRUE,
                          choices = private[["health_board_choices"]](),
                          selected = private[["health_board_choices"]]()
                        )
                      )
                    ),
                    e_output_spinner(ns("hb_bar"))
                  )
                )
              ),
              card(
                full_screen = TRUE,
                card_header("Individual hospital summary"),
                layout_column_wrap(
                  card(
                    full_screen = TRUE,
                    card_header(
                      "Average bed occupancy per hospital for selected specialty",
                      help_popover(
                        id = ns("hosp_trend_help"),
                        self[["plot_info"]]("hospital_trend")
                      ),
                      settings_popover(
                        id = ns("hosp_trend_settings"),
                        virtual_select_input(
                          ns("select_hosp_trend_specialty"),
                          label = "Select specialty",
                          choices = private[["specialty_choices"]](),
                          selected = "All Specialties"
                        ),
                        virtual_select_input(
                          ns("select_hosp_trend_hosp"),
                          label = "Select hospital",
                          choices = private[["unit_choices"]](),
                          selected = private[["unit_choices"]]()[1],
                          multiple = TRUE
                        )
                      )
                    ),
                    e_output_spinner(ns("hosp_trend"))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header(
                      "Average bed occupancy per hospital per specialty",
                      help_popover(
                        id = ns("hosp_bar_help"),
                        self[["plot_info"]]("hospital_bar")
                      ),
                      settings_popover(
                        id = ns("hosp_bar_settings"),
                        virtual_select_input(
                          ns("select_hosp_bar_specialty"),
                          label = "Select specialty",
                          choices = private[["specialty_choices"]](),
                          selected = "All Specialties",
                          multiple = TRUE
                        ),
                        virtual_select_input(
                          ns("select_hosp_bar_hosp"),
                          label = "Select hospital",
                          choices = private[["unit_choices"]](),
                          selected = private[["unit_choices"]]()[1],
                          multiple = TRUE
                        )
                      )
                    ),
                    e_output_spinner(ns("hosp_bar"))
                  )
                )
              ),
              card(downloadButton(ns("download"), "Download all statistics"))
            )
          ),
          nav_panel(
            title = "Centre lookup",
            card(
              full_screen = TRUE,
              card_header(
                help_popover(
                  id = ns("dt_help"),
                  self[["summary_info"]]("lookup")
                )
              ),
              withSpinner(DTOutput(ns("summary")))
            )
          )
        )
      )
    },
    #' @description
    #' Create server for hospital group object.
    server = function() {
      moduleServer(
        self[["ID"]](),
        function(input, output, session) {
          ns <- session[["ns"]]
          output[["national_trend"]] <- renderEcharts4r({
            log_info("Creating hospital national trend plot")
            self[["plot"]](
              type = "national_trend",
              specialties = req(input[["select_national_specialty"]])
            )
          })
          output[["hb_trend"]] <- renderEcharts4r({
            log_info("Creating hospital health board trend plot")
            self[["plot"]](
              type = "health_board_trend",
              specialties = req(input[["select_hb_trend_specialty"]]),
              health_boards = req(input[["select_hb_trend_hb"]])
            )
          })
          output[["hb_bar"]] <- renderEcharts4r({
            log_info("Creating hospital health board bar plot")
            self[["plot"]](
              type = "health_board_bar",
              specialties = req(input[["select_hb_bar_specialty"]]),
              health_boards = req(input[["select_hb_bar_hb"]])
            )
          })
          output[["hosp_trend"]] <- renderEcharts4r({
            log_info("Creating hospital trend plot")
            self[["plot"]](
              type = "hospital_trend",
              specialties = req(input[["select_hosp_trend_specialty"]]),
              hospitals = req(input[["select_hosp_trend_hosp"]])
            )
          })
          output[["hosp_bar"]] <- renderEcharts4r({
            log_info("Creating hospital bar plot")
            self[["plot"]](
              type = "hospital_bar",
              specialties = req(input[["select_hosp_bar_specialty"]]),
              hospitals = req(input[["select_hosp_bar_hosp"]])
            )
          })
          output[["download"]] <- self[["download_handler"]]()
          output[["summary"]] <- renderDT(self[["datatable"]]("lookup", ns))
          observe({
            log_info("Rendering hospital unit popup")
            obj <- self[["health_unit"]](input[["dt_button"]])
            isolate(obj[["popup_modal"]](ns))
          }) |>
            bindEvent(input[["dt_button"]])
        }
      )
    }
  )
)

#' Get example gp health unit grp object.
#' @param ids Character ID of Hospital to get. Default is c("A101H", "A201H").
#' @export
example_hospital_grp_unit <- function(ids = c("A101H", "A201H")) {
  hospitals <- map(ids, example_hospital_unit)
  hospital_grp[["new"]](hospitals, .id = "hospital")
}
