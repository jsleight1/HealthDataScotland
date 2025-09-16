#' R6 class storing health statistics for a list of hospital health units.
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
    summarise_percentage_occupancy = function(x, groups) {
      # TODO: need to check 'PercentageOccupancyQF' column
      # Might need to remove 'PercentageOccupancyQF' == "z"
      x |>
        group_by_at(groups) |>
        summarise(
          PercentageOccupancy = mean(
            .data[["PercentageOccupancy"]],
            na.rm = TRUE
          )
        )
    },
    national_trend_data = function(specialties = "All Specialties") {
      specialties <- arg_match(
        specialties,
        values = private[["specialty_choices"]](),
        multiple = TRUE
      )
      private[["map_combine"]]("data") |>
        filter(.data[["SpecialtyName"]] %in% specialties) |>
        select("FinancialYear", "SpecialtyName", "PercentageOccupancy") |>
        distinct() |>
        private[["summarise_percentage_occupancy"]](
          c("FinancialYear", "SpecialtyName")
        ) |>
        group_by(.data[["SpecialtyName"]])
    },
    national_trend = function(...) {
      self[["plot_data"]](type = "national_trend", ...) |>
        private[["trend_echart"]]()
    },
    health_board_trend_data = function(specialties = "All Specialties",
                                       health_boards = private[["health_board_choices"]]()) {
      specialties <- arg_match(
        specialties,
        values = private[["specialty_choices"]]()
      )
      health_boards <- arg_match(health_boards, multiple = TRUE)
      private[["health_board_data"]](specialties, health_boards) |>
        group_by(.data[["HBName"]])
    },
    health_board_trend = function(...) {
      data <- self[["plot_data"]](type = "health_board_trend", ...)
      data |>
        private[["trend_echart"]]() |>
        e_title(unique(data[["SpecialtyName"]]), left = "center")
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
        pivot_wider(
          names_from = "SpecialtyName",
          values_from = "PercentageOccupancy"
        ) |>
        group_by(.data[["FinancialYear"]])
    },
    health_board_data = function(specialties, health_boards) {
      self[["combine_data"]]() |>
        filter(.data[["SpecialtyName"]] %in% specialties) |>
        filter(.data[["HBName"]] %in% health_boards) |>
        distinct() |>
        private[["summarise_percentage_occupancy"]](
          c("FinancialYear", "HBName", "SpecialtyName")
        )
    },
    health_board_bar = function(...) {
      self[["plot_data"]](type = "health_board_bar", ...) |>
        private[["bar_echart"]]() |>
        e_x_axis(axisLabel = list(rotate = 35))
    },
    hospital_trend_data = function(specialties = "All Specialties",
                                   hospitals = private[["unit_choices"]]()) {
      specialties <- arg_match(
        specialties,
        values = private[["specialty_choices"]]()
      )
      hospitals <- arg_match(hospitals, multiple = TRUE)
      private[["hospital_data"]](specialties, hospitals) |>
        select(
          "FinancialYear",
          "ID",
          "SpecialtyName",
          "PercentageOccupancy"
        ) |>
        group_by(.data[["ID"]])
    },
    hospital_trend = function(...) {
      data <- self[["plot_data"]](type = "hospital_trend", ...)
      data |>
        private[["trend_echart"]]() |>
        e_title(unique(data[["SpecialtyName"]]), left = "center")
    },
    hospital_bar_data = function(specialties = private[["specialty_choices"]](),
                                 hospitals = private[["unit_choices"]]()) {
      specialties <- arg_match(specialties, multiple = TRUE)
      hospitals <- arg_match(hospitals, multiple = TRUE)
      private[["hospital_data"]](specialties, hospitals) |>
        select(
          "FinancialYear",
          "ID",
          "SpecialtyName",
          "PercentageOccupancy"
        ) |>
        pivot_wider(
          names_from = "SpecialtyName",
          values_from = "PercentageOccupancy"
        ) |>
        group_by(.data[["FinancialYear"]])
    },
    hospital_data = function(specialties, hospitals) {
      self[["combine_data"]]() |>
        mutate(ID = paste(.data[["ID"]], "-", .data[["HospitalName"]])) |>
        filter(
          .data[["SpecialtyName"]] %in% specialties,
          .data[["ID"]] %in% hospitals
        )
    },
    hospital_bar = function(...) {
      x <- self[["plot_data"]](type = "hospital_bar", ...) |>
        private[["bar_echart"]]("ID", "Hospital")
    },
    national_trend_info = function() {
      "This line chart shows the national average hospital bed
            occupancy (y-axis) across time (x-axis) for the selected
            specialty. Settings can be used to show data for different
            specialties (default is all specialities)."
    },
    health_board_trend_info = function() {
      "This line chart shows the average hospital bed occupancy (y-axis)
            across time (x-axis) per health board (colour) for the selected
            specialty. Settings can be used to show data for different
            specialties (default is all specialties) and health boards."
    },
    health_board_bar_info = function() {
      "This bar chart shows the average hospital bed occupancy (y-axis)
            across health boards (x-axis) for each specialty (colour).
            Settings can be used to show data for different specialties
            (default is all specialties) and health boards."
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
    }
  ),
  public = list(
    #' @description
    #' Get character vector of available plots for hospital grp.
    available_plots = function() {
      c(
        "national_trend", "health_board_trend", "health_board_bar",
        "hospital_trend", "hospital_bar"
      )
    },
    #' @description
    #' Plot hospital grp.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `available_plots` for options.
    #' @param ... Passed to plot functions.
    plot = function(type, ...) {
      type <- arg_match(type, values = self[["available_plots"]]())
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
    #'     Character specifying plot type. See `available_plots` for options.
    #' @param ... Passed to plot data functions.
    plot_data = function(type, ...) {
      type <- arg_match(type, values = self[["available_plots"]]())
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
    #'     Character specifying plot type. See `available_plots`
    #'   for options.
    #' @param ... Passed to plot info functions.
    plot_info = function(type, ...) {
      type <- arg_match(type, values = self[["available_plots"]]())
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
    #' @param ns
    #'     Namespace of shiny application page.
    ui = function(ns) {
      ns <- NS(self[["id"]]())
      nav_panel(
        title = "Hospital",
        class = "overflow-auto",
        div(
          card(
            full_screen = TRUE,
            card_header(
              "National average bed occupancy per specialty",
              popover(
                id = ns("national_help"),
                bs_icon("question-circle"),
                self[["plot_info"]]("national_trend")
              ),
              popover(
                id = ns("national_settings"),
                bs_icon("gear", class = "ms-auto"),
                virtualSelectInput(
                  ns("select_national_specialty"),
                  label = "Select specialty",
                  choices = private[["specialty_choices"]](),
                  multiple = TRUE,
                  selected = "All Specialties",
                  search = TRUE,
                  html = TRUE,
                  showSelectedOptionsFirst = TRUE,
                  updateOn = "close"
                )
              )
            ),
            echarts4rOutput(outputId = ns("national_trend"))
          ),
          card(
            full_screen = TRUE,
            card_header("Health board summary"),
            layout_column_wrap(
              card(
                full_screen = TRUE,
                card_header(
                  "Average bed occupancy per health board for selected specialty",
                  popover(
                    id = ns("hb_trend_help"),
                    bs_icon("question-circle"),
                    self[["plot_info"]]("health_board_trend")
                  ),
                  popover(
                    id = ns("hb_trend_settings"),
                    bs_icon("gear", class = "ms-auto"),
                    virtualSelectInput(
                      ns("select_hb_trend_specialty"),
                      label = "Select specialty",
                      choices = private[["specialty_choices"]](),
                      selected = "All Specialties",
                      search = TRUE,
                      html = TRUE,
                      showSelectedOptionsFirst = TRUE,
                      updateOn = "close"
                    ),
                    virtualSelectInput(
                      inputId = ns("select_hb_trend_hb"),
                      label = "Select health boards",
                      multiple = TRUE,
                      choices = private[["health_board_choices"]](),
                      selected = private[["health_board_choices"]](),
                      search = TRUE,
                      html = TRUE,
                      showSelectedOptionsFirst = TRUE,
                      updateOn = "close"
                    )
                  )
                ),
                echarts4rOutput(outputId = ns("hb_trend"))
              ),
              card(
                full_screen = TRUE,
                card_header(
                  "Average bed occupancy per health board and specialty",
                  popover(
                    id = ns("hb_bar_help"),
                    bs_icon("question-circle"),
                    self[["plot_info"]]("health_board_bar")
                  ),
                  popover(
                    id = ns("hb_bar_settings"),
                    bs_icon("gear", class = "ms-auto"),
                    virtualSelectInput(
                      ns("select_hb_bar_specialty"),
                      label = "Select specialty",
                      choices = private[["specialty_choices"]](),
                      multiple = TRUE,
                      selected = "All Specialties",
                      search = TRUE,
                      html = TRUE,
                      showSelectedOptionsFirst = TRUE,
                      updateOn = "close"
                    ),
                    virtualSelectInput(
                      inputId = ns("select_hb_bar_hb"),
                      label = "Select health boards",
                      multiple = TRUE,
                      choices = private[["health_board_choices"]](),
                      selected = private[["health_board_choices"]](),
                      search = TRUE,
                      html = TRUE,
                      showSelectedOptionsFirst = TRUE,
                      updateOn = "close"
                    )
                  )
                ),
                echarts4rOutput(outputId = ns("hb_bar"))
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
                  popover(
                    id = ns("hosp_trend_help"),
                    bs_icon("question-circle"),
                    self[["plot_info"]]("hospital_trend")
                  ),
                  popover(
                    id = ns("hosp_trend_settings"),
                    bs_icon("gear", class = "ms-auto"),
                    virtualSelectInput(
                      ns("select_hosp_trend_specialty"),
                      label = "Select specialty",
                      choices = private[["specialty_choices"]](),
                      selected = "All Specialties",
                      search = TRUE,
                      html = TRUE,
                      showSelectedOptionsFirst = TRUE,
                      updateOn = "close"
                    ),
                    virtualSelectInput(
                      ns("select_hosp_trend_hosp"),
                      label = "Select hospital",
                      choices = private[["unit_choices"]](),
                      selected = private[["unit_choices"]]()[1],
                      multiple = TRUE,
                      search = TRUE,
                      html = TRUE,
                      showSelectedOptionsFirst = TRUE,
                      updateOn = "close"
                    )
                  )
                ),
                echarts4rOutput(outputId = ns("hosp_trend"))
              ),
              card(
                full_screen = TRUE,
                card_header(
                  "Average bed occupancy per hospital per specialty",
                  popover(
                    id = ns("hosp_bar_help"),
                    bs_icon("question-circle"),
                    self[["plot_info"]]("hospital_bar")
                  ),
                  popover(
                    id = ns("hosp_bar_settings"),
                    bs_icon("gear", class = "ms-auto"),
                    virtualSelectInput(
                      ns("select_hosp_bar_specialty"),
                      label = "Select specialty",
                      choices = private[["specialty_choices"]](),
                      selected = "All Specialties",
                      multiple = TRUE,
                      search = TRUE,
                      html = TRUE,
                      showSelectedOptionsFirst = TRUE,
                      updateOn = "close"
                    ),
                    virtualSelectInput(
                      ns("select_hosp_bar_hosp"),
                      label = "Select hospital",
                      choices = private[["unit_choices"]](),
                      selected = private[["unit_choices"]]()[1],
                      multiple = TRUE,
                      search = TRUE,
                      html = TRUE,
                      showSelectedOptionsFirst = TRUE,
                      updateOn = "close"
                    )
                  )
                ),
                echarts4rOutput(outputId = ns("hosp_bar"))
              )
            )
          ),
          card(downloadButton(ns("download")))
        )
      )
    },
    #' @description
    #' Create server for hospital group object.
    server = function() {
      moduleServer(
        self[["id"]](),
        function(input, output, session) {
          ns <- session[["ns"]]

          national_trend_plt <- reactive({
            log_info("Creating hospital national trend plot")
            self[["plot"]](
              type = "national_trend",
              specialties = input[["select_national_specialty"]]
            )
          }) |>
            bindEvent(input[["select_national_specialty"]])

          output[["national_trend"]] <- renderEcharts4r(national_trend_plt())

          hb_trend_plt <- reactive({
            log_info("Creating hospital health board trend plot")
            self[["plot"]](
              type = "health_board_trend",
              specialties = input[["select_hb_trend_specialty"]],
              health_boards = input[["select_hb_trend_hb"]]
            )
          }) |>
            bindEvent(
              input[["select_hb_trend_specialty"]],
              input[["select_hb_trend_hb"]]
            )

          output[["hb_trend"]] <- renderEcharts4r(hb_trend_plt())

          hb_bar_plt <- reactive({
            log_info("Creating hospital health board bar plot")
            self[["plot"]](
              type = "health_board_bar",
              specialties = input[["select_hb_bar_specialty"]],
              health_boards = input[["select_hb_bar_hb"]]
            )
          }) |>
            bindEvent(
              input[["select_hb_bar_specialty"]],
              input[["select_hb_bar_hb"]]
            )

          output[["hb_bar"]] <- renderEcharts4r(hb_bar_plt())

          hosp_trend_plt <- reactive({
            log_info("Creating hospital trend plot")
            self[["plot"]](
              type = "hospital_trend",
              specialties = input[["select_hosp_trend_specialty"]],
              hospitals = input[["select_hosp_trend_hosp"]]
            )
          }) |>
            bindEvent(
              input[["select_hosp_trend_specialty"]],
              input[["select_hosp_trend_hosp"]]
            )

          output[["hosp_trend"]] <- renderEcharts4r(hosp_trend_plt())

          hosp_bar_plt <- reactive({
            log_info("Creating hospital bar plot")
            self[["plot"]](
              type = "hospital_bar",
              specialties = input[["select_hosp_bar_specialty"]],
              hospitals = input[["select_hosp_bar_hosp"]]
            )
          }) |>
            bindEvent(
              input[["select_hosp_bar_specialty"]],
              input[["select_hosp_bar_hosp"]]
            )

          output[["hosp_bar"]] <- renderEcharts4r(hosp_bar_plt())

          output[["download"]] <- self[["download_handler"]]()
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
  sf <- get_sf("hospital")
  sf <- sf[sf[["ID"]] %in% ids, ]
  hospital_grp[["new"]](hospitals, sf, .id = "hospital")
}
