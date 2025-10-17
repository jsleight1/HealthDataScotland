#' R6 class storing health statistics for a list of gp health units.
#'
#' This R6 class is designed to store population demography data for multiple
#' GP practices. This class can be used to plot summary statistics and
#' create shiny UI/server objects.
#'
#' @examples
#' gps <- lapply(c("10002", "10017"), example_gp_unit)
#' x <- gp_grp[["new"]](gps, .id = "gp")
#' x[["ID"]]()
#' x[["IDs"]]()
#' x[["titles"]]()
#' x[["metadata"]]()
#' x[["data"]]()
#' x[["subset"]](id = "10002")
#' x[["plot"]](type = "national_pyramid")
#' x[["plot_data"]](type = "national_pyramid")
#' x[["plot_info"]](type = "national_pyramid")
#' x[["summary"]](type = "lookup")
#' x[["summary_info"]](type = "lookup")
#' \dontrun{
#' x[["ui"]]()
#' x[["server"]]()
#' }
#' @export
gp_grp <- R6Class("gp_grp",
  inherit = health_unitgrp,
  private = list(
    gender_choices = function() {
      unique(private[["map_combine"]]("data")[["Sex"]])
    },
    national_pyramid = function() {
      self[["plot_data"]](type = "national_pyramid") |>
        e_pyramid() |>
        e_y_axis(axisLabel = list(fontSize = 10))
    },
    national_trend = function(...) {
      self[["plot_data"]](type = "national_trend", ...) |>
        private[["trend_echart"]]()
    },
    national_trend_data = function() {
      self[["combine_data"]]() |>
        filter(.data[["Sex"]] != "All") |>
        private[["trend_data"]](groups = c("Date", "Gender")) |>
        mutate(Gender = factor(.data[["Gender"]], levels = c("Male", "Female"))) |>
        group_by(.data[["Gender"]])
    },
    national_pyramid_data = function() {
      private[["map_combine"]]("data") |>
        pyramid_data(groups = c("Date", "Gender", "Age"))
    },
    health_board_trend = function(...) {
      self[["plot_data"]](type = "health_board_trend", ...) |>
        private[["trend_echart"]]()
    },
    health_board_bar = function(...) {
      self[["plot_data"]](type = "health_board_bar", ...) |>
        private[["bar_echart"]]()
    },
    health_board_trend_data = function(health_board = private[["health_board_choices"]](),
                                       gender = private[["gender_choices"]]()) {
      health_board <- arg_match(health_board, multiple = TRUE)
      gender <- arg_match(gender)
      private[["health_board_data"]](health_board, gender) |>
        private[["trend_data"]](groups = c("Date", "Gender", "HBName")) |>
        group_by(.data[["HBName"]])
    },
    health_board_bar_data = function(health_board = private[["health_board_choices"]](),
                                     gender = private[["gender_choices"]]()) {
      health_board <- arg_match(health_board, multiple = TRUE)
      gender <- arg_match(gender)
      private[["health_board_data"]](health_board, gender) |>
        private[["bar_data"]](col = "HBName", groups = c("Date", "Age", "HBName"))
    },
    health_board_data = function(health_board, gender) {
      self[["combine_data"]]() |>
        filter(.data[["Sex"]] %in% gender) |>
        filter(.data[["HBName"]] %in% health_board)
    },
    gp_trend = function(...) {
      self[["plot_data"]](type = "gp_trend", ...) |>
        private[["trend_echart"]]()
    },
    gp_bar = function(...) {
      self[["plot_data"]](type = "gp_bar", ...) |>
        private[["bar_echart"]]()
    },
    gp_trend_data = function(gp = private[["unit_choices"]](),
                             gender = private[["gender_choices"]]()) {
      gp <- arg_match(gp, multiple = TRUE)
      gender <- arg_match(gender)
      private[["gp_data"]](gp, gender) |>
        private[["trend_data"]]() |>
        group_by(.data[["ID"]])
    },
    gp_bar_data = function(gp = private[["unit_choices"]](),
                           gender = private[["gender_choices"]]()) {
      gp <- arg_match(gp, multiple = TRUE)
      gender <- arg_match(gender)
      private[["gp_data"]](gp, gender) |>
        private[["bar_data"]]("ID")
    },
    gp_data = function(gp, gender) {
      self[["combine_data"]]() |>
        filter(.data[["Sex"]] == gender) |>
        mutate(ID = paste(.data[["ID"]], "-", .data[["GPPracticeName"]])) |>
        filter(.data[["ID"]] %in% gp)
    },
    trend_echart = function(x) {
      super[["trend_echart"]](x, "Date", "Population")
    },
    bar_echart = function(x) {
      super[["bar_echart"]](x, "Age", "Age", "Population")
    },
    trend_data = function(x, ...) {
      x |>
        select("Date", "ID",
          "Gender" = "Sex",
          "HBName", "Population" = "AllAges"
        ) |>
        distinct() |>
        summarise_population(...)
    },
    bar_data = function(x, col, ...) {
      x |>
        select("Date", all_of(col), matches("^Ages\\d"), -matches("QF$")) |>
        pivot_longer(-c("Date", all_of(col)), names_to = "Age", values_to = "Population") |>
        summarise_population(...) |>
        factor_age() |>
        pivot_wider(names_from = all_of(col), values_from = "Population") |>
        arrange(.data[["Date"]], desc(.data[["Age"]])) |>
        group_by(.data[["Date"]])
    },
    national_trend_info = function() {
      "This line chart shows the total number of GP registered patients
        in Scotland (y-axis) across time (x-axis) for each gender (colour).
        Specifically, this plot combines data for all available GP practices
        across Scotland and calculates the total number of GP registered
        patients per gender. Please note this may show a total number of patients
        greater than the population. This may be caused by a few factors such as
        patients being registered at multiple GP practices. Please consult the
        original data sets for further information."
    },
    national_pyramid_info = function() {
      "This bar chart shows a population pyramid of the total number of
        GP registered patients in Scotland (x-axis) across age category
        (y-axis) for each gender (colour). Specifically, this plot combines data for all available GP practices
        across Scotland and calculates the total number of GP registered
        patients per age and gender. Please note this may show a total number of patients
        greater than the population. This may be caused by a few factors such as
        patients being registered at multiple GP practices. Please consult the
        original data sets for further information."
    },
    health_board_trend_info = function() {
      "This line chart shows the total number of GP registered patients
        (y-axis) for each health board (colour) across time (x-axis) for
        the selected gender. Specifically, this plot combines data for all
        available GP practices across the selected health boards and gender,
        and calculates the total number of GP registered patients.
        Settings can be used to show data for different health boards and
        genders. Please note this may show a total number of patients
        greater than the population of the health board. This may be caused by
        a few factors such as patients being registered at multiple GP practices.
        Please consult the original data sets for further information."
    },
    health_board_bar_info = function() {
      "This bar chart shows the total number of GP registered patients
        (y-axis) for each health board (colour) across age categories
        (x-axis). Specifically, this plot combines data for all available
        GP practices across the selected health boards and gender, and
        calculates the total number of GP registered patients. Settings can
        be used to show data for different health boards and genders. Please note this may show a total number of patients
        greater than the population of the health board. This may be caused by
        a few factors such as patients being registered at multiple GP practices.
        Please consult the original data sets for further information."
    },
    gp_trend_info = function() {
      "This line chart shows the total number of GP registered patients
        (y-axis) for each individal GP practice (colour) across time (x-axis)
        for the selected gender. Settings can be used to show data for
        different GP practices and genders."
    },
    gp_bar_info = function() {
      "This bar chart shows the total number of GP registered patients
        (y-axis) for each individal GP practice (colour) across
        age categories (x-axis). Settings can be used to show data for
        different GP practices and genders."
    },
    lookup = function(...) {
      data.frame(
        Title = self[["titles"]](),
        ID = self[["IDs"]](),
        Address = self[["addresses"]](),
        Telephone = self[["telephones"]](),
        `Health board` = self[["health_boards"]](),
        check.names = FALSE,
        ...
      )
    },
    lookup_info = function(...) {
      "This lookup table presents data for all available GP practices
      in the data set. This table can be searched, filtered and
      the 'Plot' column allows the user to view statistics for a
      selected GP practice."
    }
  ),
  public = list(
    #' @description
    #' Get character vector of gp unit telephone numbers.
    telephones = function() {
      unname(map_chr(self[["data"]](), ~ .x[["telephone"]]()))
    },
    #' @description
    #' Get character vector of plot types for gp grp.
    plot_types = function() {
      c(
        "national_trend", "national_pyramid", "health_board_trend",
        "health_board_bar", "gp_trend", "gp_bar"
      )
    },
    #' @description
    #' Plot gp grp.
    #' @param type (character(1))\cr
    #'   Character specifying plot type. See `plot_types` for options.
    #' @param ... Passed to plot functions.
    #' @examples
    #' x <- example_gp_grp_unit()
    #' x[["plot"]](type = "gp_bar")
    plot = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "national_trend" = private[["national_trend"]],
        "national_pyramid" = private[["national_pyramid"]],
        "health_board_trend" = private[["health_board_trend"]],
        "health_board_bar" = private[["health_board_bar"]],
        "gp_trend" = private[["gp_trend"]],
        "gp_bar" = private[["gp_bar"]]
      )(...)
    },
    #' @description
    #' Get plot data for gp grp.
    #' @param type (character(1))\cr
    #'   Character specifying plot type. See `plot_types` for options.
    #' @param ... Passed to plot data functions.
    #' @examples
    #' x <- example_gp_grp_unit()
    #' x[["plot_data"]](type = "gp_bar")
    plot_data = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "national_trend" = private[["national_trend_data"]],
        "national_pyramid" = private[["national_pyramid_data"]],
        "health_board_trend" = private[["health_board_trend_data"]],
        "health_board_bar" = private[["health_board_bar_data"]],
        "gp_trend" = private[["gp_trend_data"]],
        "gp_bar" = private[["gp_bar_data"]]
      )(...)
    },
    #' @description
    #' Get plot info for gp grp.
    #' @param type (character(1))\cr
    #'   Character specifying plot type. See `plot_types` for options.
    #' @param ... Passed to plot info functions.
    #' @examples
    #' x <- example_gp_grp_unit()
    #' x[["plot_info"]](type = "gp_bar")
    plot_info = function(type, ...) {
      type <- arg_match(type, values = self[["plot_types"]]())
      switch(type,
        "national_trend" = private[["national_trend_info"]],
        "national_pyramid" = private[["national_pyramid_info"]],
        "health_board_trend" = private[["health_board_trend_info"]],
        "health_board_bar" = private[["health_board_bar_info"]],
        "gp_trend" = private[["gp_trend_info"]],
        "gp_bar" = private[["gp_bar_info"]]
      )(...)
    },
    #' @description
    #' Create UI for general practice group object.
    #' @param ... Passed to functions.
    ui = function(...) {
      ns <- NS(self[["ID"]]())
      nav_panel(
        title = "General practice",
        class = "overflow-auto",
        navset_tab(
          nav_panel(
            title = "Visualisation",
            div(
              card(
                card_header("National summary"),
                full_screen = TRUE,
                layout_column_wrap(
                  card(
                    full_screen = TRUE,
                    card_header(
                      "National GP population trend",
                      help_popover(
                        id = ns("national_trend_help"),
                        self[["plot_info"]]("national_trend")
                      )
                    ),
                    e_output_spinner(ns("national_pop_trend"))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header(
                      "National GP population per gender and age group",
                      help_popover(
                        id = ns("national_pyramid_help"),
                        self[["plot_info"]]("national_pyramid")
                      )
                    ),
                    e_output_spinner(ns("national_pop_pyramid"))
                  )
                )
              ),
              card(
                full_screen = TRUE,
                card_header("Health board summary"),
                layout_column_wrap(
                  card(
                    full_screen = TRUE,
                    card_header(
                      "Health board GP population",
                      help_popover(
                        id = ns("hb_trend_help"),
                        self[["plot_info"]]("health_board_trend")
                      ),
                      settings_popover(
                        id = ns("hb_trend_settings"),
                        virtual_select_input(
                          inputId = ns("select_hb_trend_hb"),
                          label = "Select health boards",
                          multiple = TRUE,
                          choices = private[["health_board_choices"]](),
                          selected = private[["health_board_choices"]]()
                        ),
                        virtual_select_input(
                          inputId = ns("select_hb_trend_gender"),
                          label = "Select gender",
                          choices = c("All", "Male", "Female"),
                          selected = "All"
                        )
                      )
                    ),
                    e_output_spinner(ns("hb_pop_trend"))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header(
                      "Health board GP population per age group",
                      help_popover(
                        id = ns("hb_bar_help"),
                        self[["plot_info"]]("health_board_bar")
                      ),
                      settings_popover(
                        id = ns("hb_bar_settings"),
                        virtual_select_input(
                          inputId = ns("select_hb_bar_hb"),
                          label = "Select health boards",
                          multiple = TRUE,
                          choices = private[["health_board_choices"]](),
                          selected = private[["health_board_choices"]]()
                        ),
                        virtual_select_input(
                          inputId = ns("select_hb_bar_gender"),
                          label = "Select gender",
                          choices = c("All", "Male", "Female"),
                          selected = "All"
                        )
                      )
                    ),
                    e_output_spinner(ns("hb_pop_bar"))
                  )
                )
              ),
              card(
                full_screen = TRUE,
                card_header("Individual practice summary"),
                layout_column_wrap(
                  card(
                    full_screen = TRUE,
                    card_header(
                      "GP population for selected practice and gender",
                      help_popover(
                        id = ns("gp_trend_help"),
                        self[["plot_info"]]("gp_trend")
                      ),
                      settings_popover(
                        id = ns("gp_trend_settings"),
                        virtual_select_input(
                          inputId = ns("select_gp_trend_gp"),
                          label = "Select individual GP practices",
                          multiple = TRUE,
                          choices = private[["unit_choices"]](),
                          selected = private[["unit_choices"]]()[1]
                        ),
                        virtual_select_input(
                          inputId = ns("select_gp_trend_gender"),
                          label = "Select gender",
                          choices = c("All", "Male", "Female"),
                          selected = "All"
                        )
                      )
                    ),
                    e_output_spinner(ns("gp_pop_trend"))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header(
                      "GP population for selected practice and gender per age group",
                      help_popover(
                        id = ns("gp_bar_help"),
                        self[["plot_info"]]("gp_bar")
                      ),
                      settings_popover(
                        id = ns("gp_bar_settings"),
                        virtual_select_input(
                          inputId = ns("select_gp_bar_gp"),
                          label = "Select individual GP practices",
                          multiple = TRUE,
                          choices = private[["unit_choices"]](),
                          selected = private[["unit_choices"]]()[1]
                        ),
                        virtual_select_input(
                          inputId = ns("select_gp_bar_gender"),
                          label = "Select gender",
                          choices = c("All", "Male", "Female"),
                          selected = "All"
                        )
                      )
                    ),
                    e_output_spinner(ns("gp_pop_bar"))
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
                  id = ns("lookup_help"),
                  self[["summary_info"]]("lookup")
                )
              ),
              withSpinner(DTOutput(ns("lookup")))
            )
          )
        )
      )
    },
    #' @description
    #' Create server for general practice group object.
    server = function() {
      moduleServer(
        self[["ID"]](),
        function(input, output, session) {
          ns <- session[["ns"]]

          purrr::walk(c("trend", "pyramid"), function(plt) {
            task <- ExtendedTask[["new"]](function(x, type) {
              log_info(glue("Creating GP national {plt} plot"))
              mirai(x[["plot"]](type = type), x = x, type = type)
            })

            invoke_task <- reactive({
              task[["invoke"]](x = self, type = glue("national_{plt}"))
            })

            output[[glue("national_pop_{plt}")]] <- renderEcharts4r({
              invoke_task()
              task[["result"]]()
            })
          })

          purrr::walk(c("trend", "bar"), function(plt) {
            task <- ExtendedTask[["new"]](function(x, type, health_board, gender) {
              log_info(glue("Creating GP health board {plt} plot"))
              mirai({
                x[["plot"]](type = type, health_board = health_board, gender = gender)
              }, x = x, type = type, health_board = health_board, gender = gender)
            })

            invoke_task <- reactive({
              task[["invoke"]](
                x = self,
                type = glue("health_board_{plt}"),
                health_board = req(input[[glue("select_hb_{plt}_hb")]]),
                gender = req(input[[glue("select_hb_{plt}_gender")]])
              )
            })

            output[[glue("hb_pop_{plt}")]] <- renderEcharts4r({
              invoke_task()
              task[["result"]]()
            })
          })

          purrr::walk(c("trend", "bar"), function(plt) {
            task <- ExtendedTask[["new"]](function(x, type, gp, gender) {
              log_info("Creating GP {plt} plot")
              mirai({
                x[["plot"]](type = type, gp = gp, gender = gender)
              }, x = x, type = type, gp = gp, gender = gender)
            })

            invoke_task <- reactive({
              task[["invoke"]](
                x = self,
                type = glue("gp_{plt}"),
                gp = req(input[[glue("select_gp_{plt}_gp")]]),
                gender = req(input[[glue("select_gp_{plt}_gender")]])
              )
            })

            output[[glue("gp_pop_{plt}")]] <- renderEcharts4r({
              invoke_task()
              task[["result"]]()
            })
          })

          output[["download"]] <- self[["download_handler"]]()

          lookup_dt_task <- ExtendedTask$new(function(x, ns) {
            mirai(x[["datatable"]]("lookup", ns), x = self, ns = ns)
          })

          invoke_dt_task <- reactive({
            lookup_dt_task[["invoke"]](x = self, ns = ns)
          })

          output[["lookup"]] <- renderDT({
            log_info("Creating gp lookup table")
            invoke_dt_task()
            lookup_dt_task[["result"]]()
          })
          observe({
            log_info("Rendering gp unit popup")
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
#' @param ids Character IDs of GP practice to get. Default is c("10002", "10017").
#' @export
example_gp_grp_unit <- function(ids = c("10002", "10017")) {
  gps <- map(ids, example_gp_unit)
  gp_grp[["new"]](gps, .id = "gp")
}
