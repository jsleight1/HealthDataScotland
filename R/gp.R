#' R6 class storing health statistics for a GP practice.
gp <- R6Class("gp",
  inherit = health_unit,
  private = list(
    title_col = function() {
      "GPPracticeName"
    },
    required_metadata_cols = function() {
      c(
        "GPPracticeName", "PracticeListSize", "AddressLine1", "AddressLine2",
        "AddressLine3", "AddressLine4", "Postcode", "TelephoneNumber",
        "PracticeType", "GPCluster", "HBName"
      )
    },
    required_data_cols = function() {
      c(
        "Date", "Sex", "AllAges", "Ages85plus", "Ages75to84", "Ages65to74",
        "Ages45to64", "Ages25to44", "Ages15to24", "Ages5to14", "Ages0to4"
      )
    },
    population_pyramid_data = function() {
      self[["combine_data"]]() |>
        filter(.data[["Sex"]] != "All") |>
        select("Date",
          "Gender" = "Sex", matches("Ages\\d"), -contains("QF"),
          -"AllAges", "GPPracticeName"
        ) |>
        pivot_longer(
          -c("Gender", "GPPracticeName", "Date"),
          names_to = "Age",
          values_to = "Population"
        ) |>
        pivot_wider(names_from = "Gender", values_from = "Population") |>
        mutate(
          Age = factor(.data[["Age"]], levels = c(
            "Ages85plus",
            "Ages75to84",
            "Ages65to74",
            "Ages45to64",
            "Ages25to44",
            "Ages15to24",
            "Ages5to14",
            "Ages0to4"
          )),
          Female = Female * -1
        ) |>
        group_by(.data[["Date"]])
    },
    population_pyramid = function(...) {
      self[["plot_data"]]("population_pyramid", ...) |>
        e_charts(Age, timeline = TRUE) |>
        e_timeline_opts(autoPlay = TRUE) |>
        e_bar(Male, stack = "quantity") |>
        e_bar(Female, stack = "quantity") |>
        e_flip_coords() |>
        e_x_axis(
          axisLabel = list(
            formatter = htmlwidgets::JS(
              "function (value) {
                                return(Math.abs(value))
                            }"
            )
          )
        ) |>
        e_tooltip(
          trigger = "item",
          formatter = htmlwidgets::JS("
                    function(params){
                        return(
                            '<strong>' + 'Age: ' + '</strong>' + params.name + ' years' + '<br />' +
                            '<strong>' + 'Population: ' + '</strong>' + Math.abs(params.value[0])
                        )
                    }
                    ")
        ) |>
        e_legend(
          top = 10,
          left = "center",
          data = c("Female", "Male")
        ) |>
        e_title(self[["title"]]())
    },
    population_trend_data = function() {
      self[["combine_data"]]() |>
        filter(.data[["Sex"]] != "All") |>
        select("Date", "GPPracticeName",
          "Gender" = "Sex",
          "Population" = "AllAges"
        ) |>
        distinct() |>
        group_by(.data[["Gender"]])
    },
    population_trend_y_range = function() {
      pop <- self[["combine_data"]]() |>
        filter(.data[["Sex"]] != "All") |>
        pull("AllAges")
      c(floor(min(pop) * 0.99), ceiling(max(pop) * 1.01))
    },
    population_trend = function(...) {
      y_range <- private[["population_trend_y_range"]]()
      self[["plot_data"]]("population_trend", ...) |>
        e_charts(Date) |>
        e_line(Population) |>
        e_tooltip(trigger = "axis") |>
        e_y_axis(min = y_range[1], max = y_range[2]) |>
        e_title(self[["title"]]())
    },
    population_pyramid_info = function() {
      "This bar chart shows a population pyramid of the total number of
            GP registered patients (x-axis) across age category
            (y-axis) for each gender (colour)."
    },
    population_trend_info = function() {
      "This line chart shows the number of registered GP patients (y-axis)
            across time (x-axis) for each gender (colour)."
    }
  ),
  public = list(
    #' @description
    #' Get telephone number of GP practice.
    telephone = function() {
      self[["metadata"]]()[["TelephoneNumber"]]
    },
    #' @description
    #' Get GP cluster
    gp_cluster = function() {
      self[["metadata"]]()[["GPCluster"]]
    },
    #' @description
    #' Get character vector of available plots for gp unit. Options
    #'   are either "population_pyramid" plot or "population_trend" plot.
    available_plots = function() {
      c("population_pyramid", "population_trend")
    },
    #' @description
    #' Plot gp unit.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `available_plots`
    #'   for options.
    #' @param ... Passed to plot functions.
    plot = function(type, ...) {
      type <- arg_match(type, values = self[["available_plots"]]())
      switch(type,
        "population_pyramid" = private[["population_pyramid"]](...),
        "population_trend" = private[["population_trend"]](...)
      )
    },
    #' @description
    #' Generate plot data for gp unit.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `available_plots`
    #'   for options.
    #' @param ... Passed to plot data functions.
    plot_data = function(type, ...) {
      type <- arg_match(type, values = self[["available_plots"]]())
      switch(type,
        "population_pyramid" = private[["population_pyramid_data"]],
        "population_trend" = private[["population_trend_data"]]
      )(...)
    },
    #' @description
    #' Get plot info for gp unit.
    #' @param type (character(1))\cr
    #'     Character specifying plot type. See `available_plots`
    #'   for options.
    #' @param ... Passed to plot info functions.
    plot_info = function(type, ...) {
      type <- arg_match(type, values = self[["available_plots"]]())
      switch(type,
        "population_pyramid" = private[["population_pyramid_info"]](),
        "population_trend" = private[["population_trend_info"]]()
      )
    },
    #' @description
    #' Create UI for general practice object.
    #' @param ns
    #'     Namespace of shiny application page.
    ui = function(ns) {
      ns <- NS(ns(self[["ID"]]()))
      card(
        card_header(paste(self[["title"]](), "-", self[["ID"]]())),
        div(glue("Address: {self[['address']]()}")),
        div(glue("Telephone: {self[['telephone']]()}")),
        div(glue("Health Board: {self[['health_board']]()}")),
        card(
          full_screen = TRUE,
          card_header(
            "Population trend",
            popover(
              id = ns("pop_trend_help"),
              bs_icon("question-circle"),
              self[["plot_info"]]("population_trend")
            ),
          ),
          echarts4rOutput(ns("pop_trend"))
        ),
        card(
          full_screen = TRUE,
          card_header(
            "Population pyramid",
            popover(
              id = ns("pop_pyramid_help"),
              bs_icon("question-circle"),
              self[["plot_info"]]("population_pyramid")
            )
          ),
          echarts4rOutput(ns("pop_pyramid"))
        ),
        card(downloadButton(ns("download")))
      )
    },
    #' @description
    #' Create server for general practice object.
    server = function() {
      moduleServer(
        self[["ID"]](),
        function(input, output, session) {
          output[["pop_trend"]] <- renderEcharts4r({
            log_info("Creating GP population trend plot")
            self[["plot"]](type = "population_trend")
          })
          output[["pop_pyramid"]] <- renderEcharts4r({
            log_info("Creating GP population pyramid plot")
            self[["plot"]](type = "population_pyramid")
          })
          output[["download"]] <- downloadHandler(
            filename = function() "gp_data.csv",
            content = function(con) {
              write.csv(self[["data"]](), con)
            }
          )
        }
      )
    }
  )
)

#' Get example gp health unit object.
#' @param id Character ID of GP practice to get. Default is "10002".
#' @export
example_gp_unit <- function(id = "10002") {
  meta <- HealthDataScotland::example_gp_metadata |>
    rename("ID" = "PracticeCode", "HBName" = "HB") |>
    filter(.data[["ID"]] == id)
  data <- HealthDataScotland::example_gp_data |>
    select(-"datasetID", -"HSCP") |>
    rename("ID" = "PracticeCode") |>
    filter(.data[["ID"]] == id)
  gp[["new"]](meta, data)
}
