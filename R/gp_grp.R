
#' R6 class storing health statistics for a list of gp health units.
gp_grp <- R6Class("gp_grp",
    inherit = health_unitgrp,
    private = list(
        population_pyramid = function(practices = self[["ids"]](), ...) {
            out <- self[["subset"]](id = practices)[["data"]]() |>
                map(function(i) i[["plot"]](type = "population_pyramid"))
            out |>
                append(c(rows = length(out), cols = 1)) %>%
                do.call(e_arrange, .)
        },
        population_trend = function(practices = self[["ids"]](), ...) {
            out <- self[["subset"]](id = practices)[["data"]]() |>
                map(function(i) i[["plot"]](type = "population_trend"))
            out |>
                append(c(rows = length(out), cols = 1)) %>%
                do.call(e_arrange, .)
        },
        population_bar = function(practices = self[["ids"]](), ...) {
            private[["map_combine"]](func = "data") |>
                filter(.data[["ID"]] %in% practices, .data[["Sex"]] != "All") |>
                select("Date", "GPPracticeName", "Sex", "AllAges") |>
                distinct() |>
                pivot_wider(names_from = "Sex", values_from = "AllAges") |>
                group_by(.data[["Date"]]) |>
                e_charts(x = GPPracticeName, timeline = TRUE) |>
                e_bar(Male, stack = "quantity") |>
                e_bar(Female, stack = "quantity") |>
                e_flip_coords() |>
                e_tooltip(trigger = "axis") |>
                e_timeline_opts(autoPlay = TRUE)
        },
        date_choices = function() {
            private[["map_combine"]](func = "data") |>
                pull("Date") |>
                unique()
        },
        gender_choices = function() {
            private[["map_combine"]](func = "data") |>
                pull("Sex") |>
                unique()
        }
    ),
    public = list(
        #' @description
        #' Get character vector of available plots for gp grp. Options
        #'   are either "population_pyramid" plot or "population_trend" plot.
        available_plots = function() {
            c("population_pyramid", "population_trend", "population_bar")
        },
        #' @description
        #' Plot gp grp.
        #' @param type (character(1))\cr
        #'     Character specifying plot type. See `available_plots`
        #'   for options.
        #' @param ... Passed to plot functions.
        plot = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type,
                "population_pyramid" = private[["population_pyramid"]](...),
                "population_trend" = private[["population_trend"]](...),
                "population_bar" = private[["population_bar"]](...)
            )
        },
        #' @description
        #' Summarise gp grp.
        #' @param type (character(1))\cr
        #'     Character specifying summary type. See `available_plots`.
        #' @param id (character(1))\cr
        #'     Character specifying the ID to assign to output data.frame
        #' @param ... Passed to plot_data functions.
        summary = function(type, id = "Scotland national", ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            self[["plot_data"]](type, ...) |>
                mutate(ID = id) |>
                group_by(across(any_of(c("ID", "Date", "Gender", "Age")))) |>
                summarise_if(is.numeric, sum, na.rm = TRUE) |>
                ungroup()
        },
        #' @description
        #' Create UI for general practice group object.
        #' @param ns
        #'     Namespace of shiny application page.
        ui = function(ns) {
            ns <- NS(ns(self[["id"]]()))
            nav_panel(
                title = "General practice",
                div(
                    card(
                        full_screen = TRUE,
                        card_header("Population bar plot"),
                        uiOutput(outputId = ns("pop_bar"))
                    ),
                    card(
                        full_screen = TRUE,
                        card_header(
                            "Population trend",
                            popover(
                                id = ns("pop_trend_help"),
                                bs_icon("question-circle"),
                                "This line chart shows the population trend of
                                selected GP practices over time. Settings can
                                be used to show trends for selected practices."
                            )
                        ),
                        spinner(uiOutput(outputId = ns("pop_trend")))
                    ),
                    card(
                        full_screen = TRUE,
                        card_header(
                            "Population pyramid",
                             popover(
                                id = ns("pop_pyramid_help"),
                                bs_icon("question-circle"),
                                "This bar chart shows a population pyramid of
                                selected GP practices. Settings can be used to
                                show data for selected time frames (default is
                                the most recent) and selected practices."
                            )
                        ),
                        spinner(uiOutput(outputId = ns("pop_pyramid")))
                    ),
                    card(downloadButton(ns("download")))
                )
            )
        },
        #' @description
        #' Create server for general practice group object.
        server = function() {
            moduleServer(
                self[["id"]](),
                function(input, output, session) {
                    output[["pop_bar"]] <- renderUI(
                        self[["plot"]](type = "population_bar")
                    )
                    output[["pop_trend"]] <- renderUI(
                        self[["plot"]](type = "population_trend")
                    )
                    output[["pop_pyramid"]] <- renderUI(
                        self[["plot"]](type = "population_pyramid")
                    )
                    output[["download"]] <- self[["download_handler"]]()
                }
            )
        },
        group_choices = function() {
            c("GP cluster", "Health board", "National", "Map")
        },
        group_column = function(x) {
            switch(arg_match(x, self[["group_choices"]]()),
                "GP cluster" = "GPCluster",
                "Health board" = "HBName",
                "National" = "ID"
            )
        }
    )
)