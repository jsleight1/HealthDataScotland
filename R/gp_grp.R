
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
            c("population_pyramid", "population_trend")
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
                "population_trend" = private[["population_trend"]](...)
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
                        card_header(
                            "Population trend",
                            popover(
                                id = ns("pop_trend_help"),
                                bs_icon("question-circle"),
                                "This line chart shows the population trend of
                                selected GP practices over time. Settings can
                                be used to show trends for selected practices."
                            ),
                            popover(
                                id = ns("pop_trend_settings"),
                                bs_icon("gear", class = "ms-auto"),
                                pickerInput(
                                    inputId = ns("pop_trend_select_practice"),
                                    label = "Select GP practices",
                                    choices = private[["id_name_selection"]](),
                                    selected = private[["id_name_selection"]](),
                                    inline = TRUE,
                                    multiple = TRUE,
                                    options = list(
                                        `actions-box` = TRUE,
                                        `selected-text-format` = "count > 1"
                                    )
                                )
                            ),
                            class = "d-flex align-items-center gap-1"
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
                            ),
                            popover(
                                id = ns("pop_pyramid_settings"),
                                bs_icon("gear", class = "ms-auto"),
                                pickerInput(
                                    inputId = ns("pop_pyramid_select_practice"),
                                    label = "Select GP practices",
                                    choices = private[["id_name_selection"]](),
                                    selected = private[["id_name_selection"]](),
                                    inline = TRUE,
                                    multiple = TRUE,
                                    options = list(
                                        `actions-box` = TRUE,
                                        `selected-text-format` = "count > 1"
                                    )
                                )
                            ),
                            class = "d-flex align-items-center gap-1"
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
                    output[["pop_trend"]] <- renderUI(
                        self[["plot"]](
                            type = "population_trend",
                            practices = req(input[["pop_trend_select_practice"]])
                        )
                    )
                    output[["pop_pyramid"]] <- renderUI(
                        self[["plot"]](
                            type = "population_pyramid",
                            practices = req(input[["pop_pyramid_select_practice"]])
                        )
                    )
                    output[["download"]] <- self[["download_handler"]]()
                }
            )
        }
    )
)