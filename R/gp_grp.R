
#' R6 class storing health statistics for a list of gp health units.
gp_grp <- R6Class("gp_grp", 
    inherit = health_unitgrp, 
    private = list(
        population_pyramid_data = function(date, practices = self[["titles"]]()) {
            self[["data"]]() |> 
                map(~.x[["plot_data"]]("population_pyramid", date)) |> 
                setNames(self[["titles"]]()) |> 
                bind_rows(.id = "Practice") |>
                filter(.data[["Practice"]] %in% practices)
        },
        population_pyramid = function(date,  ...) {
            dat <- self[["plot_data"]]("population_pyramid", date, ...)
            plot <- ggplot(dat, aes(Population = Population)) +
                    geom_bar(
                        aes(
                            x = Age, 
                            fill = Gender, 
                            y = ifelse(Gender == "Male", -Population,  Population), 
                            Practice = Practice
                        ), 
                        stat = "identity"
                    ) +
                    scale_y_continuous(
                        labels = abs, 
                        limits = max(dat$Population) * c(-1,1)
                    ) + 
                    coord_flip() + 
                    facet_wrap(~Practice) +
                    theme_bw() +
                    ylab(NULL) + 
                    xlab(NULL)
            ggplotly(plot, tooltip = c("Gender", "Age", "Population", "ID"))

        },
        population_trend_data = function(
                gender = "All",
                practices = self[["titles"]](), 
                ...
            ) {
            self[["data"]]() |> 
                map(~.x[["plot_data"]]("population_trend", gender)) |> 
                setNames(self[["titles"]]()) |> 
                bind_rows(.id = "Practice") |>
                filter(.data[["Practice"]] %in% practices)
        },
        population_trend = function( ...) {
            plot <- self[["plot_data"]]("population_trend", ...) |>
                ggplot(aes(x = Date, y = Population, group = Gender, colour = Practice)) + 
                    geom_line() + 
                    theme_bw() + 
                    theme(axis.text.x = element_text(angle = 90))
            ggplotly(plot, tooltip = c("Date", "Gender", "Population", "Practice"))
        },
        date_choices = function() {
            self[["data"]]() |> 
                map(~.x[["data"]]()[, "Date"]) |>
                bind_rows() |>
                pull("Date") |>
                unique()
        },
        gender_choices = function() {
            self[["data"]]() |> 
                map(~.x[["data"]]()[, "Sex"]) |>
                bind_rows() |>
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
        #' Generate plot data for gp grp.
        #' @param type (character(1))\cr
        #'     Character specifying plot type. See `available_plots` 
        #'   for options.
        #' @param ... Passed to plot_data functions.
        plot_data = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type, 
                "population_pyramid" = private[["population_pyramid_data"]](...), 
                "population_trend" = private[["population_trend_data"]](...)
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
            box(
                title = "General Practice",
                box(
                    title = "Population trend",
                    selectInput(
                        inputId = ns("pop_trend_select"), 
                        label = "Select gender",
                        choices = private[["gender_choices"]](),
                        selected = "All"
                    ),
                    pickerInput(
                        inputId = ns("pop_trend_select_practice"),
                        label = "Select GP practices", 
                        choices = self[["titles"]](),
                        selected = self[["titles"]](),
                        inline = TRUE,
                        multiple = TRUE, 
                        options = list(
                            `actions-box` = TRUE, 
                            `selected-text-format` = "count > 1"
                        )
                    ),
                    spinner(plotlyOutput(outputId = ns("pop_trend"))),
                    width = 12
                ),
                box(
                    title = "Population pyramid",
                    selectInput(
                        inputId = ns("pop_pyramid_select_date"), 
                        label = "Select time frame",
                        choices = private[["date_choices"]]()
                    ),
                    pickerInput(
                        inputId = ns("pop_pyramid_select_practice"),
                        label = "Select GP practices", 
                        choices = self[["titles"]](),
                        selected = self[["titles"]](),
                        inline = TRUE,
                        multiple = TRUE, 
                        options = list(
                            `actions-box` = TRUE, 
                            `selected-text-format` = "count > 1"
                        )
                    ),
                    spinner(plotlyOutput(outputId = ns("pop_pyramid"))),
                    width = 12
                ),
                width = 12, 
                status = "primary",
                solidHeader = TRUE,
            )
        }, 
        #' @description
        #' Create server for general practice group object.
        server = function() {
            moduleServer(
                self[["id"]](),
                function(input, output, session) {
                    output[["pop_trend"]] <- renderPlotly(
                        self[["plot"]](
                            type = "population_trend", 
                            gender = req(input[["pop_trend_select"]]),
                            practices = req(input[["pop_trend_select_practice"]])
                        )
                    )
                    output[["pop_pyramid"]] <- renderPlotly(
                        self[["plot"]](
                            type = "population_pyramid", 
                            date = req(input[["pop_pyramid_select_date"]]),
                            practices = req(input[["pop_pyramid_select_practice"]])
                        )
                    )
                }
            )
        }
    )
)