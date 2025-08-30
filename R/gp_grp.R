
#' R6 class storing health statistics for a list of gp health units.
gp_grp <- R6Class("gp_grp",
    inherit = health_unitgrp,
    private = list(
        population_pyramid_data = function(
                date,
                practices = self[["ids"]](),
                ...
            ) {
            private[["map_combine"]](
                    func = "plot_data",
                    type = "population_pyramid",
                    date = date,
                    ...
                ) |>
                filter(.data[["ID"]] %in% practices)
        },
        population_pyramid = function(...) {
            dat <- self[["plot_data"]]("population_pyramid", ...)
            labels <- private[["id_name_labels"]](dat, "GPPracticeName")
            plot <- ggplot(
                dat,
                aes(
                    Population = .data[["Population"]],
                    ID = .data[["ID"]],
                    GPPracticeName = .data[["GPPracticeName"]]
                )
            ) +
                geom_bar(
                    aes(
                        x = .data[["Age"]],
                        fill = .data[["Gender"]],
                        y = ifelse(Gender == "Male",
                            -.data[["Population"]],
                            .data[["Population"]]
                        )
                    ),
                    stat = "identity"
                ) +
                scale_y_continuous(
                    labels = abs,
                    limits = max(dat$Population) * c(-1,1)
                ) +
                coord_flip() +
                facet_wrap(~.data[["ID"]], labeller = labeller(ID = labels)) +
                theme_bw() +
                ylab(NULL) +
                xlab(NULL)
            ggplotly(plot, tooltip = c("Gender", "Age", "Population", "ID",
                "GPPracticeName"))
        },
        population_trend_data = function(
                gender = "All",
                practices = self[["ids"]](),
                ...
            ) {
            private[["map_combine"]](
                    func = "plot_data",
                    type = "population_trend",
                    gender = gender,
                    ...
                ) |>
                filter(.data[["ID"]] %in% practices)
        },
        population_trend = function( ...) {
            dat <- self[["plot_data"]]("population_trend", ...)
            labels <- private[["id_name_labels"]](dat, "GPPracticeName")
            plot <- ggplot(
                dat,
                aes(
                    x = .data[["Date"]],
                    y = .data[["Population"]],
                    colour = .data[["ID"]],
                    Gender = .data[["Gender"]],
                    GPPracticeName = .data[["GPPracticeName"]]
                )
            ) +
                geom_line() +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90))
            out <- ggplotly(plot, tooltip = c("Date", "Gender", "Population", "ID",
                "GPPracticeName"))
            # Manually required as ggplot doesn't handle scale_colour_hue.
            out[["x"]][["data"]][] <- map(out[["x"]][["data"]], function(i) {
                i[["name"]] <- labels[[i[["name"]]]]
                i
            })
            out
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
                                be used to show trends for selected genders and
                                practices."
                            ),
                            popover(
                                id = ns("pop_trend_settings"),
                                bs_icon("gear", class = "ms-auto"),
                                selectInput(
                                    inputId = ns("pop_trend_select"),
                                    label = "Select gender",
                                    choices = private[["gender_choices"]](),
                                    selected = "All"
                                )
                            )
                        ),
                        spinner(plotlyOutput(outputId = ns("pop_trend")))
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
                                selectInput(
                                    inputId = ns("pop_pyramid_select_date"),
                                    label = "Select time frame",
                                    choices = private[["date_choices"]]()
                                )
                            )
                        ),
                        spinner(plotlyOutput(outputId = ns("pop_pyramid")))
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
                    output[["pop_trend"]] <- renderPlotly(
                        self[["plot"]](
                            type = "population_trend",
                            gender = req(input[["pop_trend_select"]]),
                        )
                    )
                    output[["pop_pyramid"]] <- renderPlotly(
                        self[["plot"]](
                            type = "population_pyramid",
                            date = req(input[["pop_pyramid_select_date"]]),
                        )
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