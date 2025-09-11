
#' R6 class storing health statistics for a list of gp health units.
gp_grp <- R6Class("gp_grp",
    inherit = health_unitgrp,
    private = list(
        date_choices = function() {
            private[["map_combine"]](func = "data") |>
                pull("Date") |>
                unique()
        },
        gender_choices = function() {
            private[["map_combine"]](func = "data") |>
                pull("Sex") |>
                unique()
        },
        health_board_choices = function() {
            private[["map_combine"]](func = "data") |>
                pull("HBName") |>
                unique()
        },
        gp_choices = function() {
            private[["map_combine"]](func = "data") |>
                select("ID", "GPPracticeName") |>
                mutate(
                    ID = paste(
                        .data[["ID"]],
                        "-",
                        .data[["GPPracticeName"]]
                    )
                ) |>
                pull("ID") |>
                unique()
        },
        factor_age = function(x) {
            x |>
                mutate(
                    Age = factor(
                        .data[["Age"]], levels = c(
                        "Ages85plus",
                        "Ages75to84",
                        "Ages65to74",
                        "Ages45to64",
                        "Ages25to44",
                        "Ages15to24",
                        "Ages5to14",
                        "Ages0to4"
                    )
                )
            )
        },
        trend_echart = function(x, title) {
            x |>
                e_charts(Date) |>
                e_line(Population) |>
                e_tooltip(trigger = "axis") |>
                e_title(title, left = "center") |>
                e_legend(show = FALSE)
        },
        bar_echart = function(x, title) {
            plt <- x |>
                e_charts(Age, timeline = TRUE) |>
                e_timeline_opts(autoPlay = TRUE) |>
                e_tooltip(trigger = "axis") |>
                e_legend(show = FALSE) |>
                e_title(title)
            for (hb in setdiff(colnames(x), c("Date", "Age"))) {
                plt <- e_bar_(plt, hb)
            }
            plt
        },
        national_trend_data = function() {
            private[["map_combine"]](func = "data") |>
                filter(.data[["Sex"]] != "All") |>
                select("Date", "GPPracticeName", "Gender" = "Sex",
                    "Population" = "AllAges") |>
                distinct() |>
                group_by_at(c("Date", "Gender")) |>
                summarise(
                    Population = sum(.data[["Population"]], na.rm = TRUE)
                ) |>
                mutate(Gender = factor(Gender, levels = c("Male", "Female"))) |>
                group_by(.data[["Gender"]])
        },
        national_trend = function(...) {
            self[["plot_data"]](type = "national_trend", ...) |>
                private[["trend_echart"]]("National GP population trend")
        },
        national_pyramid_data = function() {
            private[["map_combine"]](func = "data") |>
                filter(.data[["Sex"]] != "All") |>
                select("Date", "Gender" = "Sex", matches("Ages\\d"), -contains("QF"),
                    -"AllAges") |>
                pivot_longer(
                    -c("Gender", "Date"),
                    names_to = "Age",
                    values_to = "Population"
                ) |>
                group_by_at(c("Date", "Gender", "Age")) |>
                summarise(
                    Population = sum(.data[["Population"]], na.rm = TRUE)
                ) |>
                pivot_wider(names_from = "Gender", values_from = "Population") |>
                private[["factor_age"]]() |>
                mutate(Female = Female * -1) |>
                group_by(.data[["Date"]])
        },
        national_pyramid = function() {
            self[["plot_data"]](type = "national_pyramid") |>
                e_charts(Age, timeline = TRUE) |>
                e_timeline_opts(autoPlay = TRUE) |>
                e_bar(Male, stack = "quantity") |>
                e_bar(Female, stack = "quantity") |>
                e_flip_coords() |>
                e_x_axis(
                    axisLabel = list(
                        formatter = htmlwidgets::JS(
                            'function (value) {
                                return(Math.abs(value))
                            }'
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
                    top = 25,
                    left = "center",
                    data = c("Female", "Male")
                ) |>
                e_title("National GP population pyramid", left = "center") |>
                e_y_axis(axisLabel = list(fontSize = 10))
        },
        health_board_trend_data = function(
                health_board = private[["health_board_choices"]](),
                gender = private[["gender_choices"]]()
            ) {
            health_board <- arg_match(health_board, multiple = TRUE)
            gender <- arg_match(gender)
            private[["map_combine"]](func = "data") |>
                filter(.data[["Sex"]] %in% gender) |>
                filter(.data[["HBName"]] %in% health_board) |>
                select("Date", "GPPracticeName", "Gender" = "Sex",
                    "HBName", "Population" = "AllAges") |>
                distinct() |>
                group_by_at(c("Date", "Gender", "HBName")) |>
                summarise(
                    Population = sum(.data[["Population"]], na.rm = TRUE)
                ) |>
                group_by(.data[["HBName"]])
        },
        health_board_trend = function(...) {
            self[["plot_data"]](type = "health_board_trend", ...) |>
                private[["trend_echart"]]("Health board GP population")
        },
        health_board_bar_data = function(
                health_board = private[["health_board_choices"]](),
                gender = private[["gender_choices"]]()
            ) {
            health_board <- arg_match(health_board, multiple = TRUE)
            gender <- arg_match(gender)
            private[["map_combine"]](func = "data") |>
                filter(.data[["Sex"]] %in% gender) |>
                filter(.data[["HBName"]] %in% health_board) |>
                select("Date", matches("Ages\\d"), -contains("QF"),
                    -"AllAges", "HBName") |>
                pivot_longer(
                    -c("HBName", "Date"),
                    names_to = "Age",
                    values_to = "Population"
                ) |>
                group_by_at(c("Date", "Age", "HBName")) |>
                summarise(
                    Population = sum(.data[["Population"]], na.rm = TRUE)
                ) |>
                private[["factor_age"]]() |>
                pivot_wider(names_from = "HBName", values_from = "Population") |>
                group_by(.data[["Date"]])
        },
        health_board_bar = function(...) {
            self[["plot_data"]](type = "health_board_bar", ...) |>
                private[["bar_echart"]](
                    title = "Health board GP population per group"
                )
        },
        gp_trend_data = function(
                gp = private[["gp_choices"]](),
                gender = private[["gender_choices"]]()
            ) {
            gp <- arg_match(gp, multiple = TRUE)
            gender <- arg_match(gender)
            private[["map_combine"]](func = "data") |>
                filter(.data[["Sex"]] == gender) |>
                mutate(
                    ID = paste(
                        .data[["ID"]],
                        "-",
                        .data[["GPPracticeName"]]
                    )
                ) |>
                filter(.data[["ID"]] %in% gp) |>
                select("Date", "ID", "Gender" = "Sex",
                    "HBName", "Population" = "AllAges") |>
                distinct() |>
                group_by_at(c("Date", "Gender")) |>
                group_by(.data[["ID"]])
        },
        gp_trend = function(...) {
            self[["plot_data"]](type = "gp_trend", ...) |>
                private[["trend_echart"]]("GP population")
        },
        gp_bar_data = function(
                gp = private[["gp_choices"]](),
                gender = private[["gender_choices"]]()
            ) {
            gp <- arg_match(gp, multiple = TRUE)
            gender <- arg_match(gender)
            private[["map_combine"]](func = "data") |>
                filter(.data[["Sex"]] == gender) |>
                mutate(
                    ID = paste(
                        .data[["ID"]],
                        "-",
                        .data[["GPPracticeName"]]
                    )
                ) |>
                filter(.data[["ID"]] %in% gp) |>
                select("Date", matches("Ages\\d"), -contains("QF"),
                    -"AllAges", "ID") |>
                pivot_longer(
                    -c("ID", "Date"),
                    names_to = "Age",
                    values_to = "Population"
                ) |>
                private[["factor_age"]]() |>
                pivot_wider(names_from = "ID", values_from = "Population") |>
                group_by(.data[["Date"]])
        },
        gp_bar = function(...) {
            self[["plot_data"]](type = "gp_bar", ...) |>
                private[["bar_echart"]](
                    title = "Population per GP practice"
                )
        }
    ),
    public = list(
        #' @description
        #' Get character vector of available plots for gp grp.
        available_plots = function() {
            c("national_trend", "national_pyramid", "health_board_trend",
                "health_board_bar", "gp_trend", "gp_bar")
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
        #'     Character specifying plot type. See `available_plots`
        #'   for options.
        #' @param ... Passed to plot data functions.
        plot_data = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
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
        ui = function() {
            ns <- NS(self[["id"]]())
            data <- private[["map_combine"]](func = "data")
            nav_panel(
                title = "General practice",
                class = "overflow-auto",
                div(
                    card(
                        card_header("National summary"),
                        full_screen = TRUE,
                        layout_column_wrap(
                            card(
                                full_screen = TRUE,
                                uiOutput(outputId = ns("national_pop_trend"))
                            ),
                            card(
                                full_screen = TRUE,
                                uiOutput(outputId = ns("national_pop_pyramid"))
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        card_header(
                            "Health board summary",
                            popover(
                                id = ns("hb_summary_settings"),
                                bs_icon("gear", class = "ms-auto"),
                                virtualSelectInput(
                                    inputId = ns("select_hb"),
                                    label = "Select health boards",
                                    multiple = TRUE,
                                    choices = unique(data[["HBName"]]),
                                    selected = unique(data[["HBName"]])
                                ),
                                virtualSelectInput(
                                    inputId = ns("select_hb_gender"),
                                    label = "Select gender",
                                    choices = c("All", "Male", "Female"),
                                    selected = "All"
                                )
                            ),
                        ),
                        layout_column_wrap(
                            card(
                                full_screen = TRUE,
                                uiOutput(outputId = ns("hb_pop_trend"))
                            ),
                            card(
                                full_screen = TRUE,
                                uiOutput(outputId = ns("hb_pop_bar"))
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        card_header(
                            "Individual practice summary",
                            popover(
                                id = ns("gp_summary_settings"),
                                bs_icon("gear", class = "ms-auto"),
                                virtualSelectInput(
                                    inputId = ns("select_gp"),
                                    label = "Select individual GP practices",
                                    multiple = TRUE,
                                    choices = paste(
                                        self[["ids"]](),
                                        "-",
                                        self[["titles"]]()
                                    ),
                                    selected = paste(
                                        self[["ids"]](),
                                        "-",
                                        self[["titles"]]()
                                    )[1],
                                    search = TRUE,
                                    showSelectedOptionsFirst = TRUE,
                                    updateOn = "close"
                                ),
                                virtualSelectInput(
                                    inputId = ns("select_gp_gender"),
                                    label = "Select gender",
                                    choices = c("All", "Male", "Female"),
                                    selected = "All"
                                )
                            )
                        ),
                        layout_column_wrap(
                            card(
                                full_screen = TRUE,
                                uiOutput(outputId = ns("gp_pop_trend"))
                            ),
                            card(
                                full_screen = TRUE,
                                uiOutput(outputId = ns("gp_pop_bar"))
                            )
                        )
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
                    ns <- session[["ns"]]
                    data <- private[["map_combine"]](func = "data")

                    output[["national_pop_trend"]] <- renderUI({
                        self[["plot"]](type = "national_trend")
                    })
                    output[["national_pop_pyramid"]] <- renderUI({
                        self[["plot"]](type = "national_pyramid")
                    })

                    hb_pop_trend_plt <- reactive({
                        self[["plot"]](
                            type = "health_board_trend",
                            health_board = input[["select_hb"]],
                            gender = input[["select_hb_gender"]]
                        )
                    }) |>
                    bindEvent(input[["select_hb"]], input[["select_hb_gender"]])

                    hb_pop_bar_plt <- reactive({
                        self[["plot"]](
                            type = "health_board_bar",
                            health_board = input[["select_hb"]],
                            gender = input[["select_hb_gender"]]
                        )
                    }) |>
                    bindEvent(input[["select_hb"]], input[["select_hb_gender"]])

                    output[["hb_pop_trend"]] <- renderUI(hb_pop_trend_plt())

                    output[["hb_pop_bar"]] <- renderUI(hb_pop_bar_plt())

                    gp_pop_trend_plt <- reactive({
                        self[["plot"]](
                            type = "gp_trend",
                            gp = input[["select_gp"]],
                            gender = input[["select_gp_gender"]]
                        )
                    }) |>
                    bindEvent(input[["select_gp"]], input[["select_gp_gender"]])

                    output[["gp_pop_trend"]] <- renderUI(gp_pop_trend_plt())

                    gp_pop_bar_plt <- reactive({
                        self[["plot"]](
                            type = "gp_bar",
                            gp = input[["select_gp"]],
                            gender = input[["select_gp_gender"]]
                        )
                    }) |>
                    bindEvent(input[["select_gp"]], input[["select_gp_gender"]])

                    output[["gp_pop_bar"]] <- renderUI(gp_pop_bar_plt())

                    output[["download"]] <- self[["download_handler"]]()
                }
            )
        }
    )
)