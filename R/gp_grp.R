
#' R6 class storing health statistics for a list of gp health units.
gp_grp <- R6Class("gp_grp",
    inherit = health_unitgrp,
    private = list(
        date_choices = function() {
            self[["get_download"]]() |>
                pull("Date") |>
                unique()
        },
        gender_choices = function() {
            self[["get_download"]]() |>
                pull("Sex") |>
                unique()
        },
        health_board_choices = function() {
            self[["get_download"]]() |>
                pull("HBName") |>
                unique()
        },
        gp_choices = function() {
            paste(self[["ids"]](), "-", self[["titles"]]()) |>
                unique()
        },
        trend_echart = function(x) {
            super$trend_echart(x, "Date", "Population")
        },
        bar_echart = function(x) {
            super$bar_echart(x, "Age", "Age", "Population")
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
        factor_gender = function(x) {
            x |>
                mutate(Gender = factor(.data[["Gender"]], levels = c("Male", "Female")))
        },
        summarise_population = function(x, groups) {
            x |>
                group_by_at(groups) |>
                summarise(Population = sum(.data[["Population"]], na.rm = TRUE))
        },
        national_trend_data = function() {
            self[["get_download"]]() |>
                filter(.data[["Sex"]] != "All") |>
                distinct() |>
                rename("Population" = "AllAges", "Gender" = "Sex") |>
                private[["summarise_population"]](c("Date", "Gender")) |>
                private[["factor_gender"]]() |>
                group_by(.data[["Gender"]])
        },
        national_trend = function(...) {
            self[["plot_data"]](type = "national_trend", ...) |>
                private[["trend_echart"]]()
        },
        national_pyramid_data = function() {
            self[["get_download"]]() |>
                filter(.data[["Sex"]] != "All") |>
                select("Date", "Gender" = "Sex", matches("Ages\\d"), -contains("QF"),
                    -"AllAges") |>
                pivot_longer(
                    -c("Gender", "Date"),
                    names_to = "Age",
                    values_to = "Population"
                ) |>
                private[["summarise_population"]](c("Date", "Gender", "Age")) |>
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
                e_y_axis(axisLabel = list(fontSize = 10))
        },
        health_board_trend_data = function(
                health_board = private[["health_board_choices"]](),
                gender = private[["gender_choices"]]()
            ) {
            health_board <- arg_match(health_board, multiple = TRUE)
            gender <- arg_match(gender)
            self[["get_download"]]() |>
                filter(.data[["Sex"]] %in% gender) |>
                filter(.data[["HBName"]] %in% health_board) |>
                rename("Population" = "AllAges", "Gender" = "Sex") |>
                distinct() |>
                private[["summarise_population"]](c("Date", "Gender", "HBName")) |>
                group_by(.data[["HBName"]])
        },
        health_board_trend = function(...) {
            self[["plot_data"]](type = "health_board_trend", ...) |>
                private[["trend_echart"]]()
        },
        health_board_bar_data = function(
                health_board = private[["health_board_choices"]](),
                gender = private[["gender_choices"]]()
            ) {
            health_board <- arg_match(health_board, multiple = TRUE)
            gender <- arg_match(gender)
            self[["get_download"]]() |>
                filter(.data[["Sex"]] %in% gender) |>
                filter(.data[["HBName"]] %in% health_board) |>
                select("Date", "HBName", matches("^Ages\\d"), -matches("QF$")) |>
                pivot_longer(
                    -c("Date", "HBName"),
                    names_to = "Age",
                    values_to = "Population"
                ) |>
                private[["summarise_population"]](c("Date", "Age", "HBName")) |>
                private[["factor_age"]]() |>
                pivot_wider(names_from = "HBName", values_from = "Population") |>
                group_by(.data[["Date"]])
        },
        health_board_bar = function(...) {
            self[["plot_data"]](type = "health_board_bar", ...) |>
                private[["bar_echart"]]()
        },
        gp_trend_data = function(
                gp = private[["gp_choices"]](),
                gender = private[["gender_choices"]]()
            ) {
            gp <- arg_match(gp, multiple = TRUE)
            gender <- arg_match(gender)
            self[["get_download"]]() |>
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
                private[["trend_echart"]]()
        },
        gp_bar_data = function(
                gp = private[["gp_choices"]](),
                gender = private[["gender_choices"]]()
            ) {
            gp <- arg_match(gp, multiple = TRUE)
            gender <- arg_match(gender)
            self[["get_download"]]() |>
                filter(.data[["Sex"]] == gender) |>
                mutate(
                    ID = paste(
                        .data[["ID"]],
                        "-",
                        .data[["GPPracticeName"]]
                    )
                ) |>
                filter(.data[["ID"]] %in% gp) |>
                select("Date", "ID", matches("^Ages\\d"), -contains("QF")) |>
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
                private[["bar_echart"]]()
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
        #' Create UI for general practice group object.
        ui = function() {
            ns <- NS(self[["id"]]())
            data <- self[["get_download"]]()
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
                                card_header("National GP population trend"),
                                uiOutput(outputId = ns("national_pop_trend"))
                            ),
                            card(
                                full_screen = TRUE,
                                card_header("National GP population per gender and age group"),
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
                                    selected = unique(data[["HBName"]]),
                                    search = TRUE,
                                    html = TRUE,
                                    showSelectedOptionsFirst = TRUE,
                                    updateOn = "close"
                                ),
                                virtualSelectInput(
                                    inputId = ns("select_hb_gender"),
                                    label = "Select gender",
                                    choices = c("All", "Male", "Female"),
                                    selected = "All",
                                    search = TRUE,
                                    html = TRUE,
                                    showSelectedOptionsFirst = TRUE,
                                    updateOn = "close"
                                )
                            ),
                        ),
                        layout_column_wrap(
                            card(
                                full_screen = TRUE,
                                card_header("Health board GP population"),
                                uiOutput(outputId = ns("hb_pop_trend"))
                            ),
                            card(
                                full_screen = TRUE,
                                card_header("Health board GP population per age group"),
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
                                    choices = private[["gp_choices"]](),
                                    selected = private[["gp_choices"]]()[1],
                                    search = TRUE,
                                    html = TRUE,
                                    showSelectedOptionsFirst = TRUE,
                                    updateOn = "close"
                                ),
                                virtualSelectInput(
                                    inputId = ns("select_gp_gender"),
                                    label = "Select gender",
                                    choices = c("All", "Male", "Female"),
                                    selected = "All",
                                    search = TRUE,
                                    html = TRUE,
                                    showSelectedOptionsFirst = TRUE,
                                    updateOn = "close"
                                )
                            )
                        ),
                        layout_column_wrap(
                            card(
                                full_screen = TRUE,
                                card_header("GP population for selected practice and gender"),
                                uiOutput(outputId = ns("gp_pop_trend"))
                            ),
                            card(
                                full_screen = TRUE,
                                card_header("GP population for selected practice and gender per age group"),
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
                    data <- self[["get_download"]]()

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