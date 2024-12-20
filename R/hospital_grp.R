#' R6 class storing health statistics for a list of hospital health units.
hospital_grp <- R6Class("hospital_grp",
    inherit = health_unitgrp,
    private = list(
        specialty_data = function(hospitals = self[["ids"]](), ...) {
            private[["map_combine"]](
                    func = "plot_data",
                    type = "specialty_bar",
                    ...
                ) |>
                filter(.data[["ID"]] %in% hospitals)
        },
        specialty_bar = function(...) {
            dat <- self[["plot_data"]]("specialty_bar", ...)
            labels <- private[["id_name_labels"]](dat, "HospitalName")
            plot <- ggplot(
                dat,
                aes(
                    x = .data[["FinancialYear"]],
                    y = .data[["AllStaffedBeds"]],
                    fill = .data[["SpecialtyName"]],
                    ID = .data[["ID"]],
                    HospitalName = .data[["HospitalName"]]
                )
            ) +
                geom_bar(stat = "identity") +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90)) +
                facet_wrap(~.data[["ID"]], labeller = labeller(ID = labels))
            ggplotly(plot, tooltip = c("FinancialYear", "SpecialtyName",
                "AllStaffedBeds", "ID", "HospitalName"))
        },
        specialty_line_data = function(hospitals = self[["ids"]](), ...) {
            out <- private[["map_combine"]](
                    func = "plot_data",
                    type = "specialty_line",
                    ...
                ) |>
                filter(.data[["ID"]] %in% hospitals)
            assert_that(length(unique(out[["SpecialtyName"]])) == 1,
                msg = "`hospital_grp` line plots can only display one specialty"
            )
            out
        },
        specialty_line = function(...) {
            dat <- self[["plot_data"]]("specialty_line", ...)
            labels <- private[["id_name_labels"]](dat, "HospitalName")
            plot <- ggplot(
                dat,
                aes(
                    x = .data[["FinancialYear"]],
                    y = .data[["value"]],
                    color = .data[["name"]],
                    text = .data[["text"]]
                )
            ) +
                geom_point(aes(group = .data[["name"]])) +
                geom_line(aes(group = .data[["name"]])) +
                theme_bw() +
                theme(
                    axis.text.x = element_text(angle = 90),
                    legend.position = "bottom",
                    legend.title = element_blank()
                ) +
                labs(color = "Category") +
                xlab(NULL) +
                ylab(NULL) +
                facet_wrap(~.data[["ID"]], labeller = labeller(ID = labels))
            ggplotly(plot, tooltip = "text") |>
                plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
        },
        specialty_choices = function() {
            self[["data"]]() |>
                map(~.x[["data"]]()) |>
                map(pull, "SpecialtyName") |>
                unlist() |>
                unique() |>
                sort()
        }
    ),
    public = list(
        #' @description
        #' Get character vector of available plots for hospital grp. Options
        #'   are either "specialty_bar" or "specialty_line" plot.
        available_plots = function() {
            c("specialty_bar", "specialty_line")
        },
        #' @description
        #' Plot hospital grp.
        #' @param type (character(1))\cr
        #'     Character specifying plot type. See `available_plots` for options.
        #' @param ... Passed to plot functions.
        plot = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type,
                "specialty_bar" = private[["specialty_bar"]],
                "specialty_line" = private[["specialty_line"]]
            )(...)
        },
        #' @description
        #' Generate plot data for hospital grp.
        #' @param type (character(1))\cr
        #'     Character specifying plot type. See `available_plots` for options.
        #' @param ... Passed to plot functions.
        plot_data = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type,
                "specialty_bar" = private[["specialty_data"]],
                "specialty_line" = private[["specialty_line_data"]]
            )(...)
        },
        #' @description
        #' Summarise hospital grp unit.
        #' @param id (character(1))\cr
        #'     Character specifying the ID to assign to output data.frame.
        #' @param ... Passed to plot_data.
        summary = function(id = "Scotland national", ...) {
            self[["plot_data"]]("specialty_bar", ...) |>
                mutate(ID = id) |>
                select("ID", "FinancialYear", "SpecialtyName", "AllStaffedBeds") |>
                group_by_if(is.character) |>
                summarise_if(is.numeric, sum, na.rm = TRUE) |>
                ungroup()
        },
        #' @description
        #' Create UI for hospital group object.
        #' @param ns
        #'     Namespace of shiny application page.
        ui = function(ns) {
            ns <- NS(ns(self[["id"]]()))
            box(
                title = "Hospital",
                box(
                    title = "Annually Available Staffed Beds",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    pickerInput(
                        inputId = ns("hospital_annual_select"),
                        label = "Select hospitals",
                        choices = private[["id_name_selection"]](),
                        selected = private[["id_name_selection"]](),
                        inline = TRUE,
                        multiple = TRUE,
                        options = list(
                            `actions-box` = TRUE,
                            `selected-text-format` = "count > 1"
                        )
                    ),
                    selectInput(
                        ns("specialty_annual_select"),
                        label = "Select specialty",
                        choices = private[["specialty_choices"]](),
                        selected = "All Specialties"
                    ),
                    spinner(plotlyOutput(ns("annual_beds")))
                ),
                box(
                    title = "Daily Average Available Staffed Beds",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    pickerInput(
                        inputId = ns("hospital_daily_select"),
                        label = "Select hospitals",
                        choices = private[["id_name_selection"]](),
                        selected = private[["id_name_selection"]](),
                        inline = TRUE,
                        multiple = TRUE,
                        options = list(
                            `actions-box` = TRUE,
                            `selected-text-format` = "count > 1"
                        )
                    ),
                    selectInput(
                        ns("specialty_daily_select"),
                        label = "Select specialty",
                        choices = private[["specialty_choices"]](),
                        selected = "All Specialties"
                    ),
                    spinner(plotlyOutput(ns("daily_beds")))
                ),
                downloadButton(ns("download")),
                width = 12,
                status = "primary",
                solidHeader = TRUE
            )
        },
        #' @description
        #' Create server for hospital group object.
        server = function() {
            moduleServer(
                self[["id"]](),
                function(input, output, session) {
                    ns <- session[["ns"]]
                    output[["annual_beds"]] <- renderPlotly(
                        self[["plot"]](
                            type = "specialty_line",
                            data_type = "annual",
                            specialties = req(input[["specialty_annual_select"]]),
                            hospitals = req(input[["hospital_annual_select"]])
                        )
                    )
                    output[["daily_beds"]] <- renderPlotly(
                        self[["plot"]](
                            type = "specialty_line",
                            data_type = "daily",
                            specialties = req(input[["specialty_daily_select"]]),
                            hospitals = req(input[["hospital_daily_select"]])
                        )
                    )
                    output[["download"]] <- self[["download_handler"]]()
                }
            )
        }
    )
)
