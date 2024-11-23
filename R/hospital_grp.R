#' R6 class storing health statistics for a list of hospital health units.
hospital_grp <- R6Class("hospital_grp", 
    inherit = health_unitgrp, 
    private = list(
        specialty_bar_data = function(specialties = "All Specialties") {
            self[["data"]]() |> 
                map(~.x[["plot_data"]]("specialty_bar", specialties)) |> 
                setNames(self[["titles"]]()) |> 
                bind_rows(.id = "ID")
        },
        specialty_bar = function(specialties = "All Specialties", ...) {
            plot <- self[["plot_data"]]("specialty_bar", specialties, ...) |>
                ggplot(
                    aes(
                        x = .data[["FinancialYear"]], 
                        y = .data[["AllStaffedBeds"]], 
                        fill = .data[["SpecialtyName"]], 
                        ID = .data[["ID"]]
                    )
                ) + 
                    geom_bar(stat = "identity") + 
                    theme_bw() + 
                    theme(axis.text.x = element_text(angle = 90)) +
                    facet_wrap(~ID, ncol = 2)
            ggplotly(plot, tooltip = c("FinancialYear", "SpecialtyName", 
                "AllStaffedBeds", "ID"))
        },
        specialty_choices = function() {
            self[["data"]]() |> 
                map(~filter(.x[["data"]](), is.na(.data[["SpecialtyNameQF"]]))) |> 
                bind_rows() |>
                pull("SpecialtyName") |> 
                unique()
        }
    ),
    public = list(
        #' @description
        #' Get character vector of available plots for hospital grp. Options 
        #'   are either "specialty_bar" plot.
        available_plots = function() {
            c("specialty_bar")
        },
        #' @description
        #' Plot hospital grp.
        #' @param type (character(1))\cr
        #'     Character specifying plot type. See `available_plots` for options.
        #' @param ... Passed to plot functions.
        plot = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type, 
                "specialty_bar" = private[["specialty_bar"]](...)
            )
        }, 
        #' @description
        #' Generate plot data for hospital grp.
        #' @param type (character(1))\cr
        #'     Character specifying plot type. See `available_plots` for options.
        #' @param ... Passed to plot functions.
        plot_data = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type, 
                "specialty_bar" = private[["specialty_bar_data"]](...)
            )
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
                    title = "All specialties",
                    spinner(plotlyOutput(outputId = ns("all_specialty"))),
                    width = 12
                ),
                box(
                    title = "Selected specialties",
                    selectInput(
                        inputId = ns("specialty_select"), 
                        label = "Select specialty", 
                        choices = private[["specialty_choices"]](),
                        multiple = TRUE, 
                        selected = private[["specialty_choices"]]()[1]
                    ),
                    spinner(plotlyOutput(outputId = ns("selected_specialties"))),
                    width = 12
                ),
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
                    output[["all_specialty"]] <- renderPlotly(
                        self[["plot"]](type = "specialty_bar")
                    )
                    output[["selected_specialties"]] <- renderPlotly({
                        self[["plot"]](
                            type = "specialty_bar",
                            specialties = req(input[["specialty_select"]])
                        )
                    })
                }
            )
        }
    )
)
