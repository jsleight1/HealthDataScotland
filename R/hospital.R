#' R6 class storing health statistics for a Hospital.
hospital <- R6Class("hospital", 
    inherit = health_unit,
    private = list(
        title_col = function() {
            "HospitalName"
        },
        required_cols = function() {
            c("HospitalName", "FinancialYear", "SpecialtyName", "SpecialtyNameQF",
                "AllStaffedBeds", "Postcode")
        },
        specialty_bar_data = function(specialties = "All Specialties") {
            self[["data"]]() |> 
                filter(.data[["SpecialtyName"]] %in% specialties)
        },
        specialty_bar = function(...) {
            plot <- self[["plot_data"]]("specialty_bar", ...) |>
                ggplot(
                    aes(
                        x = .data[["FinancialYear"]], 
                        y = .data[["AllStaffedBeds"]], 
                        fill = .data[["SpecialtyName"]]
                    )
                ) + 
                    geom_bar(stat = "identity") + 
                    theme_bw() + 
                    theme(axis.text.x = element_text(angle = 90))
            ggplotly(plot, tooltip = c("FinancialYear", "SpecialtyName", "AllStaffedBeds"))
        },
        specialty_choices = function() {
            self[["data"]]() |> 
                filter(is.na(.data[["SpecialtyNameQF"]])) |> 
                pull("SpecialtyName") |> 
                unique()
        }
    ),
    public = list(
        #' @description
        #' Get character vector of available plots for hospital unit. Options 
        #'   are either "specialty_bar" plot.
        available_plots = function() {
            c("specialty_bar")
        },
        #' @description
        #' Plot hospital unit.
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
        #' Generate plot data for hospital unit.
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
        #' Create UI for hospital object.
        #' @param ns 
        #'     Namespace of shiny application page.
        ui = function(ns) {
            ns <- NS(ns(self[["ID"]]()))
            fluidRow(
                box(
                    title = paste(self[["title"]](), "-", self[["ID"]]()),
                    width = 12, 
                    status = "primary",
                    solidHeader = TRUE,
                    fluidRow(box(title = "Address", self[["address"]](), width = 12)),
                    fluidRow(box(title = "Health board", self[["health_board"]](), width = 12)),
                    fluidRow(
                        box(
                            title = "All specialties",
                            spinner(plotlyOutput(ns("all_specialty"))),
                            width = 12
                        ), 
                    ),
                    fluidRow(
                        box(
                            title = "Selected specialties",
                            selectInput(
                                ns("specialty_select"), 
                                label = "Select specialty", 
                                choices = private[["specialty_choices"]](),
                                multiple = TRUE, 
                                selected = private[["specialty_choices"]]()[1]
                            ),
                            spinner(plotlyOutput(ns("selected_specialties"))),
                            width = 12
                        )
                    ),
                    downloadButton(ns("download"))
                )
            )
        }, 
        #' @description
        #' Create server for hospital object.
        server = function() {
            moduleServer(
                self[["ID"]](),
                function(input, output, session) {
                    ns <- session[["ns"]]
                    output[["all_specialty"]] <- renderPlotly(
                        self[["plot"]](type = "specialty_bar")
                    )
                    output[["selected_specialties"]] <- renderPlotly(
                        self[["plot"]](
                            type = "specialty_bar",
                            specialties = req(input[["specialty_select"]])
                        )
                    )
                    output[["download"]] <- downloadHandler(
                        filename = function() "hospital_data.csv",
                        content = function(con) {
                            write.csv(self[["data"]](), con)
                        }
                    )
                }
            )
        }
    )
)
