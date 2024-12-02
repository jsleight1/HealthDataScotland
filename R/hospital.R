#' R6 class storing health statistics for a Hospital.
hospital <- R6Class("hospital",
    inherit = health_unit,
    private = list(
        title_col = function() {
            "HospitalName"
        },
        required_cols = function() {
            c("HospitalName", "FinancialYear", "SpecialtyName", "SpecialtyNameQF",
                "AllStaffedBeds", "AllStaffedBeds", "AverageAvailableStaffedBeds",
                "AverageOccupiedBeds", "Postcode")
        },
        specialty_data = function(specialties = "All Specialties") {
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
                    theme(axis.text.x = element_text(angle = 90)) +
                    xlab("Financial Year") +
                    ylab("Annually Available Staffed Beds")
            ggplotly(plot, tooltip = c("FinancialYear", "SpecialtyName", "AllStaffedBeds"))
        },
        specialty_line_data = function(
                data_type = c("annual", "daily"),
                ...
            ) {
            cols <- switch(arg_match(data_type),
                "annual" = private[["annual_cols"]](),
                "daily" = private[["daily_cols"]]()
            )
            private[["specialty_data"]](...) |>
                select(
                    "ID",
                    "FinancialYear",
                    "HospitalName",
                    "SpecialtyName",
                    all_of(cols)
                ) |>
                pivot_longer(cols = names(cols))
        },
        annual_cols = function() {
            c(
                "Annual number of available staffed beds" = "AllStaffedBeds",
                "Annual number of occupied beds" = "TotalOccupiedBeds"
            )
        },
        daily_cols = function() {
            c(
                "Daily average number of available staffed beds" = "AverageAvailableStaffedBeds",
                "Daily average number of occupied beds" = "AverageOccupiedBeds"
            )
        },
        specialty_line = function(...) {
            plot <- self[["plot_data"]]("specialty_line", ...) |>
                ggplot(
                    aes(
                        x = .data[["FinancialYear"]],
                        y = .data[["value"]],
                        color = .data[["name"]]
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
                    facet_wrap(~SpecialtyName, scales = "free_y", ncol = 1)
            ggplotly(plot, tooltip = c("FinancialYear", "Category", "group", "value")) |>
                plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
        },
        specialty_choices = function() {
            out <- self[["data"]]() |>
                filter(is.na(.data[["SpecialtyNameQF"]])) |>
                pull("SpecialtyName") |>
                unique()
            c("All Specialties", out)
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
        #' Plot hospital unit.
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
        #' Generate plot data for hospital unit.
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
                    box(title = "Address", self[["address"]](), width = 12),
                    box(title = "Health board", self[["health_board"]](), width = 12),
                    box(
                        title = "Annually Available Staffed Beds",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        selectInput(
                            ns("specialty_annual_select"),
                            label = "Select specialty",
                            choices = private[["specialty_choices"]](),
                            multiple = TRUE,
                            selected = private[["specialty_choices"]]()[1]
                        ),
                        spinner(plotlyOutput(ns("annual_beds")))
                    ),
                    box(
                        title = "Daily Average Available Staffed Beds",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        selectInput(
                            ns("specialty_daily_select"),
                            label = "Select specialty",
                            choices = private[["specialty_choices"]](),
                            multiple = TRUE,
                            selected = private[["specialty_choices"]]()[1]
                        ),
                        spinner(plotlyOutput(ns("daily_beds")))
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

                    output[["annual_beds"]] <- renderPlotly(
                        self[["plot"]](
                            type = "specialty_line",
                            data_type = "annual",
                            specialties = req(input[["specialty_annual_select"]])
                        )
                    )

                    output[["daily_beds"]] <- renderPlotly(
                        self[["plot"]](
                            type = "specialty_line",
                            data_type = "daily",
                            specialties = req(input[["specialty_daily_select"]])
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
