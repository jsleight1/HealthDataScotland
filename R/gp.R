
#' R6 class storing health statistics for a GP practice.
gp <- R6Class("gp", 
    inherit = health_unit,
    private = list(
        title_col = function() {
            "GPPracticeName"
        },
        required_cols = function() {
            c("GPPracticeName", "PracticeListSize", 
                "AddressLine1", "AddressLine2", "AddressLine3", "AddressLine4", 
                "Postcode", "TelephoneNumber", "PracticeType", "GPCluster", 
                "Date")
        },
        population_pyramid_data = function(date) {
            self[["data"]]() %>% 
                filter(.data[["Date"]] == date, .data[["Sex"]] != "All") %>%
                select("Gender" = "Sex", matches("Ages\\d"), -contains("QF"), -"AllAges") %>%
                pivot_longer(-"Gender", names_to = "Age", values_to = "Population") %>% 
                mutate(Age = factor(.data[["Age"]], levels = c(
                    "Ages85plus",
                    "Ages75to84",
                    "Ages65to74",
                    "Ages45to64",
                    "Ages25to44",
                    "Ages15to24",
                    "Ages5to14", 
                    "Ages0to4"
                )))
        },
        population_pyramid = function(date, ...) {
            dat <- self[["plot_data"]]("population_pyramid", date, ...)
            plot <- ggplot(dat, aes(Population = Population)) +
                geom_bar(
                    aes(
                        x = Age, 
                        fill = Gender, 
                        y = ifelse(Gender == "Male", -Population,  Population)
                    ), 
                    stat = "identity"
                ) +
                scale_y_continuous(
                    labels = abs, 
                    limits = max(dat$Population) * c(-1,1)
                ) + 
                coord_flip() + 
                theme_bw() +
                ylab(NULL) + 
                xlab(NULL)
            ggplotly(plot, tooltip = c("Gender", "Age", "Population"))
        }, 
        population_trend_data = function(gender = "All") {
            self[["data"]]() %>% 
                select("Date", "Gender" = "Sex", "Population" = "AllAges") %>% 
                distinct() %>% 
                filter(Gender == gender)
        },
        population_trend = function(gender = "All", ...) {
            plot <- dat <- self[["plot_data"]]("population_trend", gender, ...) %>%
                ggplot(aes(x = Date, y = Population, group = Gender)) + 
                    geom_line() + 
                    theme_bw() + 
                    theme(axis.text.x = element_text(angle = 90))
            ggplotly(plot, tooltip = c("Date", "Gender", "Population"))
        }
    ),
    public = list(
        #' @description
        #' Get telephone number of GP practice.
        telephone = function() {
            unique(self[["data"]]()[["TelephoneNumber"]])
        },
        #' @description
        #' Get character vector of available plots for gp unit. Options 
        #'   are either "population_pyramid" plot or "population_trend" plot.
        available_plots = function() {
            c("population_pyramid", "population_trend")
        },
        #' @description
        #' Plot gp unit.
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
        #' Generate plot data for gp unit.
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
        #' Create UI for general practice object.
        #' @param ns 
        #'     Namespace of shiny application page.
        ui = function(ns) {
            ns <- NS(ns(self[["id"]]()))
            fluidRow(
                box(
                    title = paste(self[["title"]](), "-", self[["id"]]()), 
                    width = 12, 
                    status = "primary",
                    solidHeader = TRUE,
                    fluidRow(box(title = "Address", self[["address"]](), width = 12)),
                    fluidRow(box(title = "Telephone", self[["telephone"]](), width = 12)),
                    fluidRow(box(title = "Health board", self[["health_board"]](), width = 12)),
                    fluidRow(
                        box(
                            title = "Population pyramid",
                            selectInput(
                                ns("pop_pyramid_select"), 
                                label = "Select time frame",
                                choices = unique(self[["data"]]()[["Date"]])
                            ),
                            plotlyOutput(ns("pop_pyramid")),
                            width = 6
                        ), 
                        box(
                            title = "Population trend",
                            selectInput(
                                ns("pop_trend_select"), 
                                label = "Select gender", 
                                choices = unique(self[["data"]]()[["Sex"]]), 
                                selected = "All"
                            ),
                            plotlyOutput(ns("pop_trend")),
                            width = 6 
                        )
                    )
                )
            )
        }, 
        #' @description
        #' Create server for general practice object.
        server = function() {
            moduleServer(
                self[["id"]](),
                function(input, output, session) {
                    ns <- session[["ns"]]
                    output[["pop_pyramid"]] <- renderPlotly(
                        self[["plot"]](
                            type = "population_pyramid", 
                            date = req(input[["pop_pyramid_select"]])
                        )
                    )
                    output[["pop_trend"]] <- renderPlotly(
                        self[["plot"]](
                            type = "population_trend", 
                            gender = req(input[["pop_trend_select"]])
                        )
                    )

                }
            )
        }
    )
)
