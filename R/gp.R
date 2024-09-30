
#' R6 class storing health statistics for a GP practice.
gp <- R6Class("gp", 
    inherit = health_unit,
    private = list(
        id_col = function() {
            "PracticeCode"
        },
        title_col = function() {
            "GPPracticeName"
        },
        required_cols = function() {
            c("PracticeCode", "GPPracticeName", "PracticeListSize", 
                "AddressLine1", "AddressLine2", "AddressLine3", "AddressLine4", 
                "Postcode", "TelephoneNumber", "PracticeType", "GPCluster", 
                "Time")
        },
        population_pyramid = function(time) {
            dat <- self[["data"]] %>% 
                filter(Time == time, Sex != "All") %>% 
                select("Gender" = "Sex", matches("Ages"), -matches("QF")) %>% 
                pivot_longer(-"Gender", names_to = "Age", values_to = "Population") %>% 
                filter(Age != "AllAges") %>% 
                mutate(Age = factor(Age, levels = c(
                    "Ages85plus",
                    "Ages75to84",
                    "Ages65to74",
                    "Ages45to64",
                    "Ages25to44",
                    "Ages15to24",
                    "Ages5to14", 
                    "Ages0to4"
                )))

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
        population_trend = function(gender = "All") {
            plot <- self[["data"]] %>% 
                select("Time", "Date", "Gender" = "Sex", "Population" = "AllAges") %>% 
                distinct() %>% 
                filter(Gender == gender) %>%
                ggplot(aes(x = Time, y = Population, group = Gender)) + 
                    geom_line() + 
                    theme_bw() + 
                    theme(axis.text.x = element_text(angle = 90))
            ggplotly(plot, tooltip = c("Time", "Gender", "Population"))
        }
    ),
    public = list(
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
        }
    )
)

gp_UI <- function(x, ns) {
    ns <- NS(ns(x[["id"]]()))
    fluidRow(
        box(
            title = x[["title"]](), 
            width = 12, 
            status = "primary",
            solidHeader = TRUE,
            fluidRow(box(title = "Address", x[["address"]](), width = 12)),
            fluidRow(
                box(
                    title = "Population pyramid",
                    selectInput(
                        ns("pop_pyramid_select"), 
                        label = "Select time frame",
                        choices = unique(x[["data"]][["Time"]])
                    ),
                    plotlyOutput(ns("pop_pyramid")),
                    width = 6
                ), 
                box(
                    title = "Population trend",
                    selectInput(
                        ns("pop_trend_select"), 
                        label = "Select gender", 
                        choices = unique(x[["data"]][["Sex"]]), 
                        selected = "All"
                    ),
                    plotlyOutput(ns("pop_trend")),
                    width = 6 
                )
            )
        )
    )

}

gp_server <- function(x) {
    moduleServer(
        x[["id"]](),
        function(input, output, session) {
            ns <- session[["ns"]]

            output[["pop_pyramid"]] <- renderPlotly(
                x[["plot"]](
                    type = "population_pyramid", 
                    time = req(input[["pop_pyramid_select"]])
                )
            )

            output[["pop_trend"]] <- renderPlotly(
                x[["plot"]](
                    type = "population_trend", 
                    gender = req(input[["pop_trend_select"]])
                )
            )

        }
    )
}

initialise_gp_popup <- function(meta, data, event, ns) {
    obj <- meta %>% 
        filter(.data[["PracticeCode"]] == event[["id"]]) %>% 
        select(-c("HB", "HSCP", "Time")) %>% 
        inner_join(data, by = "PracticeCode") %>% 
        gp[["new"]]()

    showModal(modalDialog(
        gp_UI(obj, ns),
        size = "l",
        easyClose = TRUE
    ))
    gp_server(obj)
}