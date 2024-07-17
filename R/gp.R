

gp <- R6Class("gp", 
    private = list(
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
                tidyr::pivot_longer(-"Gender", names_to = "Age", values_to = "Population") %>% 
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
            plotly::ggplotly(plot, tooltip = c("Gender", "Age", "Population"))
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
            plotly::ggplotly(plot, tooltip = c("Time", "Gender", "Population"))
        }
    ),
    public = list(
        data = NA,
        initialize = function(data) {
            self[["data"]] = data
            self[["validate"]]()
        }, 
        validate = function() {
            assert_that(inherits(self[["data"]], "data.frame"), 
                msg = "Data set must be in data.frame")
            col_check <- private[["required_cols"]]() %in% colnames(self[["data"]])
            assert_that(all(col_check), 
                msg = paste(
                    paste(private[["required_cols"]]()[!col_check], collapse = ", "), 
                    "column missing from data"
                )
            )
            assert_that(length(unique(self[["data"]][["PracticeCode"]])) == 1, 
                msg = "Data set must contain only one unique PracticeCode")
            self
        },
        id = function() {
            unique(self[["data"]][["PracticeCode"]])
        },
        title = function() {
            unique(self[["data"]][["GPPracticeName"]])
        },
        address = function() {
            self[["data"]] %>% 
                select(contains("Address"), "Postcode") %>% 
                distinct() %>% 
                tidyr::unite(col = "x", sep = ", ") %>% 
                pull("x")
        }, 
        available_plots = function() {
            c("population_pyramid", "population_trend")
        },
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