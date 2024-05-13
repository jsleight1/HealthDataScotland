

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
                ylab(NULL)
            plotly::ggplotly(plot, tooltip = c("Gender", "Age", "Population"))
        }
    ),
    public = list(
        data = NA,
        initialize = function(data) {
            self[["data"]] = data
            self[["validate"]]()
        }, 
        validate = function() {
            assert_that(inherits(self[["data"]], "data.frame"))
            assert_that(all(private[["required_cols"]]() %in% colnames(self[["data"]])))
            assert_that(length(unique(self[["data"]][["PracticeCode"]])) == 1)
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
            c("population_pyramid")
        },
        plot = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type, 
                "population_pyramid" = private[["population_pyramid"]](...)
            )
        }
    )
)

gpUI <- function(x, ns) {
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
                        ns("pyramid_select"), 
                        label = "Select time frame",
                        choices = unique(x[["data"]][["Time"]])
                    ),
                    plotlyOutput(ns("pyramid"), height = "700px"),
                    width = 12
                )
            )
        )
    )

}

gpServer <- function(x) {
    moduleServer(
        x[["id"]](),
        function(input, output, session) {
            ns <- session[["ns"]]

            output[["pyramid"]] <- renderPlotly({
                x[["plot"]](
                    type = "population_pyramid", 
                    time = req(input[["pyramid_select"]])
                )
            })

        }
    )
}