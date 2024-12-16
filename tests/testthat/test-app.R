library(shinytest2)

object_app <- function(x) {
    ui <- fluidPage(uiOutput("ui"))
    server <- function(input, output, session) {
        ns <- session[["ns"]]
        output[["ui"]] <- renderUI(
            x[["ui"]](ns)
        )
        x[["server"]]()
    }
    shinyApp(ui, server)
}

with_mocked_bindings(
    get_gp_meta = function() HealthDataScotland::example_gp_metadata,
    get_gp_data = function() HealthDataScotland::example_gp_data,
    get_hospital_meta = function() HealthDataScotland::example_hospital_metadata,
    get_hospital_data = function() HealthDataScotland::example_hospital_data,
    {
        gp_meta <- process_gp_meta()
        gp_data <- process_gp_data()
        hospital_meta <- process_hospital_meta()
        hospital_data <- process_hospital_data()
        data_objects <- create_data_objects(
            list(
                "General practice" = process_data(
                    "gp",
                    function() gp_meta,
                    function() gp_data,
                    process_gp_sf
                ),
                "Hospital" = process_data(
                    "hospital",
                    function() hospital_meta,
                    function() hospital_data,
                    process_hospital_sf
                )
            )
        )
    }
)

with_mocked_bindings(
    create_data_objects = function(x) data_objects,
    shiny_app <- health_data_scotland()
)

test_that("example works", {
    ui <- fluidPage(
        titlePanel("Hello Shiny!"),
        sidebarLayout(
            sidebarPanel(
                sliderInput(
                    inputId = "bins",
                    label = "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30
                )
            ),
            mainPanel(plotOutput(outputId = "distPlot"))
        )
    )

    server <- function(input, output) {
        output$distPlot <- renderPlot({
            x <- datasets::faithful$waiting
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            hist(x, breaks = bins, col = "#75AADB", border = "white",
                xlab = "Waiting time to next eruption (in mins)",
                main = "Histogram of waiting times")

        })
    }

    app <- shinyApp(ui, server)

    app <- AppDriver$new(app, name = "example", width = 800,
        height = 700, seed = 4323, load_timeout = 20 * 1000)

    app$expect_values()
    app$stop()
})
