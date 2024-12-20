library(shinytest2)

object_ui <- function(x) {
    ui <- fluidPage(x[["ui"]](function(x) "ui"))
    server <- function(input, output, session) {}
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

test_that("health_data_scotland app initial values works", {
    skip_on_cran()

    app <- AppDriver$new(shiny_app, name = "initial", width = 800, height = 700,
        seed = 4323, load_timeout = 20 * 1000)

    app$expect_values()
    app$expect_unique_names()
    app$stop()
})

test_that("comparison tab works with no selected data", {
    skip_on_cran()

    app <- AppDriver$new(shiny_app, name = "empty_comparison_tab", width = 800,
        height = 700, seed = 4323, load_timeout = 20 * 1000)

    app$set_inputs(map_tabs = "Comparison")
    app$expect_values()
    app$expect_unique_names()
    app$stop()
})

test_that("downloads page works", {
    skip_on_cran()

    app <- AppDriver$new(shiny_app, name = "downloads_page", width = 800,
        height = 700, seed = 4323, load_timeout = 20 * 1000)

    app$set_inputs(sidebar = "download")
    app$expect_values()
    app$stop()
})

test_that("references page works", {
    skip_on_cran()

    app <- AppDriver$new(shiny_app, name = "references_page", width = 800,
        height = 700, seed = 4323, load_timeout = 20 * 1000)

    app$set_inputs(sidebar = "references")
    app$expect_values()
    app$stop()
})

test_that("gp ui/server works", {
    skip_on_cran()
    gp_unit <- data_objects[["General practice"]][["data"]]()[[1]]

    app <- object_ui(gp_unit)
    app <- AppDriver$new(app, name = "gp_object", width = 800,
        height = 700, seed = 4323, load_timeout = 20 * 1000)

    app$expect_values()
    app$stop()
})

test_that("gp_grp ui/server works", {
    skip_on_cran()
    gp_grp_unit <- data_objects[["General practice"]][["subset"]](
        c("10002", "10017", "10036")
    )

    app <- object_ui(gp_grp_unit)
    app <- AppDriver$new(app, name = "gp_grp_object", width = 800,
        height = 700, seed = 4323, load_timeout = 20 * 1000)

    app$expect_values()
    app$stop()
})

test_that("hospital ui/server works", {
    skip_on_cran()
    hospital_unit <- data_objects[["Hospital"]][["data"]]()[[1]]

    app <- object_ui(hospital_unit)
    app <- AppDriver$new(app, name = "hopsital_object", width = 800,
        height = 700, seed = 4323, load_timeout = 20 * 1000)

    app$expect_values()
    app$stop()
})

test_that("hospital_grp ui/server works", {
    skip_on_cran()
    hospital_grp_unit <- data_objects[["Hospital"]][["subset"]](
        c("A101H", "A103H", "A110H")
    )

    app <- object_ui(hospital_grp_unit)
    app <- AppDriver$new(app, name = "hospital_grp_object", width = 800,
        height = 700, seed = 4323, load_timeout = 20 * 1000)

    app$expect_values()
    app$stop()
})