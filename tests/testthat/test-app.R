library(shinytest2)

with_mocked_bindings(
    get_gp_meta = function() HealthDataScotland::example_gp_metadata,
    gp_meta <- process_gp_meta()
)
with_mocked_bindings(
    get_gp_data = function() HealthDataScotland::example_gp_data,
    gp_data <- process_gp_data()
)

with_mocked_bindings(
    get_hosp_meta = function() HealthDataScotland::example_hospital_metadata,
    hosp_meta <- process_hospital_meta()
)
with_mocked_bindings(
    get_hosp_data = function() HealthDataScotland::example_hospital_data,
    hosp_data <- process_hospital_data()
)

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
            function() hosp_meta, 
            function() hosp_data,
            process_hospital_sf
        )
    )
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
