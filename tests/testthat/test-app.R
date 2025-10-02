library(shinytest2)

object_ui <- function(x) {
  with_mocked_bindings(
    # Stub virtual select due to random values when rendered
    virtual_select_input = function(...) "",
    ui <- fluidPage(x[["ui"]](function(x) "ui"))
  )
  server <- function(input, output, session) {}
  shinyApp(ui, server)
}

with_mocked_bindings(
  get_gp_meta = function() HealthDataScotland::example_gp_metadata,
  get_gp_data = function() HealthDataScotland::example_gp_data,
  get_hospital_meta = function() HealthDataScotland::example_hospital_metadata,
  get_hospital_data = function() HealthDataScotland::example_hospital_data,
  data <- create_processed_data()
)

data_objects <- create_data_objects(data)

with_mocked_bindings(
  virtual_select_input = function(...) "",
  load_processed_data = function(...) data,
  create_data_objects = function(x) data_objects,
  shiny_app <- health_data_scotland()
)

test_that("health_data_scotland app initial values works", {
  skip_on_cran()

  app <- AppDriver$new(shiny_app,
    name = "initial", width = 800, height = 700,
    seed = 4323, load_timeout = 40 * 1000
  )

  app$expect_values()
  app$expect_unique_names()
  app$stop()
})

test_that("gp ui/server works", {
  skip_on_cran()
  gp_unit <- example_gp_unit()

  app <- object_ui(gp_unit)
  app <- AppDriver$new(app,
    name = "gp_object", width = 800,
    height = 700, seed = 4323, load_timeout = 40 * 1000
  )

  app$expect_values()
  app$stop()
})

test_that("gp_grp ui/server works", {
  skip_on_cran()
  gp_grp_unit <- example_gp_grp_unit()

  app <- object_ui(gp_grp_unit)
  app <- AppDriver$new(app,
    name = "gp_grp_object", width = 800,
    height = 700, seed = 4323, load_timeout = 40 * 1000
  )

  app$expect_values()
  app$stop()
})

test_that("hospital ui/server works", {
  skip_on_cran()
  hospital_unit <- example_hospital_unit()

  app <- object_ui(hospital_unit)
  app <- AppDriver$new(app,
    name = "hospital_object", width = 800,
    height = 700, seed = 4323, load_timeout = 40 * 1000
  )

  app$expect_values()
  app$stop()
})

test_that("hospital_grp ui/server works", {
  skip_on_cran()
  hospital_grp_unit <- example_hospital_grp_unit()

  app <- object_ui(hospital_grp_unit)
  app <- AppDriver$new(app,
    name = "hospital_grp_object", width = 800,
    height = 700, seed = 4323, load_timeout = 40 * 1000
  )

  app$expect_values()
  app$stop()
})

test_that("map ui/server works", {
  skip_on_cran()
  map_unit <- example_map_unit()

  app <- object_ui(map_unit)
  app <- AppDriver$new(app,
    name = "map_object", width = 800,
    height = 700, seed = 4323, load_timeout = 40 * 1000
  )

  app$expect_values()
  app$stop()
})
