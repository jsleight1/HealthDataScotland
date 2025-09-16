gp_unit <- example_gp_unit()

test_that("gp class works", {
  metadata <- gp_unit[["metadata"]]()
  data <- gp_unit[["data"]]()

  gp[["new"]]("metadata", "data") |>
    expect_error("Metadata set must be in data.frame")

  gp[["new"]](metadata, "data") |>
    expect_error("Data set must be in data.frame")

  gp[["new"]](bind_rows(metadata, metadata), data) |>
    expect_error("Metadata set must contain only one unique ID")

  metadata |>
    select(-"GPPracticeName") |>
    gp[["new"]](data) |>
    expect_error("GPPracticeName column missing from metadata")

  gp[["new"]](metadata, select(data, -"Date")) |>
    expect_error("Date column missing from data")

  output <- gp[["new"]](metadata, data) |>
    expect_no_error()

  expect_true(inherits(output, "gp"))
  expect_identical(output[["ID"]](), "10002")
  expect_identical(output[["health_board"]](), "S08000030")
  expect_identical(output[["telephone"]](), "01382   580   264")
  expect_identical(output[["title"]](), "Muirhead Medical Centre")
  expect_identical(
    output[["address"]](),
    "Muirhead Medical Centre, Liff Road, Muirhead, DD2 5NH"
  )
  expect_identical(
    output[["available_plots"]](),
    c("population_pyramid", "population_trend")
  )
})

test_that("combine_data works", {
  output <- gp_unit[["combine_data"]]() |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "combine_data")
})

test_that("gp class can be plotted", {
  for (plt in gp_unit[["available_plots"]]()) {
    output <- gp_unit[["plot"]](type = plt) |>
      expect_no_error()
    expect_s3_class(output, "echarts4r")
  }
})

test_that("gp plot info works", {
  for (plt in gp_unit[["available_plots"]]()) {
    gp_unit[["plot_info"]](type = plt) |>
      expect_snapshot()
  }
})

test_that("gp plot functions error if wrong type", {
  gp_unit[["plot"]](type = "p") |>
    expect_error("`type` must be one.+")
  gp_unit[["plot_data"]](type = "p") |>
    expect_error("`type` must be one.+")
  gp_unit[["plot_info"]](type = "p") |>
    expect_error("`type` must be one.+")
})

test_that("population_pyramid_data works", {
  output <- gp_unit[["plot_data"]](type = "population_pyramid") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "population_pyramid_data")
})

test_that("population_trend_data works", {
  output <- gp_unit[["plot_data"]](type = "population_trend") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "population_trend_data")
})

test_that("gp ui works", {
  output <- gp_unit[["ui"]](function(i) "ns") |>
    expect_no_error()
  expect_s3_class(output, "shiny.tag")
})
