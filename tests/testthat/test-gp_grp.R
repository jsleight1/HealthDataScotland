gp_unit <- example_gp_unit(id = "10002")
gp_unit2 <- example_gp_unit(id = "10017")
gp_grp_unit <- example_gp_grp_unit()

test_that("gp_grp class works", {
  list(gp_unit, "gp_unit") |>
    gp_grp[["new"]](.id = "gp") |>
    expect_error("group must contain the same class of health units")

  list(gp_unit, gp_unit2) |>
    gp_grp[["new"]](.id = 1) |>
    expect_error("ID must be character of length 1")

  list(gp_unit, gp_unit) |>
    gp_grp[["new"]](.id = "gp") |>
    expect_error("Health units must not be duplicated")

  output <- list(gp_unit, gp_unit2) |>
    gp_grp[["new"]](.id = "gp") |>
    expect_no_error()

  expect_true(inherits(output, "gp_grp"))
  expect_identical(output[["ID"]](), "gp")
  expect_identical(output[["IDs"]](), c("10002", "10017"))
  expect_identical(output[["titles"]](), c("Muirhead Medical Centre", "The Blue Practice"))
  expect_identical(output[["data"]](), list(gp_unit, gp_unit2))
  expect_identical(output[["health_unit"]]("10002"), gp_unit)
  expect_identical(
    output[["available_plots"]](),
    c(
      "national_trend", "national_pyramid", "health_board_trend",
      "health_board_bar", "gp_trend", "gp_bar"
    )
  )
})

test_that("gp_grp class can be plotted", {
  for (plt in gp_grp_unit[["available_plots"]]()) {
    output <- gp_grp_unit[["plot"]](type = plt) |>
      expect_no_error()
    expect_s3_class(output, "echarts4r")
  }
})

test_that("gp_grp plot info works", {
  for (plt in gp_grp_unit[["available_plots"]]()) {
    gp_grp_unit[["plot_info"]](type = plt) |>
      expect_snapshot()
  }
})

test_that("gp_grp plot functions error if wrong type", {
  gp_grp_unit[["plot"]](type = "p") |>
    expect_error("`type` must be one.+")
  gp_grp_unit[["plot_data"]](type = "p") |>
    expect_error("`type` must be one.+")
  gp_grp_unit[["plot_info"]](type = "p") |>
    expect_error("`type` must be one.+")
})

test_that("national_trend_data works", {
  output <- gp_grp_unit[["plot_data"]](type = "national_trend") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "national_trend_data")
})

test_that("national_pyramid_data works", {
  output <- gp_grp_unit[["plot_data"]](type = "national_pyramid") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "national_pyramid_data")
})

test_that("health_board_trend_data works", {
  output <- gp_grp_unit[["plot_data"]](type = "health_board_trend") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "health_board_trend_data")
})

test_that("health_board_bar_data works", {
  output <- gp_grp_unit[["plot_data"]](type = "health_board_bar") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "health_board_bar_data")
})

test_that("gp_trend_data works", {
  output <- gp_grp_unit[["plot_data"]](type = "gp_trend") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "gp_trend_data")
})

test_that("gp_bar_data works", {
  output <- gp_grp_unit[["plot_data"]](type = "gp_bar") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "gp_bar_data")
})

test_that("gp_grp subset works", {
  gp_grp_unit[["subset"]]("ID") |>
    expect_error("IDs are not found in health unit group")

  output <- gp_grp_unit[["subset"]]("10002") |>
    expect_no_error()

  expect_true(inherits(output, "gp_grp"))
  expect_identical(output[["IDs"]](), "10002")
})

test_that("gp_grp combine_data works", {
  output <- gp_grp_unit[["combine_data"]]() |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_identical(gp_grp_unit[["IDs"]](), unique(output[["ID"]]))
})
