hosp_unit <- example_hospital_unit(id = "A101H")
hosp_unit2 <- example_hospital_unit(id = "A201H")
hosp_grp_unit <- example_hospital_grp_unit()
sf <- get_sf("hospital")
sf <- sf[sf[["ID"]] %in% c("A101H", "A201H"), ]

test_that("hospital_grp class works", {
  list(hosp_unit, "hosp_unit") |>
    hospital_grp[["new"]](.sf = sf, .id = "hospital") |>
    expect_error("group must contain the same class of health units")

  list(hosp_unit, hosp_unit2) |>
    hospital_grp[["new"]](.sf = "sf", .id = "hospital") |>
    expect_error("sf must be sf object")

  tst_sf <- sf[sf[["ID"]] == "A101H", ]

  list(hosp_unit2) |>
    hospital_grp[["new"]](.sf = tst_sf, .id = "hospital") |>
    expect_error("Are all ids in map sf found in object")

  output <- list(hosp_unit, hosp_unit2) |>
    hospital_grp[["new"]](.sf = sf, .id = 1) |>
    expect_error("ID must be character of length 1")

  list(hosp_unit, hosp_unit) |>
    hospital_grp[["new"]](.sf = bind_rows(tst_sf, tst_sf), .id = "hospital") |>
    expect_error("Health units must not be duplicated")

  output <- list(hosp_unit, hosp_unit2) |>
    hospital_grp[["new"]](.sf = sf, .id = "hospital") |>
    expect_no_error()

  expect_true(inherits(output, "hospital_grp"))
  expect_identical(output[["id"]](), "hospital")
  expect_identical(output[["ids"]](), c("A101H", "A201H"))
  expect_identical(output[["titles"]](), c("Arran War Memorial Hospital", "Ailsa Hospital"))
  expect_identical(output[["data"]](), list(hosp_unit, hosp_unit2))
  expect_identical(output[["sf"]](), sf)
  expect_identical(output[["health_unit"]]("A101H"), hosp_unit)
  expect_identical(
    output[["available_plots"]](),
    c(
      "national_trend", "health_board_trend", "health_board_bar",
      "hospital_trend", "hospital_bar"
    )
  )
})

test_that("hospital_grp class can be plotted", {
  for (plt in hosp_grp_unit[["available_plots"]]()) {
    output <- hosp_grp_unit[["plot"]](type = plt) |>
      expect_no_error()
    expect_s3_class(output, "echarts4r")
  }
})

test_that("hospital_grp plot info works", {
  for (plt in hosp_grp_unit[["available_plots"]]()) {
    hosp_grp_unit[["plot_info"]](type = plt) |>
      expect_snapshot()
  }
})

test_that("hospital_grp plot functions error if wrong type", {
  hosp_grp_unit[["plot"]](type = "p") |>
    expect_error("`type` must be one.+")
  hosp_grp_unit[["plot_data"]](type = "p") |>
    expect_error("`type` must be one.+")
  hosp_grp_unit[["plot_info"]](type = "p") |>
    expect_error("`type` must be one.+")
})

test_that("national_trend_data works", {
  output <- hosp_grp_unit[["plot_data"]](type = "national_trend") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "national_trend_data")
})

test_that("health_board_trend_data works", {
  output <- hosp_grp_unit[["plot_data"]](type = "health_board_trend") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "health_board_trend_data")
})

test_that("health_board_bar_data works", {
  output <- hosp_grp_unit[["plot_data"]](type = "health_board_bar") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "health_board_bar_data")
})

test_that("hospital_trend_data works", {
  output <- hosp_grp_unit[["plot_data"]](type = "hospital_trend") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "hospital_trend_data")
})

test_that("hospital_bar_data works", {
  output <- hosp_grp_unit[["plot_data"]](type = "hospital_bar") |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "hospital_bar_data")
})

test_that("hospital_grp subset works", {
  hosp_grp_unit[["subset"]]("ID") |>
    expect_error("ids are not found in health unit group")

  output <- hosp_grp_unit[["subset"]]("A101H") |>
    expect_no_error()

  expect_true(inherits(output, "hospital_grp"))
  expect_identical(output[["ids"]](), "A101H")
  expect_identical(output[["sf"]]()[["ID"]], "A101H")
})

test_that("hospital_grp get_download works", {
  output <- hosp_grp_unit[["get_download"]]() |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_identical(hosp_grp_unit[["ids"]](), unique(output[["ID"]]))
})
