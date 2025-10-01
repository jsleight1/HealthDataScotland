map_unit <- example_map_unit()
sf <- map_unit[["sf"]]()

test_that("map class works", {
  "sf" |>
    map[["new"]]() |>
    expect_error("sf must be sf spatial data.frame")

  sf |>
    select(-"ID") |>
    map[["new"]]() |>
    expect_error("ID column missing from sf")


  output <- map[["new"]](sf) |>
    expect_no_error()
  expect_true(inherits(output, "map"))
  expect_s3_class(output[["sf"]](), c("sf", "tbl_df"))
  expect_identical(output[["id"]](), "map")
  expect_identical(output[["title"]](), "Interactive Map")
  expect_identical(output[["available_plots"]](), "interactive_map")
})

test_that("map class can be plotted", {
  for (plt in map_unit[["available_plots"]]()) {
    output <- map_unit[["plot"]](type = plt) |>
      expect_no_error()
    expect_s3_class(output, c("leaflet", "htmlwidget"))
  }
})

test_that("map plot info works", {
  for (plt in map_unit[["available_plots"]]()) {
    map_unit[["plot_info"]](type = plt) |>
      expect_snapshot()
  }
})

test_that("map plot functions error if wrong type", {
  map_unit[["plot"]](type = "p") |>
    expect_error("`type` must be one.+")
  map_unit[["plot_data"]](type = "p") |>
    expect_error("`type` must be one.+")
  map_unit[["plot_info"]](type = "p") |>
    expect_error("`type` must be one.+")
})

test_that("interactive_map_data works", {
  output <- map_unit[["plot_data"]](type = "interactive_map") |>
    expect_no_error()
  expect_s3_class(output, c("sf", "tbl_df"))
  expect_false("board" %in% output[["type"]])
  expect_snapshot_json(output, "interactive_map_data_full")

  output <- map_unit[["plot_data"]](type = "interactive_map", "10002") |>
    expect_no_error()
  expect_s3_class(output, c("sf", "tbl_df"))
  expect_false("board" %in% output[["type"]])
  expect_snapshot_json(output, "interactive_map_data_filtered")
})

test_that("map ui works", {
  output <- map_unit[["ui"]]() |>
    expect_no_error()
  expect_s3_class(output, "shiny.tag")
})
