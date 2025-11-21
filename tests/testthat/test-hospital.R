hosp_unit <- example_hospital_unit()

test_that("hospital class works", {
  metadata <- hosp_unit[["metadata"]]()
  data <- hosp_unit[["data"]]()

  hospital[["new"]]("metadata", "data") |>
    expect_error("Metadata set must be in data.frame")

  hospital[["new"]](metadata, "data") |>
    expect_error("Data set must be in data.frame")

  hospital[["new"]](bind_rows(metadata, metadata), data) |>
    expect_error("Metadata set must contain only one unique ID")

  metadata |>
    select(-"HospitalName") |>
    hospital[["new"]](data) |>
    expect_error("HospitalName column missing from metadata")

  hospital[["new"]](metadata, select(data, -"FinancialYear")) |>
    expect_error("FinancialYear column missing from data")

  output <- hospital[["new"]](metadata, data) |>
    expect_no_error()

  expect_true(inherits(output, "hospital"))
  expect_identical(output[["ID"]](), "A101H")
  expect_identical(output[["health_board"]](), "S08000015")
  expect_identical(output[["title"]](), "Arran War Memorial Hospital")
  expect_identical(
    output[["address"]](),
    "Lamlash, Isle of Arran, KA278LF"
  )
  expect_identical(output[["plot_types"]](), "specialty_line")
})

test_that("hospital class can be plotted", {
  expect_s3_class(hosp_unit[["plot"]](type = "specialty_line"), "shiny.tag")
})

test_that("hospital plot info works", {
  expect_snapshot(hosp_unit[["plot_info"]]("specialty_line"))
})

test_that("hospital plot functions error if wrong type", {
  hosp_unit[["plot"]](type = "p") |>
    expect_error("`type` must be one.+")
  hosp_unit[["plot_data"]](type = "p") |>
    expect_error("`type` must be one.+")
  hosp_unit[["plot_info"]](type = "p") |>
    expect_error("`type` must be one.+")
})

test_that("hospital plot data works", {
  output <- hosp_unit[["plot_data"]](
    type = "specialty_line",
    data_type = "annual",
    specialties  = c("All Specialties", "General Medicine")
  ) |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "annual_specialty_data")

  output <- hosp_unit[["plot_data"]](
    type = "specialty_line",
    data_type = "daily",
    specialties = c("All Specialties", "General Medicine")
  ) |>
    expect_no_error()
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "daily_specialty_data")

  hosp_unit[["plot_data"]](type = "p") |>
    expect_error("`type` must be one.+")
})

test_that("hospital ui works", {
  output <- hosp_unit[["ui"]](function(i) "ns") |>
    expect_no_error()
  expect_s3_class(output, "shiny.tag")
})

test_that("hospital summary works", {
  for (smy in hosp_unit[["summary_types"]]()) {
    output <- hosp_unit[["summary"]](type = smy) |>
      expect_no_error()
    expect_s3_class(output, "data.frame")
    expect_snapshot_json(output, glue("hospial_{smy}"))
  }
})

test_that("hospital summary info works", {
  for (smy in hosp_unit[["summary_types"]]()) {
    hosp_unit[["summary_info"]](type = smy) |>
      expect_snapshot()
  }
})

test_that("hospital summary functions error if wrong type", {
  hosp_unit[["summary"]](type = "p") |>
    expect_error("`type` must be one.+")
  hosp_unit[["summary_info"]](type = "p") |>
    expect_error("`type` must be one.+")
})

test_that("hospital datatable works", {
  output <- hosp_unit[["datatable"]]("specialty_summary") |>
    expect_no_error()
  expect_s3_class(output, "datatables")
  expect_identical(
    output[["x"]][["data"]],
    as.data.frame(hosp_unit[["summary"]]("specialty_summary"))
  )
})
