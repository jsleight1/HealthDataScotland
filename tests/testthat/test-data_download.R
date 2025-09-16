library(mockery)

test_that("get_gp_data works", {
  m <- mock()
  with_mocked_bindings(
    get_phs_dataset = m,
    expect_no_error(get_gp_data())
  )
  expect_args(m, 1, "gp-practice-populations", max_resources = 10)
})

test_that("get_gp_meta works", {
  m <- mock()
  with_mocked_bindings(
    get_phs_dataset = m,
    expect_no_error(get_gp_meta())
  )
  expect_args(m, 1, "gp-practice-contact-details-and-list-sizes",
    max_resources = 1
  )
})

test_that("get_hospital_data works", {
  m <- mock()
  with_mocked_bindings(
    get_phs_dataset = m,
    expect_no_error(get_hospital_data())
  )
  expect_args(m, 1, "annual-hospital-beds-information")
})

test_that("get_hospital_meta works", {
  m <- mock()
  with_mocked_bindings(
    get_phs_dataset = m,
    expect_no_error(get_hospital_meta())
  )
  expect_args(m, 1, "hospital-codes", max_resources = 1)
})

test_that("get_sf works", {
  output <- get_sf() |>
    expect_no_error()
  expect_s3_class(output, "sf")

  output <- get_sf("hospital") |>
    expect_no_error()
  expect_s3_class(output, "sf")

  output <- get_sf("board") |>
    expect_no_error()
  expect_s3_class(output, "sf")

  expect_error(get_sf("sf"), "`type` must be one of .+")
})

test_that("process_data works", {
  m1 <- mock(HealthDataScotland::example_gp_metadata)
  with_mocked_bindings(
    get_gp_meta = m1,
    meta <- process_gp_meta()
  )
  m2 <- mock(HealthDataScotland::example_gp_data)
  with_mocked_bindings(
    get_gp_data = m2,
    data <- process_gp_data()
  )
  output <- process_data(
    "gp",
    function() meta,
    function() data,
    process_gp_sf
  ) |>
    expect_no_error()
  expect_called(m1, 1)
  expect_called(m2, 1)
  expect_true(inherits(output, "list"))
  expect_identical(names(output), c("meta", "data", "sf"))
})

test_that("process_gp_meta works", {
  m <- mock(HealthDataScotland::example_gp_metadata)
  with_mocked_bindings(
    get_gp_meta = m,
    output <- process_gp_meta() |>
      expect_no_error()
  )
  expect_called(m, 1)
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "process_gp_meta")
})

test_that("process_gp_data works", {
  m <- mock(HealthDataScotland::example_gp_data)
  with_mocked_bindings(
    get_gp_data = m,
    output <- process_gp_data() |>
      expect_no_error()
  )
  expect_called(m, 1)
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "process_gp_data")
})

test_that("process_gp_sf works", {
  m <- mock(HealthDataScotland::example_gp_metadata)
  with_mocked_bindings(
    get_gp_meta = m,
    meta <- process_gp_meta()
  )
  output <- get_sf() |>
    process_gp_sf(c("10002", "10017"), meta) |>
    expect_no_error()
  expect_called(m, 1)
  expect_s3_class(output, "sf")
  expect_identical(output[["ID"]], c("10002", "10017"))
})

test_that("process_hopsital_meta works", {
  m <- mock(HealthDataScotland::example_hospital_metadata)
  with_mocked_bindings(
    get_hospital_meta = m,
    output <- process_hospital_meta() |>
      expect_no_error()
  )
  expect_called(m, 1)
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "process_hospital_meta")
})

test_that("process_hopsital_data works", {
  m <- mock(HealthDataScotland::example_hospital_data)
  with_mocked_bindings(
    get_hospital_data = m,
    output <- process_hospital_data() |>
      expect_no_error()
  )
  expect_called(m, 1)
  expect_s3_class(output, "data.frame")
  expect_snapshot_json(output, "process_hospital_data")
})

test_that("process_hospital_sf works", {
  m <- mock(HealthDataScotland::example_hospital_metadata)
  with_mocked_bindings(
    get_hospital_meta = m,
    meta <- process_hospital_meta()
  )
  output <- get_sf("hospital") |>
    process_hospital_sf(c("A101H", "A201H"), meta) |>
    expect_no_error()
  expect_called(m, 1)
  expect_s3_class(output, "sf")
  expect_identical(output[["ID"]], c("A101H", "A201H"))
})
