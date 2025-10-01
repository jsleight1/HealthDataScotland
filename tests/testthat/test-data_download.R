library(mockery)

test_that("get_gp_data works", {
  m <- mock()
  with_mocked_bindings(
    get_phs_dataset = m,
    expect_no_error(get_gp_data())
  )
  expect_args(m, 1, "gp-practice-populations")
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
  expect_equal(dim(output), c(911, 10))

  output <- get_sf("hospital") |>
    expect_no_error()
  expect_s3_class(output, "sf")
  expect_equal(dim(output), c(309, 16))

  output <- get_sf("board") |>
    expect_no_error()
  expect_s3_class(output, "sf")
  expect_equal(dim(output), c(14, 8))

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
  output <- HealthDataScotland::example_gp_sf |>
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
  output <- HealthDataScotland::example_hospital_sf |>
    process_hospital_sf(c("A101H", "A201H"), meta) |>
    expect_no_error()
  expect_called(m, 1)
  expect_s3_class(output, "sf")
  expect_identical(output[["ID"]], c("A101H", "A201H"))
})

test_that("create_processed_data works", {
  with_mocked_bindings(
    get_gp_meta = function() HealthDataScotland::example_gp_metadata,
    get_gp_data = function() HealthDataScotland::example_gp_data,
    get_hospital_meta = function() HealthDataScotland::example_hospital_metadata,
    get_hospital_data = function() HealthDataScotland::example_hospital_data,
    data <- create_processed_data() |>
      expect_no_error()
  )
  expect_true(inherits(data, "list"))
  expect_identical(names(data), c("gp", "hospital"))
  expect_true(inherits(data[["gp"]], "list"))
  expect_identical(names(data[["gp"]]), c("meta", "data", "sf"))
  expect_no_error(do.call(check_ids, data[["gp"]]))
  expect_true(inherits(data[["hospital"]], "list"))
  expect_identical(names(data[["hospital"]]), c("meta", "data", "sf"))
  expect_no_error(do.call(check_ids, data[["hospital"]]))
})
