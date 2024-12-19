
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
        max_resources = 1)
})

test_that("get_hospital_data works", {
    m <- mock()
    with_mocked_bindings(
        get_phs_dataset = m,
        expect_no_error(get_hospital_data())
    )
    expect_args(m, 1, "annual-hospital-beds-information")
})

test_that("get_hospital_data works", {
    m <- mock()
    with_mocked_bindings(
        get_phs_dataset = m,
        expect_no_error(get_hospital_meta())
    )
    expect_args(m, 1, "hospital-codes", max_resources = 1)
})

test_that("get_sf works", {
    out <- get_sf() |>
        expect_no_error()
    expect_s3_class(out, "sf")

    out <- get_sf("hospital") |>
        expect_no_error()
    expect_s3_class(out, "sf")

    out <- get_sf("board") |>
        expect_no_error()
    expect_s3_class(out, "sf")

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
    out <- process_data(
            "gp",
            function() meta,
            function() data,
            process_gp_sf
        ) |>
        expect_no_error()
    expect_called(m1, 1)
    expect_called(m2, 1)
    expect_true(inherits(out, "list"))
    expect_identical(names(out), c("x", "sf"))
})

test_that("process_gp_meta works", {
    m <- mock(HealthDataScotland::example_gp_metadata)
    with_mocked_bindings(
        get_gp_meta = m,
        out <- process_gp_meta() |>
            expect_no_error()
    )
    expect_called(m, 1)
    expect_s3_class(out, "data.frame")
    expect_snapshot_output(head(as.data.frame(out)))
})

test_that("process_gp_data works", {
    m <- mock(HealthDataScotland::example_gp_data)
    with_mocked_bindings(
        get_gp_data = m,
        out <- process_gp_data() |>
            expect_no_error()
    )
    expect_called(m, 1)
    expect_s3_class(out, "data.frame")
    expect_snapshot_output(head(as.data.frame(out)))
})

test_that("process_gp_sf works", {
    m <- mock(HealthDataScotland::example_gp_metadata)
    with_mocked_bindings(
        get_gp_meta = m,
        meta <- process_gp_meta()
    )
    out <- get_sf() |>
        process_gp_sf(c("30030", "30059"), meta) |>
        expect_no_error()
    expect_called(m, 1)
    expect_s3_class(out, "sf")
    expect_identical(out[["ID"]], c("30030", "30059"))
})

test_that("process_hopsital_meta works", {
    m <- mock(HealthDataScotland::example_hospital_metadata)
    with_mocked_bindings(
        get_hospital_meta = m,
        out <- process_hospital_meta() |>
            expect_no_error()
    )
    expect_called(m, 1)
    expect_s3_class(out, "data.frame")
    expect_snapshot_output(head(as.data.frame(out)))
})

test_that("process_hopsital_data works", {
    m <- mock(HealthDataScotland::example_hospital_data)
    with_mocked_bindings(
        get_hospital_data = m,
        out <- process_hospital_data() |>
            expect_no_error()
    )
    expect_called(m, 1)
    expect_s3_class(out, "data.frame")
    expect_snapshot_output(head(as.data.frame(out)))
})

test_that("process_hospital_sf works", {
    m <- mock(HealthDataScotland::example_hospital_metadata)
    with_mocked_bindings(
        get_hospital_meta = m,
        meta <- process_hospital_meta()
    )
    out <- get_sf("hospital") |>
        process_hospital_sf(c("A013G", "A026B"), meta) |>
        expect_no_error()
    expect_called(m, 1)
    expect_s3_class(out, "sf")
    expect_identical(out[["ID"]], c("A013G", "A026B"))
})
