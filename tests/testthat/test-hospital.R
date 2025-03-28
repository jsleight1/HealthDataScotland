hosp_unit <- hospital_data |>
    filter(.data[["ID"]] == "A101H") |>
    hospital[["new"]]()

test_that("hospital class works", {
    "hospital_data" |>
        hospital[["new"]]() |>
        expect_error("Data set must be in data.frame")

    hospital_data |>
        hospital[["new"]]() |>
        expect_error("Data set must contain only one unique ID")

    hospital_data |>
        select(-"FinancialYear") |>
        hospital[["new"]]() |>
        expect_error("FinancialYear column missing from data")

    out <- hospital_data |>
        filter(.data[["ID"]] == "A101H") |>
        hospital[["new"]]() |>
        expect_no_error()

    expect_true(inherits(out, "hospital"))
    expect_identical(out[["ID"]](), "A101H")
     expect_identical(out[["health_board"]](), "Ayrshire and Arran")
    expect_identical(out[["title"]](), "Arran War Memorial Hospital")
    expect_identical(out[["address"]](),
        "Lamlash, Isle of Arran, KA278LF")
    expect_identical(
        out[["available_plots"]](),
        c("specialty_bar", "specialty_line")
    )
})

test_that("hospital class can be plotted", {
    expect_s3_class(hosp_unit[["plot"]](type = "specialty_bar"), "plotly")
    expect_s3_class(hosp_unit[["plot"]](type = "specialty_line"), "plotly")
})

test_that("hospital plot data works", {
    out <- hosp_unit[["plot_data"]](
            type = "specialty_bar",
            c("All Specialties", "General Medicine")
        ) |>
        expect_no_error()
    expect_s3_class(out, "data.frame")
    expect_snapshot_output(as.data.frame(out))

    out <- hosp_unit[["plot_data"]](
            type = "specialty_line",
            "annual",
            c("All Specialties", "General Medicine")
        ) |>
        expect_no_error()
    expect_s3_class(out, "data.frame")
    expect_snapshot_output(as.data.frame(out))

    out <- hosp_unit[["plot_data"]](
            type = "specialty_line",
            "daily",
            c("All Specialties", "General Medicine")
        ) |>
        expect_no_error()
    expect_s3_class(out, "data.frame")
    expect_snapshot_output(as.data.frame(out))

    hosp_unit[["plot_data"]](type = "p") |>
        expect_error("`type` must be one.+")
})

test_that("hospital ui works", {
    out <- hosp_unit[["ui"]](function(i) "ns") |>
        expect_no_error()
    expect_s3_class(out, "shiny.tag")
})