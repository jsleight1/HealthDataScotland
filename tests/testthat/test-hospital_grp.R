hosp_unit <- hospital_data |>
    filter(.data[["ID"]] == "A101H") |>
    hospital[["new"]]()

hosp_unit2 <- hospital_data |>
    filter(.data[["ID"]] == "A201H") |>
    hospital[["new"]]()

capture_output(sf <- get_sf("hospital"))

sf <- sf[sf[["ID"]] %in% c("A101H", "A201H"), ]

hosp_grp_unit <- hospital_grp[["new"]](
    list(hosp_unit, hosp_unit2),
    .sf = sf,
    .id = "hospital"
)

test_that("hospital_grp class works", {
    list(hosp_unit, "hosp_unit") |>
        hospital_grp[["new"]](.sf = sf, .id = "hospital") |>
        expect_error("group must contain the same class of health units")

    list(hosp_unit, hosp_unit2) |>
        hospital_grp[["new"]](.sf = "sf", .id = "hospital") |>
        expect_error("sf must be sf object")

    tst_sf <- sf[sf[["ID"]] == "A101H", ]

    list(hosp_unit, hosp_unit2) |>
        hospital_grp[["new"]](.sf = tst_sf, .id = "hospital") |>
        expect_error("Are all health units present in sf")

    out <- list(hosp_unit, hosp_unit2) |>
        hospital_grp[["new"]](.sf = sf, .id = 1) |>
        expect_error("ID must be character of length 1")

    list(hosp_unit, hosp_unit) |>
        hospital_grp[["new"]](.sf = bind_rows(tst_sf, tst_sf), .id = "hospital") |>
        expect_error("Health units must not be duplicated")

    out <- list(hosp_unit, hosp_unit2) |>
        hospital_grp[["new"]](.sf = sf, .id = "hospital") |>
        expect_no_error()

    expect_true(inherits(out, "hospital_grp"))
    expect_identical(out[["id"]](), "hospital")
    expect_identical(out[["ids"]](), c("A101H", "A201H"))
    expect_identical(out[["titles"]](), c("Arran War Memorial Hospital", "Ailsa Hospital"))
    expect_identical(out[["data"]](), list(hosp_unit, hosp_unit2))
    expect_identical(out[["sf"]](), sf)
    expect_identical(out[["health_unit"]]("A101H"), hosp_unit)
    expect_identical(
        out[["available_plots"]](),
        c("national_trend", "health_board_trend", "health_board_bar",
            "hospital_trend", "hospital_bar")
    )
})

test_that("hospital_grp class can be plotted", {
    for (plt in hosp_grp_unit[["available_plots"]]()) {
        output <- hosp_grp_unit[["plot"]](type = plt) |>
            expect_no_error()
        expect_s3_class(output, "echarts4r")
    }
})

test_that("hospital_grp plot data errors if wrong type", {
    hosp_grp_unit[["plot_data"]](type = "p") |>
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

    out <- hosp_grp_unit[["subset"]]("A101H") |>
        expect_no_error()

    expect_true(inherits(out, "hospital_grp"))
    expect_identical(out[["ids"]](), "A101H")
    expect_identical(out[["sf"]]()[["ID"]], "A101H")
})

test_that("hospital_grp get_download works", {
    out <- hosp_grp_unit[["get_download"]]() |>
       expect_no_error()
    expect_s3_class(out, "data.frame")
    expect_identical(hosp_grp_unit[["ids"]](), unique(out[["ID"]]))
})