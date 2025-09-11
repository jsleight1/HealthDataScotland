gp_unit <- gp_data |>
    filter(.data[["ID"]] == "10002") |>
    gp[["new"]]()

gp_unit2 <- gp_data |>
    filter(.data[["ID"]] == "10017") |>
    gp[["new"]]()

capture_output(sf <- get_sf())

sf <- sf[sf[["ID"]] %in% c("10002", "10017"), ]

gp_grp_unit <- gp_grp[["new"]](list(gp_unit, gp_unit2), .sf = sf, .id = "gp")

test_that("gp_grp class works", {
    list(gp_unit, "gp_unit") |>
        gp_grp[["new"]](.sf = sf, .id = "gp") |>
        expect_error("group must contain the same class of health units")

    list(gp_unit, gp_unit2) |>
        gp_grp[["new"]](.sf = "sf", .id = "gp") |>
        expect_error("sf must be sf object")

    tst_sf <- sf[sf[["ID"]] == "10002", ]

    list(gp_unit, gp_unit2) |>
        gp_grp[["new"]](.sf = tst_sf, .id = "gp") |>
        expect_error("Are all health units present in sf")

    list(gp_unit, gp_unit2) |>
        gp_grp[["new"]](.sf = sf, .id = 1) |>
        expect_error("ID must be character of length 1")

    list(gp_unit, gp_unit) |>
        gp_grp[["new"]](.sf = bind_rows(tst_sf, tst_sf), .id = "gp") |>
        expect_error("Health units must not be duplicated")

    out <- list(gp_unit, gp_unit2) |>
        gp_grp[["new"]](.sf = sf, .id = "gp") |>
        expect_no_error()

    expect_true(inherits(out, "gp_grp"))
    expect_identical(out[["id"]](), "gp")
    expect_identical(out[["ids"]](), c("10002", "10017"))
    expect_identical(out[["titles"]](), c("Muirhead Medical Centre", "The Blue Practice"))
    expect_identical(out[["data"]](), list(gp_unit, gp_unit2))
    expect_identical(out[["sf"]](), sf)
    expect_identical(out[["health_unit"]]("10002"), gp_unit)
    expect_identical(out[["available_plots"]](),
        c("national_trend", "national_pyramid", "health_board_trend",
        "health_board_bar", "gp_trend", "gp_bar"))
})

test_that("gp_grp class can be plotted", {
    for (plt in gp_grp_unit[["available_plots"]]()) {
        output <- gp_grp_unit[["plot"]](type = plt) |>
            expect_no_error()
        expect_s3_class(output, "echarts4r")
    }
})

test_that("gp_grp plot data errors if wrong type", {
    gp_grp_unit[["plot_data"]](type = "p") |>
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
        expect_error("ids are not found in health unit group")

    out <- gp_grp_unit[["subset"]]("10002") |>
        expect_no_error()

    expect_true(inherits(out, "gp_grp"))
    expect_identical(out[["ids"]](), "10002")
    expect_identical(out[["sf"]]()[["ID"]], "10002")
})

test_that("gp_grp get_download works", {
    out <- gp_grp_unit[["get_download"]]() |>
       expect_no_error()
    expect_s3_class(out, "data.frame")
    expect_identical(gp_grp_unit[["ids"]](), unique(out[["ID"]]))
})