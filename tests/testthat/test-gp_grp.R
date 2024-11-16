gp_unit <- gp_data |> 
    filter(.data[["ID"]] == "10002") |>
    gp[["new"]]() 

gp_unit2 <- gp_data |> 
    filter(.data[["ID"]] == "10017") |>
    gp[["new"]]() 

capture_output(sf <- get_sf())

sf <- sf[sf[["id"]] %in% c("10002", "10017"), ]

gp_grp_unit <- gp_grp[["new"]](list(gp_unit, gp_unit2), .sf = sf)

test_that("gp_grp class works", {
    list(gp_unit, "gp_unit") |> 
        gp_grp[["new"]](.sf = sf) |> 
        expect_error("group must contain the same class of health units")

    list(gp_unit, gp_unit2) |> 
        gp_grp[["new"]](.sf = "sf") |> 
        expect_error("sf must be sf object")

    tst_sf <- sf[sf[["id"]] == "78185", ]

    list(gp_unit, gp_unit2) |> 
        hospital_grp[["new"]](.sf = tst_sf) |> 
        expect_error("All all health units present in sf")

    out <- list(gp_unit, gp_unit2) |> 
        gp_grp[["new"]](.sf = sf) |> 
        expect_no_error()

    expect_true(inherits(out, "gp_grp"))
    expect_identical(out[["ids"]](), c("10002", "10017"))
    expect_identical(out[["titles"]](), c("Muirhead Medical Centre", "The Blue Practice"))
    expect_identical(out[["data"]](), list(gp_unit, gp_unit2))
    expect_identical(out[["sf"]](), sf)
    expect_identical(out[["health_unit"]]("10002"), gp_unit)
    expect_identical(out[["available_plots"]](), 
        c("population_pyramid", "population_trend"))
})

test_that("gp_grp class can be plotted", {
    gp_grp_unit[["plot"]](type = "population_pyramid", date = 20240401) |> 
        suppressWarnings() |> 
        expect_s3_class("plotly")
    expect_s3_class(gp_grp_unit[["plot"]](type = "population_trend"), "plotly")
})

test_that("gp_grp subset works", {
    gp_grp_unit[["subset"]]("id") |> 
        expect_error("ids are not found in health unit group")

    out <- gp_grp_unit[["subset"]]("10002") |> 
        expect_no_error()

    expect_true(inherits(out, "gp_grp"))
    expect_identical(out[["ids"]](), "10002")
    expect_identical(out[["sf"]]()[["id"]], "10002")
})

test_that("gp_grp get_download works", {
    out <- gp_grp_unit[["get_download"]]() |>
       expect_no_error()
    expect_s3_class(out, "data.frame")
    expect_identical(gp_grp_unit[["ids"]](), unique(out[["ID"]]))
})