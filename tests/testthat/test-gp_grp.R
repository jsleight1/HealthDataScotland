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

    tst_sf <- sf[sf[["ID"]] == "78185", ]

    list(gp_unit, gp_unit2) |> 
        hospital_grp[["new"]](.sf = tst_sf, .id = "gp") |> 
        expect_error("All all health units present in sf")

    list(gp_unit, gp_unit2) |> 
        gp_grp[["new"]](.sf = sf, .id = 1) |> 
        expect_error("ID must be character of length 1")

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
        c("population_pyramid", "population_trend"))
})

# test_that("gp_grp class can be plotted", {
#     out <- gp_grp_unit[["plot"]](type = "population_pyramid", date = 20240401) |> 
#         suppressWarnings() |> 
#         expect_no_error()
#     expect_s3_class(out, "plotly")
#     expect_snapshot_plotly(out, "gp_grp_pyramid")

#     out <- gp_grp_unit[["plot"]](type = "population_trend") |>
#         expect_no_error()
#     expect_s3_class(out, "plotly")
#     expect_snapshot_plotly(out, "gp_grp_trend")
# })

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

test_that("gp_grp ui works", {
    out <- gp_grp_unit[["ui"]](function(i) "ns") |> 
        expect_no_error()
    expect_s3_class(out, "shiny.tag")
})