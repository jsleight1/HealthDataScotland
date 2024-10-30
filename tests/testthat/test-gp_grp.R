gp_unit <- gp_data %>% 
    filter(.data[["ID"]] == "10002") %>%
    gp[["new"]]() 

gp_unit2 <- gp_data %>% 
    filter(.data[["ID"]] == "10017") %>%
    gp[["new"]]() 

capture_output(json <- get_geojson())

json <- json[json[["id"]] %in% c("10002", "10017"), ]

gp_grp_unit <- gp_grp[["new"]](list(gp_unit, gp_unit2), .json = json)

test_that("gp_grp class works", {
    list(gp_unit, "gp_unit") %>% 
        gp_grp[["new"]](.json = json) %>% 
        expect_error("group must contain the same class of health units")

    list(gp_unit, gp_unit2) %>% 
        gp_grp[["new"]](.json = "json") %>% 
        expect_error("JSON must be SpatialPointsDataFrame object")

    tst_json <- json[json[["id"]] == "78185", ]

    list(gp_unit, gp_unit2) %>% 
        hospital_grp[["new"]](.json = tst_json) %>% 
        expect_error("All all health units present in JSON")

    out <- list(gp_unit, gp_unit2) %>% 
        gp_grp[["new"]](.json = json) %>% 
        expect_error(NA)

    expect_true(inherits(out, "gp_grp"))
    expect_identical(out[["ids"]](), c("10002", "10017"))
    expect_identical(out[["data"]](), list(gp_unit, gp_unit2))
    expect_identical(out[["json"]](), json)
    expect_identical(out[["health_unit"]]("10002"), gp_unit)
    expect_identical(out[["available_plots"]](), 
        c("population_pyramid", "population_trend"))
})

test_that("gp_grp class can be plotted", {
    gp_grp_unit[["plot"]](type = "population_pyramid", date = 20240401) %>% 
        suppressWarnings() %>% 
        expect_s3_class("plotly")
    expect_s3_class(gp_grp_unit[["plot"]](type = "population_trend"), "plotly")
})

test_that("gp_grp subset works", {
    gp_grp_unit[["subset"]]("id") %>% 
        expect_error("ids are not found in health unit group")

    out <- gp_grp_unit[["subset"]]("10002") %>% 
        expect_error(NA)

    expect_true(inherits(out, "gp_grp"))
    expect_identical(out[["ids"]](), "10002")
    expect_identical(out[["json"]]()[["id"]], "10002")
})