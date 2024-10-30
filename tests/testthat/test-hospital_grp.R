hosp_unit <- hospital_data %>% 
    filter(.data[["ID"]] == "A101H") %>%
    hospital[["new"]]()

hosp_unit2 <- hospital_data %>% 
    filter(.data[["ID"]] == "A201H") %>%
    hospital[["new"]]()

capture_output(json <- get_geojson("hospital"))

json <- json[json[["id"]] %in% c("A101H", "A201H"), ]

hosp_grp_unit <- hospital_grp[["new"]](list(hosp_unit, hosp_unit2), .json = json)

test_that("hospital_grp class works", {
    list(hosp_unit, "hosp_unit") %>% 
        hospital_grp[["new"]](.json = json) %>% 
        expect_error("group must contain the same class of health units")

    list(hosp_unit, hosp_unit2) %>% 
        hospital_grp[["new"]](.json = "json") %>% 
        expect_error("JSON must be SpatialPointsDataFrame object")

    tst_json <- json[json[["id"]] == "L203H", ]

    list(hosp_unit, hosp_unit2) %>% 
        hospital_grp[["new"]](.json = tst_json) %>% 
        expect_error("All all health units present in JSON")

    out <- list(hosp_unit, hosp_unit2) %>% 
        hospital_grp[["new"]](.json = json) %>% 
        expect_error(NA)

    expect_true(inherits(out, "hospital_grp"))
    expect_identical(out[["ids"]](), c("A101H", "A201H"))
    expect_identical(out[["data"]](), list(hosp_unit, hosp_unit2))
    expect_identical(out[["json"]](), json)
    expect_identical(out[["health_unit"]]("A101H"), hosp_unit)
    expect_identical(out[["available_plots"]](), "specialty_bar")
})

test_that("hospital_grp class can be plotted", {
    expect_s3_class(hosp_grp_unit[["plot"]](type = "specialty_bar"), "plotly")
})

test_that("hospital_grp subset works", {
    hosp_grp_unit[["subset"]]("id") %>% 
        expect_error("ids are not found in health unit group")

    out <- hosp_grp_unit[["subset"]]("A101H") %>% 
        expect_error(NA)

    expect_true(inherits(out, "hospital_grp"))
    expect_identical(out[["ids"]](), "A101H")
    expect_identical(out[["json"]]()[["id"]], "A101H")
})