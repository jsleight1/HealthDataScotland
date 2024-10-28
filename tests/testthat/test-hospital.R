hospital_data <- HealthDataScotland::example_hospital_data %>% 
    inner_join(
        HealthDataScotland::example_hospital_metadata, 
        by = c("Location" = "HospitalCode")
    ) %>% 
    rename("ID" = "Location") %>% 
    inner_join(
        select(as_tibble(get_geojson("board")), "id", "HBName"),
        by = c("HB" = "id")
    )

hosp_unit <- hospital_data %>% 
    filter(.data[["ID"]] == "A101H") %>%
    hospital[["new"]]()

test_that("hospital class works", {
    "hospital_data" %>% 
        hospital[["new"]]() %>% 
        expect_error("Data set must be in data.frame")
      
    hospital_data %>%
        hospital[["new"]]() %>% 
        expect_error("Data set must contain only one unique ID")

    hospital_data %>% 
        select(-"FinancialYear") %>% 
        hospital[["new"]]() %>% 
        expect_error("FinancialYear column missing from data")

    out <- hospital_data %>% 
        filter(.data[["ID"]] == "A101H") %>%
        hospital[["new"]]() %>% 
        expect_error(NA)

    expect_true(inherits(out, "hospital"))
    expect_identical(out[["id"]](), "A101H")
     expect_identical(out[["health_board"]](), "Ayrshire and Arran")
    expect_identical(out[["title"]](), "Arran War Memorial Hospital")
    expect_identical(out[["address"]](), 
        "Lamlash, Isle of Arran, KA278LF")
    expect_identical(out[["available_plots"]](), "specialty_bar")
})

test_that("hospital class can be plotted", {
    expect_s3_class(hosp_unit[["plot"]](type = "specialty_bar"), "plotly")
})

test_that("hospital ui works", {
    out <- hosp_unit[["ui"]](function(i) "ns") %>% 
        expect_error(NA)
    expect_s3_class(out, "shiny.tag")
})