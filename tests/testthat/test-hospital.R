hospital_data <- HealthDataScotland::example_hospital_data %>% 
    inner_join(
        HealthDataScotland::example_hospital_metadata, 
        by = c("Location" = "HospitalCode")
    )

hosp_unit <- hospital_data %>% 
    filter(.data[["Location"]] == "A101H") %>%
    hospital[["new"]]()

test_that("hospital class works", {
    "hospital_data" %>% 
        hospital[["new"]]() %>% 
        expect_error("Data set must be in data.frame")
      
    hospital_data %>%
        hospital[["new"]]() %>% 
        expect_error("Data set must contain only one unique Location")

    hospital_data %>% 
        select(-"Location") %>% 
        hospital[["new"]]() %>% 
        expect_error("Location column missing from data")

    out <- hospital_data %>% 
        filter(.data[["Location"]] == "A101H") %>%
        hospital[["new"]]() %>% 
        expect_error(NA)

    expect_true(inherits(out, "hospital"))
    expect_identical(out[["id"]](), "A101H")
    expect_identical(out[["title"]](), "Arran War Memorial Hospital")
    expect_identical(out[["address"]](), 
        "Lamlash, Isle of Arran, KA278LF")
    expect_identical(out[["available_plots"]](), "specialty_bar")
})

test_that("hospital class can be plotted", {
    expect_s3_class(hosp_unit[["plot"]](type = "specialty_bar"), "plotly")
})