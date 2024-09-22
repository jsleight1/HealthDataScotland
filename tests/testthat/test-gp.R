gp_data <- HealthDataScotland::example_gp_metadata %>% 
    select(-c("HB", "HSCP", "Time")) %>%
    inner_join(HealthDataScotland::example_gp_data, by = "PracticeCode") 

test_that("gp class works", {
    "gp_data" %>% 
        gp[["new"]]() %>% 
        expect_error("Data set must be in data.frame")
      
    gp_data %>%
        gp[["new"]]() %>% 
        expect_error("Data set must contain only one unique PracticeCode")

    gp_data %>% 
        select(-"GPPracticeName") %>% 
        gp[["new"]]() %>% 
        expect_error("GPPracticeName column missing from data")

    out <- gp_data %>% 
        filter(.data[["PracticeCode"]] == 10002) %>%
        gp[["new"]]() %>% 
        expect_error(NA)

    expect_true(inherits(out, "gp"))
    expect_identical(out[["id"]](), 10002)
    expect_identical(out[["title"]](), "Muirhead Medical Centre")
    expect_identical(out[["address"]](), 
        "Muirhead Medical Centre, Liff Road, Muirhead, DD2 5NH")
    expect_identical(out[["available_plots"]](), 
        c("population_pyramid", "population_trend"))
})