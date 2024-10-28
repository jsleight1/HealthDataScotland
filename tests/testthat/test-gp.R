gp_data <- HealthDataScotland::example_gp_metadata %>% 
    select(-c("HB", "HSCP", "Time")) %>%
    inner_join(HealthDataScotland::example_gp_data, by = "PracticeCode") %>% 
    rename("ID" = "PracticeCode") %>% 
    inner_join(
        select(as_tibble(get_geojson("board")), "id", "HBName"),
        by = c("HB" = "id")
    )

gp_unit <- gp_data %>% 
    filter(.data[["ID"]] == 10002) %>%
    gp[["new"]]() 

test_that("gp class works", {
    "gp_data" %>% 
        gp[["new"]]() %>% 
        expect_error("Data set must be in data.frame")
      
    gp_data %>%
        gp[["new"]]() %>% 
        expect_error("Data set must contain only one unique ID")

    gp_data %>% 
        select(-"GPPracticeName") %>% 
        gp[["new"]]() %>% 
        expect_error("GPPracticeName column missing from data")

    out <- gp_data %>% 
        filter(.data[["ID"]] == 10002) %>%
        gp[["new"]]() %>% 
        expect_error(NA)

    expect_true(inherits(out, "gp"))
    expect_identical(out[["id"]](), 10002)
    expect_identical(out[["health_board"]](), "Tayside")
    expect_identical(out[["telephone"]](), "01382   580   264")
    expect_identical(out[["title"]](), "Muirhead Medical Centre")
    expect_identical(out[["address"]](), 
        "Muirhead Medical Centre, Liff Road, Muirhead, DD2 5NH")
    expect_identical(out[["available_plots"]](), 
        c("population_pyramid", "population_trend"))
})

test_that("gp class can be plotted", {
    gp_unit[["plot"]](type = "population_pyramid", date = 20240401) %>% 
        suppressWarnings() %>% 
        expect_s3_class("plotly")
    expect_s3_class(gp_unit[["plot"]](type = "population_trend"), "plotly")
})

test_that("gp ui works", {
    out <- gp_unit[["ui"]](function(i) "ns") %>% 
        expect_error(NA)
    expect_s3_class(out, "shiny.tag")
})