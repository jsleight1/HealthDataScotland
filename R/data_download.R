gp_data_urls <- function() {
    list(
        "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/3306ab5a-cd22-494a-be76-ee6753cef92d/download/practice_listsizes_apr2024-open-data.csv",
        "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/488685e9-95ff-4a48-b085-af50e1dc1863/download/practice_listsizes_jan2024-open-data.csv",
        "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/ab677c28-f495-4191-83e1-aaa0c3e6a9b4/download/practice_listsizes_oct2023-open-data.csv",
        "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/d7f423dd-9611-4ae9-a9c8-4dcc532ece22/download/practice_listsizes_jul2023-open-data.csv", 
        "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/4b9aa55f-b425-4a4f-8610-4c1c87b46c2e/download/practice_listsizes_apr2023-open-data.csv",
        "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/8921c5c1-ec7e-4c62-b55d-b73c3d086e3f/download/practice_listsizes_jan2023-open-data.csv",
        "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/2c7dcb55-c83a-44bd-9128-47d1d6f339ba/download/practice_listsizes_oct2022-open-data.csv",
        "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/64918d4f-f1d9-4e99-8e9f-130ddc890748/download/practice_listsizes_jul2022-open-data.csv",
        "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/2c701f90-c26d-4963-8062-95b8611e5fd1/download/practice_listsizes_apr2022-open-data.csv"
    )
}

get_gp_data <- function() {
    map_dfr(gp_data_urls(), get_data)
}

gp_meta_urls <- function() {
    list("https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/b3b126d3-3b0c-4856-b348-0b37f8286367/download/practice_contactdetails_apr2024-open-data.csv")
}

get_gp_meta <- function() {
    map_dfr(gp_meta_urls(), get_data)
}

hosp_meta_url <- function() {
    "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/hospitals.csv"
}

hosp_data_url <- function() {
    "https://www.opendata.nhs.scot/dataset/7e21f62c-64a1-4aa7-b160-60cbdd8a700d/resource/d719af13-5fb3-430f-810e-ab3360961107/download/beds_by_location_of-treatment_specialty.csv"
}

get_hosp_meta <- function() {
    get_data(hosp_meta_url())
}

get_hosp_data <- function() {
    get_data(hosp_data_url())
}

get_data <- function(x) {
    httr::GET(
        x, 
        httr::write_disk(file <- tempfile(fileext = ".xlsx"), overwrite = TRUE)
    )
    out <- read_csv(file)
    out
}

get_sf <- function(type = c("gp", "hospital", "board")) {
    switch(rlang::arg_match(type), 
        "gp" = sf::read_sf(
            system.file("extdata", "scotland_gps.json", package = "HealthDataScotland")
        ) %>% 
        set_id("prac_code") %>% 
        set_type("General practice"),
        "hospital" = sf::read_sf(
            system.file("extdata", "scotland_hosp.json", package = "HealthDataScotland")
        ) %>% 
        set_id("sitecode") %>% 
        set_type("Hospital"),
        "board" = sf::read_sf(
            system.file("extdata", "scotland_boards.json", package = "HealthDataScotland")
        ) %>% 
        set_id("HBCode")
    )
}

set_id <- function(x, col) {
    x[["id"]] <- as.character(x[[col]])
    x
}

set_type <- function(x, value) {
    x[["type"]] <- value 
    x
}