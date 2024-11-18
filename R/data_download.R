get_gp_data <- function(max_resources = 10, ...) {
    get_dataset(
        "gp-practice-populations", 
        max_resources = max_resources,
        ...
    )
}

get_gp_meta <- function(...) {
    get_dataset(
        "gp-practice-contact-details-and-list-sizes", 
        max_resources = 1,
        ...
    )
}

get_hosp_data <- function(...) {
    get_dataset("annual-hospital-beds-information", ...)
}

get_hosp_meta <- function(...) {
    get_dataset("hospital-codes", max_resources = 1, ...)
}

get_sf <- function(type = c("gp", "hospital", "board")) {
    switch(rlang::arg_match(type), 
        "gp" = sf::read_sf(
            system.file("extdata", "scotland_gps.json", package = "HealthDataScotland")
        ) |> 
        set_id("prac_code") |> 
        set_type("General practice"),
        "hospital" = sf::read_sf(
            system.file("extdata", "scotland_hosp.json", package = "HealthDataScotland")
        ) |> 
        set_id("sitecode") |> 
        set_type("Hospital"),
        "board" = sf::read_sf(
            system.file("extdata", "scotland_boards.json", package = "HealthDataScotland")
        ) |> 
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