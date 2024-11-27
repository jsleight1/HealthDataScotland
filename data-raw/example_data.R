library(dplyr)
library(purrr)
library(httr2)

get_phs_dataset <- function(ids, ...) {
    dump_url <- "https://www.opendata.nhs.scot/datastore/dump"
    urls <- map(ids, ~glue::glue("{dump_url}/{.x}"))
    map(urls, ~{
            request(.x) |>
                req_retry(max_tries = 5) |>
                req_perform() |>
                resp_body_string() |>
                readr::read_csv() |>
                select(-"_id") |>
                suppressMessages()
        }) |>
        set_names(ids) |>
        bind_rows(.id = "datasetID")
}

example_gp_data <- c(
        "3306ab5a-cd22-494a-be76-ee6753cef92d", 
        "488685e9-95ff-4a48-b085-af50e1dc1863",
        "ab677c28-f495-4191-83e1-aaa0c3e6a9b4"
    ) |>
    get_phs_dataset() |>
    mutate_at("PracticeCode", as.character)

example_gp_metadata <- get_phs_dataset("b3b126d3-3b0c-4856-b348-0b37f8286367") |>
    mutate_at("PracticeCode", as.character)

example_hospital_data <- get_phs_dataset("d719af13-5fb3-430f-810e-ab3360961107") |> 
    filter(FinancialYear %in% c("2022/23", "2023/24")) |>
    mutate(datasetID = "d719af13-5fb3-430f-810e-ab3360961107")

example_hospital_metadata <- get_phs_dataset("c698f450-eeed-41a0-88f7-c1e40a568acc")

example_gp_sf <- sf::read_sf(
    "inst/extdata/scotland_gps.json"
)
example_gp_sf[["ID"]] <- as.character(example_gp_sf[["prac_code"]])
example_gp_sf[["type"]] <- "General practice"

example_hospital_sf <- sf::read_sf(
    "inst/extdata/scotland_hosp.json"
)
example_hospital_sf[["ID"]] <- example_hospital_sf[["sitecode"]]
example_hospital_sf[["type"]] <- "Hospital"

example_board_sf <- sf::read_sf(
    "inst/extdata/scotland_boards.json"
)
example_board_sf[["ID"]] <- example_board_sf[["HBCode"]]

usethis::use_data(
    example_gp_data, 
    example_gp_metadata,
    example_hospital_data,
    example_hospital_metadata,
    example_gp_sf, 
    example_hospital_sf,
    example_board_sf,
    overwrite = TRUE
)
