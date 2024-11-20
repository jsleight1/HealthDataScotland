library(tidyverse)

example_gp_data <- list(
        phsopendata::get_resource("3306ab5a-cd22-494a-be76-ee6753cef92d"),
        phsopendata::get_resource("488685e9-95ff-4a48-b085-af50e1dc1863"),
        phsopendata::get_resource("ab677c28-f495-4191-83e1-aaa0c3e6a9b4")
    ) |>
    dplyr::bind_rows() |>
    mutate_at("PracticeCode", as.character)

example_gp_metadata <- phsopendata::get_resource("b3b126d3-3b0c-4856-b348-0b37f8286367") |>
    mutate_at("PracticeCode", as.character)

example_hospital_data <- phsopendata::get_resource("d719af13-5fb3-430f-810e-ab3360961107") |> 
    filter(FinancialYear %in% c("2022/23", "2023/24"))

example_hospital_metadata <- phsopendata::get_resource("c698f450-eeed-41a0-88f7-c1e40a568acc")

example_gp_sf <- sf::read_sf(
    "inst/extdata/scotland_gps.json"
)
example_gp_sf[["ID"]] <- as.character(example_gp_sf[["prac_code"]])
example_gp_sf[["type"]] <- example_gp_sf[["General practice"]]

example_hospital_sf <- sf::read_sf(
    "inst/extdata/scotland_hosp.json"
)
example_hospital_sf[["ID"]] <- example_hospital_sf[["sitecode"]]
example_hospital_sf[["type"]] <- example_hospital_sf[["Hospital"]]

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
