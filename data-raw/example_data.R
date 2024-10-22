library(tidyverse)

gp_data_links <- list(
    "apr 2024" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/3306ab5a-cd22-494a-be76-ee6753cef92d/download/practice_listsizes_apr2024-open-data.csv",
    "jan 2024" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/488685e9-95ff-4a48-b085-af50e1dc1863/download/practice_listsizes_jan2024-open-data.csv",
    "oct 2023" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/ab677c28-f495-4191-83e1-aaa0c3e6a9b4/download/practice_listsizes_oct2023-open-data.csv"
)

gp_meta_links <- list(
    "apr 2024" = "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/b3b126d3-3b0c-4856-b348-0b37f8286367/download/practice_contactdetails_apr2024-open-data.csv"
)

hospital_data_links <- list(
    "2023" = "https://www.opendata.nhs.scot/dataset/7e21f62c-64a1-4aa7-b160-60cbdd8a700d/resource/d719af13-5fb3-430f-810e-ab3360961107/download/beds_by_location_of-treatment_specialty.csv"
)

hopsital_meta_links <- list(
    "jul 2024" = "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/hospitals.csv"
)

board_data_url <- "https://www.opendata.nhs.scot/dataset/7e21f62c-64a1-4aa7-b160-60cbdd8a700d/resource/5d55964b-8e45-4c49-bfdd-9ea3e1fb962d/download/beds_by_nhs_board-of-treatment_specialty.csv"

get_data <- function(x, nm = NULL) {
    httr::GET(
        x, 
        httr::write_disk(gp_file <- tempfile(fileext = ".xlsx"), overwrite = TRUE)
    )
    out <- readr::read_csv(gp_file)
    out[["Time"]] <- nm 
    out
}

years <- c("2022/23", "2023/24")

example_gp_metadata <- purrr::imap_dfr(gp_meta_links, get_data)
example_gp_data <- purrr::imap_dfr(gp_data_links, get_data)
example_hospital_metadata <- purrr::imap_dfr(hopsital_meta_links, get_data)
example_hospital_data <- purrr::imap_dfr(hospital_data_links, get_data) %>% 
    filter(FinancialYear %in% years)
example_board_data <- get_data(board_data_url) %>% 
    filter(FinancialYear %in% years)
example_board_metadata <- unique(example_board_data[, c("HB")])
example_board_metadata[["HBName"]] <- example_board_metadata[["HB"]]

usethis::use_data(
    example_gp_data, 
    example_gp_metadata,
    example_hospital_data,
    example_hospital_metadata,
    example_board_data, 
    example_board_metadata,
    overwrite = TRUE
)
