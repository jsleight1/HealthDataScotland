data_links <- list(
    "apr 2024" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/3306ab5a-cd22-494a-be76-ee6753cef92d/download/practice_listsizes_apr2024-open-data.csv",
    "jan 2024" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/488685e9-95ff-4a48-b085-af50e1dc1863/download/practice_listsizes_jan2024-open-data.csv",
    "oct 2023" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/ab677c28-f495-4191-83e1-aaa0c3e6a9b4/download/practice_listsizes_oct2023-open-data.csv"
)

meta_links <- list(
    "apr 2024" = "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/b3b126d3-3b0c-4856-b348-0b37f8286367/download/practice_contactdetails_apr2024-open-data.csv"
)

get_data <- function(x, nm) {
    httr::GET(
        x, 
        httr::write_disk(gp_file <- tempfile(fileext = ".xlsx"), overwrite = TRUE)
    )
    out <- readr::read_csv(gp_file)
    out[["Time"]] <- nm 
    out
}

example_gp_data <- purrr::imap_dfr(meta_links, get_data)
example_gp_metadata <- purrr::imap_dfr(data_links, get_data)

usethis::use_data(
    example_gp_data, 
    example_gp_metadata,
    overwrite = TRUE
)
