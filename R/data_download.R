

get_gp_data <- function() {
    imap_dfr(gp_data_urls(), get_data)
}

gp_data_urls <- function() {
    list(
        "apr 2024" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/3306ab5a-cd22-494a-be76-ee6753cef92d/download/practice_listsizes_apr2024-open-data.csv",
        "jan 2024" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/3306ab5a-cd22-494a-be76-ee6753cef92d/download/practice_listsizes_jan2024-open-data.csv",
        "oct 2023" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/3306ab5a-cd22-494a-be76-ee6753cef92d/download/practice_listsizes_oct2023-open-data.csv",
        "jul 2023" = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/3306ab5a-cd22-494a-be76-ee6753cef92d/download/practice_listsizes_jul2023-open-data.csv"
    )
}

gp_meta_urls <- function() {
    list(
        "apr 2024" = "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/b3b126d3-3b0c-4856-b348-0b37f8286367/download/practice_contactdetails_apr2024-open-data.csv"
    )
}

get_gp_meta <- function() {
    imap_dfr(gp_meta_urls(), get_data)
}

get_data <- function(x, nm) {
    httr::GET(
        x, 
        httr::write_disk(gp_file <- tempfile(fileext = ".xlsx"), overwrite = TRUE)
    )
    out <- read_csv(gp_file)
    out[["Time"]] <- nm 
    out
}

get_geojson <- function() {
    rgdal::readOGR(
        system.file("extdata", "scotland_gps.json", package = "gpScotland")
    )
}