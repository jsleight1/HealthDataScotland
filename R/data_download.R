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
    x[["ID"]] <- as.character(x[[col]])
    x
}

set_type <- function(x, value) {
    x[["type"]] <- value 
    x
}

process_data <- function(type, meta_func, data_func, sf_func) {
    sf <- get_sf(type)
    meta <- meta_func()
    data <- data_func()
    master_ids <- list(sf[["ID"]], meta[["ID"]], data[["ID"]]) |> 
        intersect_ids()
    sf <- sf_func(sf, master_ids, meta)
    meta <- filter(meta, .data[["ID"]] %in% master_ids)
    data <- filter(data, .data[["ID"]] %in% master_ids)
    create_process_lst(meta, data, sf)
}

process_gp_meta <- function() {
    get_gp_meta() |> 
        rename("ID" = "PracticeCode") |> 
        mutate_at("ID", as.character) |>
        inner_join(
            select(as_tibble(get_sf("board")), "ID", "HBName"),
            by = c("HB" = "ID")
        )
}

process_gp_data <- function() {
    get_gp_data() |> 
        select(-matches("QF$"), -"HB", -"HSCP") |>
        rename("ID" = "PracticeCode") |>
        mutate(
            Date = as.Date(as.character(.data[["Date"]]), format = "%Y%m%d"),
            ID = as.character(.data[["ID"]])
        )
}

process_gp_sf <- function(x, ids, meta, ...) {
    x |>
        filter(.data[["ID"]] %in% ids) |> 
        mutate_at("uprn", as.character) |> 
        left_join(meta, by = c("ID" = "ID")) |> 
        rename("hbcode" = "HB")
}

intersect_ids <- function(...) {
    reduce(..., intersect)
}

process_hospital_meta <- function() {
    get_hosp_meta() |> 
        rename("ID" = "HospitalCode") |>
        inner_join(
            select(as_tibble(get_sf("board")), "ID", "HBName"),
            by = c("HealthBoard" = "ID")
        )
}

process_hospital_data <- function() {
    get_hosp_data() |>
        filter(!is.na(.data[["FinancialYear"]])) |>
        rename("ID" = "Location") |>
        select_if(~ !all(is.na(.))) |>
        select(-contains("Address"))
}

process_hospital_sf <- function(x, ids, ...) {
    x |>
        filter(.data[["ID"]] %in% ids) |> 
        mutate_at("uprn", as.character)
}

create_process_lst <- function(meta, data, sf) {
    out <- meta |>
        inner_join(data, by = "ID") |>
        group_split(.data[["ID"]])
    names(out) <- map_chr(out, ~unique(.x[["ID"]]))
    list("x" = out, "sf" = sf)
}

create_data_objects <- function(x) {
    purrr::imap(x, ~{
        switch(.y, 
            "General practice" = create_gp_grp, 
            "Hospital" = create_hospital_grp
        ) |>
        do.call(.x)
    })
}

create_gp_grp <- function(x, sf) {
    x |>
        map(gp[["new"]]) |>
        gp_grp[["new"]](.sf = sf, .id = "gp")
}

create_hospital_grp <- function(x, sf) {
    x |>
        map(hospital[["new"]]) |>
        hospital_grp[["new"]](.sf = sf, .id = "hospital")
}

# tribble(
#         ~type,        ~meta_func,            ~data_func,              ~sf_func,
#         "gp",         process_gp_meta,       process_gp_data,         process_gp_sf,
#         "hospital",   process_hospital_meta, process_hospital_data,   process_hospital_sf
#     ) |>
#     purrr::pmap(process_data) |>
#     setNames(c("General practice", "Hospital")) |>
#     saveRDS("data.RDS")