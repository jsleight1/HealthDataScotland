get_phs_dataset <- function(x, ...) {
    ids <- get_phs_ids(x, ...)
    dump_url <- "https://www.opendata.nhs.scot/datastore/dump"
    urls <- map(ids, ~glue("{dump_url}/{.x}"))
    map(urls, ~{
            request(.x) |>
                req_retry(max_tries = 5) |>
                req_perform() |>
                resp_body_string() |>
                readr::read_csv() |>
                dplyr::select(-"_id") |>
                suppressMessages()
        }) |>
        set_names(ids) |>
        bind_rows(.id = "datasetID")
}

get_phs_ids <- function(x, max_resources = NULL, ...) {
    url <- glue("https://www.opendata.nhs.scot/api/3/action/package_show?id={x}")
    content <- request(url) |>
        req_retry(max_tries = 5) |>
        req_perform() |>
        resp_body_json()
    ids <- map_chr(content[["result"]][["resources"]], ~.x[["id"]])
    ids[seq(min(length(ids), max_resources))]
}

get_gp_data <- function(max_resources = 10, ...) {
    get_phs_dataset(
        "gp-practice-populations",
        max_resources = max_resources,
        ...
    )
}

get_gp_meta <- function(...) {
    get_phs_dataset(
        "gp-practice-contact-details-and-list-sizes",
        max_resources = 1,
        ...
    )
}

get_hospital_data <- function(...) {
    get_phs_dataset("annual-hospital-beds-information", ...)
}

get_hospital_meta <- function(...) {
    get_phs_dataset("hospital-codes", max_resources = 1, ...)
}

get_sf <- function(type = c("gp", "hospital", "board")) {
    requireNamespace("sf", quietly = TRUE)
    switch(rlang::arg_match(type),
        "gp" = HealthDataScotland::example_gp_sf,
        "hospital" = HealthDataScotland::example_hospital_sf,
        "board" = HealthDataScotland::example_board_sf,
    )
}

process_data <- function(type, meta_func, data_func, sf_func) {
    sf <- get_sf(type)
    meta <- rename(meta_func(), "PHS metadata ID" = "datasetID")
    data <- rename(data_func(), "PHS data ID" = "datasetID")
    master_ids <- list(sf[["ID"]], meta[["ID"]], data[["ID"]]) |>
        reduce(intersect)
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

process_hospital_meta <- function() {
    get_hospital_meta() |>
        rename("ID" = "HospitalCode") |>
        inner_join(
            select(as_tibble(get_sf("board")), "ID", "HBName"),
            by = c("HealthBoard" = "ID")
        )
}

process_hospital_data <- function() {
    get_hospital_data() |>
        filter(.data[["datasetID"]] == "d719af13-5fb3-430f-810e-ab3360961107") |>
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

save_processed_data <- function(out = "processed_health_data.RDS") {
    tribble(
        ~type,        ~meta_func,            ~data_func,              ~sf_func,
        "gp",         process_gp_meta,       process_gp_data,         process_gp_sf,
        "hospital",   process_hospital_meta, process_hospital_data,   process_hospital_sf
    ) |>
    purrr::pmap(process_data) |>
    set_names(c("General practice", "Hospital")) |>
    saveRDS(out)
}
