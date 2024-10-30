#' Launch shiny application
#' @param ... Passed to shiny::shinyApp.
#' @export
health_data_scotland <- function(...) {

    all_data <- list(process_gp_data(), process_hospital_data()) %>% 
        set_names(c("General practice", "Hospital")) %>%
        suppressMessages()

    ui <- dashboardPage(
        dashboardHeader(
            title = "Health Data Scotland"
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Map", tabName = "map")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "map",
                    fluidRow(
                        map_UI(
                            id = "map", 
                            boards = get_geojson("board") %>% 
                                as_tibble() %>% 
                                select("HBName", "id") %>% 
                                tibble::deframe()
                        )
                    )
                )
            )
        )
    )

    server <- function(input, output) {
        map_server("map", all_data, get_geojson("board"))
    }

    shinyApp(ui, server, ...)
}

process_gp_data <- function() {
    json <- get_geojson()

    meta <- get_gp_meta() %>% 
        rename("ID" = "PracticeCode") %>% 
        inner_join(
            select(as_tibble(get_geojson("board")), "id", "HBName"),
            by = c("HB" = "id")
        )

    data <- get_gp_data() %>% 
        select(-matches("QF$")) %>%
        rename("ID" = "PracticeCode") %>%
        mutate(Date = as.Date(as.character(.data[["Date"]]), format = "%Y%m%d"))

    master_ids <- list(json[["id"]], meta[["ID"]], data[["ID"]]) %>% 
        reduce(intersect)
    
    json <- json[json[["id"]] %in% master_ids, ]
    attr(json, "data") <- attr(json, "data") %>% 
        left_join(meta, by = c("id" = "ID")) %>% 
        rename("hbcode" = "HB") %>%
        select(required_json_cols())
    meta <- filter(meta, .data[["ID"]] %in% master_ids)
    data <- filter(data, .data[["ID"]] %in% master_ids)

    meta[["ID"]] %>% 
        map(function(id) {
            meta %>% 
                filter(.data[["ID"]] == id) %>% 
                select(-"HB", -"HSCP") %>%
                inner_join(data, by = "ID") %>% 
                gp[["new"]]()
        }) %>% 
        set_names(meta[["ID"]]) %>%
        gp_grp[["new"]](.json = json)
}

process_hospital_data <- function() {
    json <- get_geojson("hospital")
    
    meta <- get_hosp_meta() %>% 
        rename("ID" = "HospitalCode") %>% 
        inner_join(
            select(as_tibble(get_geojson("board")), "id", "HBName"),
            by = c("HealthBoard" = "id")
        )
    
    data <- rename(get_hosp_data(), "ID" = "Location")

    master_ids <- list(json[["id"]], meta[["ID"]], data[["ID"]]) %>% 
        reduce(intersect)
    
    json <- json[json[["id"]] %in% master_ids, ]
    attr(json, "data") <- select(attr(json, "data"), all_of(required_json_cols()))

    meta <- filter(meta, .data[["ID"]] %in% master_ids)
    data <- filter(data, .data[["ID"]] %in% master_ids)

    meta[["ID"]] %>% 
        map(function(id) {
            meta %>% 
                filter(.data[["ID"]] == id) %>% 
                inner_join(data, by = "ID") %>% 
                hospital[["new"]]()
        }) %>% 
        set_names(meta[["ID"]]) %>%
        hospital_grp[["new"]](.json = json)
}

required_json_cols <- function() {
    c("id", "postcode", "address", "type", "uprn", "hbcode")
}