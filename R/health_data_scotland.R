#' Launch shiny application
#' @param ... Passed to shiny::shinyApp.
#' @export
health_data_scotland <- function(...) {

    all_data <- list(process_gp_data(), process_hospital_data()) %>% 
        reduce(bind_rows) %>% 
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
        filter(.data[["PracticeCode"]] %in% json[["prac_code"]]) %>% 
        rename("ID" = "PracticeCode") %>% 
        inner_join(
            select(as_tibble(get_geojson("board")), "id", "HBName"),
            by = c("HB" = "id")
        )
    data <- get_gp_data() %>% 
        filter(.data[["PracticeCode"]] %in% json[["prac_code"]]) %>% 
        select(-matches("QF$")) %>%
        filter(.data[["HB"]] %in% get_geojson("board")[["id"]]) %>%
        rename("ID" = "PracticeCode") %>%
        mutate(Date = as.Date(as.character(.data[["Date"]]), format = "%Y%m%d"))
    json <- json[json[["id"]] %in% meta[["ID"]] & json[["id"]] %in% data[["ID"]], ]
    attr(json, "data") <- attr(json, "data") %>% 
        left_join(meta, by = c("id" = "ID")) %>% 
        rename("hbcode" = "HB") %>%
        select(required_json_cols())
    tribble(
        ~type,               ~meta, ~data, ~json, ~initialise,
        "General practice",  meta,  data,  json,  gp
    )
}

process_hospital_data <- function() {
    json <- get_geojson("hospital")
    meta <- get_hosp_meta() %>% 
        rename("ID" = "HospitalCode") %>% 
        inner_join(
            select(as_tibble(get_geojson("board")), "id", "HBName"),
            by = c("HealthBoard" = "id")
        )
    data <- get_hosp_data() %>% 
        rename("ID" = "Location") %>%
        filter(.data[["HB"]] %in% get_geojson("board")[["id"]]) %>%
        filter(.data[["ID"]] %in% json[["id"]]) 
    json <- json[json[["id"]] %in% meta[["ID"]] & json[["id"]] %in% data[["ID"]], ]
    attr(json, "data") <- select(attr(json, "data"), all_of(required_json_cols()))
    tribble(
        ~type,       ~meta, ~data, ~json, ~initialise,
        "Hospital",  meta,  data,  json,  hospital
    )
}

required_json_cols <- function() {
    c("id", "postcode", "address", "type", "uprn", "hbcode")
}