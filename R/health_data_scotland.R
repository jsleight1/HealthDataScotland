#' Launch shiny application
#' @param ... Passed to shiny::shinyApp.
#' @export
health_data_scotland <- function(...) {
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
                    fluidRow(map_UI(id = "map"))
                )
            )
        )
    )

    server <- function(input, output) {

        board_json <- get_geojson("board")
        board_meta <- as_tibble(board_json) %>%
            select("id", "HBName")

        gp_json <- get_geojson()
        gp_meta <- get_gp_meta() %>% 
            filter(.data[["PracticeCode"]] %in% gp_json[["prac_code"]]) %>% 
            rename("ID" = "PracticeCode") %>%
            suppressMessages()
        gp_data <- get_gp_data() %>% 
            filter(.data[["PracticeCode"]] %in% gp_json[["prac_code"]]) %>% 
            select(-matches("QF$")) %>%
            filter(.data[["HB"]] %in% board_meta[["id"]]) %>%
            rename("ID" = "PracticeCode") %>%
            mutate(Date = as.Date(as.character(.data[["Date"]]), format = "%Y%m%d")) %>%
            suppressMessages()

        hosp_json <- get_geojson("hospital")
        hosp_meta <- get_hosp_meta() %>% 
            rename("ID" = "HospitalCode") %>%
            suppressMessages()
        hosp_data <- get_hosp_data() %>% 
            rename("ID" = "Location") %>%
            filter(.data[["HB"]] %in% board_meta[["id"]]) %>%
            filter(.data[["ID"]] %in% hosp_json[["id"]]) %>%
            suppressMessages()
        hosp_json <- hosp_json[hosp_json[["id"]] %in% hosp_meta[["ID"]], ]
        hosp_json <- hosp_json[hosp_json[["id"]] %in% hosp_data[["ID"]], ]

        all_data <- list(
            "gp" = list(
                "json" = gp_json, 
                "meta" = gp_meta, 
                "data" = gp_data
            ),
            "hosp" = list(
                "json" = hosp_json, 
                "meta" = hosp_meta, 
                "data" = hosp_data
            ), 
            "board" = board_json
        )

        map_server("map", all_data)
    }

    shinyApp(ui, server, ...)
}
