#' Launch shiny application
#' @param ... Passed to shiny::shinyApp.
#' @export
health_data_scotland <- function(...) {
    ui <- dashboardPage(
        dashboardHeader(
            title = "Health data Scotland"
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Introduction", tabName = "intro", icon = icon("home"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "intro",
                    fluidRow(
                        gp_map_UI(id = "gp_map")
                    )
                )
            )
        )
    )

    server <- function(input, output) {

        json <- get_geojson()

        gp_meta <- suppressMessages(get_gp_meta()) %>% 
            filter(.data[["PracticeCode"]] %in% json[["prac_code"]])
        
        gp_data <- suppressMessages(get_gp_data()) %>% 
            filter(.data[["PracticeCode"]] %in% json[["prac_code"]])


        gp_map_server(
            id = "gp_map", 
            json = json,
            meta = gp_meta, 
            data = gp_data
        )
    }

    shinyApp(ui, server, ...)
}