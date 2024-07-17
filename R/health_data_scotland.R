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
                menuItem("Introduction", tabName = "intro", icon = icon("home")), 
                menuItem("GP practices", tabName = "gp")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "intro"), 
                tabItem(tabName = "gp", fluidRow(pin_map_UI(id = "gp_map")))
            )
        )
    )

    server <- function(input, output) {

        gp_json <- get_geojson()
        gp_meta <- suppressMessages(get_gp_meta()) %>% 
            filter(.data[["PracticeCode"]] %in% gp_json[["prac_code"]])
        gp_data <- suppressMessages(get_gp_data()) %>% 
            filter(.data[["PracticeCode"]] %in% gp_json[["prac_code"]])

        pin_map_server(
            id = "gp_map", 
            json = gp_json,
            meta = gp_meta, 
            data = gp_data, 
            initialise_popup = initialise_gp_popup
        )
    }

    shinyApp(ui, server, ...)
}