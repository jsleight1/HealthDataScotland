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
                menuItem("Introduction", tabName = "intro", icon = icon("home")), 
                menuItem("GP practices", tabName = "gp"),
                menuItem("Hospitals", tabName = "hosp")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "intro"), 
                tabItem(tabName = "gp", fluidRow(pin_map_UI(id = "gp_map"))),
                tabItem(tabName = "hosp", fluidRow(pin_map_UI(id = "hosp_map")))
            )
        )
    )

    server <- function(input, output) {

        gp_json <- get_geojson()
        gp_meta <- get_gp_meta() %>% 
            filter(.data[["PracticeCode"]] %in% gp_json[["prac_code"]]) %>% 
            suppressMessages()
        gp_data <- get_gp_data() %>% 
            filter(.data[["PracticeCode"]] %in% gp_json[["prac_code"]]) %>% 
            suppressMessages()

        pin_map_server(
            id = "gp_map", 
            json = gp_json,
            meta = gp_meta, 
            data = gp_data, 
            initialise_popup = initialise_gp_popup
        )

        hosp_meta <- get_hosp_meta()
        hosp_data <- get_hosp_data()
        hosp_json <- get_geojson("hospital")
        hosp_json <- hosp_json[hosp_json[["id"]] %in% hosp_meta[["HospitalCode"]], ]
        hosp_json <- hosp_json[hosp_json[["id"]] %in% hosp_data[["Location"]], ]
        hosp_meta <- hosp_meta %>% 
            filter(.data[["HospitalCode"]] %in% hosp_json[["id"]])

        pin_map_server(
            id = "hosp_map",
            json = hosp_json, 
            meta = hosp_meta,
            data = hosp_data, 
            initialise_popup = initialise_hosp_popup
        )


    }

    shinyApp(ui, server, ...)
}