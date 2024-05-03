#' @export
gpScotland <- function(...) {
    ui <- dashboardPage(
        dashboardHeader(
            title = "GP Practices Scotland"
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Introduction", tabName = "intro", icon = icon("home")),
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "intro",
                    fluidRow(
                        h2("Map of GP practices"),
                        mapUI(id = "map")
                    )
                )
            )
        )
    )

    server <- function(input, output) {
        introServer(id = "intro")
        mapServer(id = "map")
    }

    shinyApp(ui, server, ...)
}