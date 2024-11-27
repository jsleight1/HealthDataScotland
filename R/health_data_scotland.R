#' Launch shiny application
#' @param ... Passed to shiny::shinyApp.
#' @export
health_data_scotland <- function(...) {
    requireNamespace("sf", quietly = TRUE)
    data <- create_data_objects(readRDS("processed_health_data.RDS"))

    ui <- dashboardPage(
        dashboardHeader(title = "Health Data Scotland"),
        dashboardSidebar(
            sidebarMenu(
                id = "sidebar",
                menuItem("Dashboard", tabName = "map"), 
                menuItem("Downloads", tabName = "download"), 
                menuItem("References", tabName = "references")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "map", 
                    box(
                        title = "Introduction", 
                        width = 14,
                        status = "primary",
                        solidHeader = TRUE,
                        h4("Welcome to the HealthDataScotland shiny application."),
                        h5("This dashboard shows a summary of demographic information
                        for GP practices and a summary of bed capacity for hospitals
                        across Scotland. The interactive map below can be used to 
                        click on indivdual GP practice and hospital to view associated
                        data for that particular health centre. The map draw functions 
                        (top left pentagon and square icons) can be used to draw an area
                        around particular health centres. Comparisons between the selected
                        health centres can then be viewed in the comparisons tab.
                        Data has been obtained from various sources (see References page)
                        and is available for download (see Downloads page)"), 
                    ),
                    fluidRow(
                        tabBox(
                            id = "map_tabs",
                            width = 12,
                            map_UI(
                                id = "map", 
                                boards = get_sf("board") |> 
                                    as_tibble() |> 
                                    select("HBName", "ID") |> 
                                    tibble::deframe()
                            ),
                            map_comparison_UI(id = "map_comparison")
                        )
                    ),
                    p("Please note that this application was built as a hobby project.
                    Therefore informed decisions based on the presented data should 
                    only be made after consulating the relevant published data sets.
                    These can be accessed in the references section.",
                    style = "color:red; padding-top:10px;")
                ), 
                tabItem(
                    tabName = "download",
                    download_UI("downloads")
                ),
                tabItem(
                    tabName = "references",
                    box(
                        title = "References",
                        h4("Data sources"),
                        p(tags$a(href = "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90", " - GP metadata")),
                        p(tags$a(href = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c", " - GP demography data")),
                        p(tags$a(href = "https://data.spatialhub.scot/dataset/gp_practices-is/resource/8389fd1d-563d-4c05-9833-26d9f07fd6cd", " - GP spatial data")),
                        p(tags$a(href = "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0", " - Hospital metadata")),
                        p(tags$a(href = "https://www.opendata.nhs.scot/dataset/7e21f62c-64a1-4aa7-b160-60cbdd8a700d", " - Hospital specialty data")),
                        p(tags$a(href = "https://data.spatialhub.scot/dataset/nhs_hospitals-is/resource/b810d206-45bd-4dff-bac7-110a50b4bd3b", " - Hospital spatial data")),
                        p(tags$a(href = "https://data.gov.uk/dataset/27d0fe5f-79bb-4116-aec9-a8e565ff756a/nhs-health-boards", " - Health board spatial data")),
                        h4("GitHub"),
                        p(tags$a(href = "https://github.com/jsleight1/HealthDataScotland", " - HealthDataScotland")),
                        h4("Notes"), 
                        p("Please note that GP practices and hospitals included in this application
                        are only those present in all three of the metadata, demography/specialty data 
                        and spatial data. Where missing appears to be missing please refer to the
                        original published data sets."),
                        width = 12
                    )
                )
            )
        )
    )

    server <- function(input, output) {
        requireNamespace("sf", quietly = TRUE)
        selected_data <- map_server("map", data, get_sf("board"))
        map_comparison_server("map_comparison", selected_data)
        download_server("downloads", data)
    }

    shinyApp(ui, server, ...)
}
