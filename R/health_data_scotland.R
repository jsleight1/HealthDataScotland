#' Launch shiny application
#' @param ... Passed to shiny::shinyApp.
#' @export
health_data_scotland <- function(...) {
    requireNamespace("sf", quietly = TRUE)
    data <- create_data_objects(readRDS("processed_health_data.RDS"))

    value_boxes <- data |>
        purrr::imap(function(x, nm) {
            value_box(
                title = glue("Number of {tolower(nm)}s analysed"),
                showcase = bs_icon("bar-chart"),
                value = length(x[["ids"]]())
            )
        })

    ui <- page_navbar(
        theme = bs_theme(
            brand = system.file("www", "_brand.yml", package = "HealthDataScotland")
        ),
        title = "Health Data Scotland",
        id = "main",
        nav_panel(
            title = bs_icon("house-fill"),
            card(
                full_screen = TRUE,
                card_body(
                    h3("Welcome to HealthDataScotland"),
                    h4("This application presents a summary of demographics
                    information for GP practices and a summary of bed capacity
                    for hospitals across Scotland. Data has been obtained from
                    various sources (see References page) and is avaiable for
                    download (see Downloads page).")
                )
            ),
            reduce(value_boxes, layout_column_wrap)
        ),
        nav_panel(
            title = "Map",
            full_screen = TRUE,
            height = "600px",
            nav_panel(
                title = "Interactive map",
                map_UI(
                    id = "map",
                    boards = get_sf("board") |>
                        as_tibble() |>
                        select("HBName", "ID") |>
                        tibble::deframe()
                )
            )
        ),
        nav_menu(
            title = "Summary",
            data[["General practice"]][["ui"]](),
            data[["Hospital"]][["ui"]]()
        ),
        nav_panel(
            title = "Notes",
            card(
                h4("Data sources"),
                p(tags$a(href = "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90", " - GP metadata")),
                p(tags$a(href = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c", " - GP demography data")),
                p(tags$a(href = "https://data.spatialhub.scot/dataset/gp_practices-is/resource/8389fd1d-563d-4c05-9833-26d9f07fd6cd", " - GP spatial data")),
                p(tags$a(href = "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0", " - Hospital metadata")),
                p(tags$a(href = "https://www.opendata.nhs.scot/dataset/7e21f62c-64a1-4aa7-b160-60cbdd8a700d", " - Hospital specialty data")),
                p(tags$a(href = "https://data.spatialhub.scot/dataset/nhs_hospitals-is/resource/b810d206-45bd-4dff-bac7-110a50b4bd3b", " - Hospital spatial data")),
                p(tags$a(href = "https://data.gov.uk/dataset/27d0fe5f-79bb-4116-aec9-a8e565ff756a/nhs-health-boards", " - Health board spatial data")),
                p("Please note that GP practices and hospitals included in this application
                are only those present in all three of the metadata, demography/specialty data
                and spatial data. Where missing appears to be missing please refer to the
                original published data sets.")
            )
        ),
        nav_spacer(),
        nav_item(
            a(
                href = "https://www.linkedin.com/in/jack-sleight-461a6699/",
                target = "_blank",
                bs_icon("linkedin")
            )
        ),
        nav_item(
            a(
                href = "https://github.com/jsleight1/HealthDataScotland",
                target = "_blank",
                bs_icon("github")
            )
        ),
        footer = div(
            class = "footer",
            "Created by Jack Sleight"
        )
    )

    server <- function(input, output) {
        requireNamespace("sf", quietly = TRUE)
        map_selected_data <- map_server("map", data, get_sf("board"))
        data[["General practice"]][["server"]]()
        data[["Hospital"]][["server"]]()
    }

    shinyApp(ui, server, ...)
}