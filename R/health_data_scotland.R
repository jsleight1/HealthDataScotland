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
            notes_card()
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
        map_server("map", data, get_sf("board"))
        data[["General practice"]][["server"]]()
        data[["Hospital"]][["server"]]()
    }

    shinyApp(ui, server, ...)
}