#' Launch shiny application
#' @param ... Passed to shiny::shinyApp.
#' @export
health_data_scotland <- function(...) {
  requireNamespace("sf", quietly = TRUE)
  log_info("Creating data objects")
  data <- create_data_objects(load_processed_data())
  log_info("Created data objects")

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
      intro_card(),
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
            deframe()
        )
      )
    ),
    nav_menu(
      title = "Summary",
      data[["gp"]][["ui"]](),
      data[["hospital"]][["ui"]]()
    ),
    nav_panel(
      title = "Notes",
      notes_card()
    ),
    nav_spacer(),
    nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
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
    data[["gp"]][["server"]]()
    data[["hospital"]][["server"]]()
  }

  shinyApp(ui, server, ...)
}
