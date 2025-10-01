#' Launch shiny application
#' @param ... Passed to shiny::shinyApp.
#' @export
health_data_scotland <- function(...) {
  requireNamespace("sf", quietly = TRUE)

  log_info("Creating initial objects")
  data <- load_processed_data()
  health_unit_grps <- create_data_objects(data)
  map <- create_map_unit(data)
  log_info("Created initial objects")

  value_boxes <- health_unit_grps |>
    purrr::imap(function(x, nm) {
      value_box(
        title = glue("Number of {tolower(nm)}s analysed"),
        showcase = bs_icon("bar-chart"),
        value = length(x[["IDs"]]())
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
      map[["ui"]]()
    ),
    nav_menu(
      title = "Summary",
      health_unit_grps[["gp"]][["ui"]](),
      health_unit_grps[["hospital"]][["ui"]]()
    ),
    nav_panel(
      title = "About",
      class = "overflow-auto",
      about_card(),
      package_card(),
      data_workflow_card()
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
    log_info(
      glue("Launching HealthDataScotland v{packageVersion('HealthDataScotland')}")
    )
    log_info(glue::glue_collapse(list.files(), "\n"))
    map[["server"]](health_unit_grps)
    health_unit_grps[["gp"]][["server"]]()
    health_unit_grps[["hospital"]][["server"]]()
  }

  shinyApp(ui, server, ...)
}
