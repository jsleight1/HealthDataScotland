map_UI <- function(id, boards) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(
      "Interactive map",
      popover(
        bs_icon("question-circle"),
        "The interactive map below can be used to
                click on indivdual GP practice and hospital to view associated
                data for that particular health centre."
      ),
      popover(
        bs_icon("gear", class = "ms-auto"),
        virtualSelectInput(
          inputId = ns("board_select"),
          label = "Select health board",
          choices = boards,
          selected = boards,
          inline = TRUE,
          multiple = TRUE,
          search = TRUE,
          html = TRUE,
          showSelectedOptionsFirst = TRUE,
          updateOn = "close"
        ),
        virtualSelectInput(
          ns("health_select"),
          label = "Display health centre types",
          choices = c("General practice" = "gp", "Hospital" = "hospital"),
          selected = c("gp", "hospital"),
          inline = TRUE,
          multiple = TRUE,
          search = TRUE,
          html = TRUE,
          showSelectedOptionsFirst = TRUE,
          updateOn = "close"
        ),
        title = "Map settings"
      )
    ),
    leafletOutput(ns("map"), width = "100%", height = "100%")
  )
}

map_server <- function(id, data, boards) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session[["ns"]]

      pin_sf <- data |>
        map(~ .x[["sf"]]()) |>
        reduce(bind_rows)

      observe({
        leafletProxy(id) |> clearPopups()
        event <- input[[paste0(id, "_marker_click")]]
        if (is.null(event)) return()
        id <- strsplit(event[["id"]], ":")[[1]][[1]]
        type <- strsplit(event[["id"]], ":")[[1]][[2]]
        obj <- data[[type]][["health_unit"]](id)
        isolate(obj[["popup_modal"]](ns))
      })

      pin_data <- reactive({
        health_boards <- input[["board_select"]]
        centre_types <- input[["health_select"]]
        pin_sf[pin_sf[["type"]] %in% centre_types & pin_sf[["hbcode"]] %in% health_boards, ]
      })

      output[["map"]] <- renderLeaflet({
        leaflet() |>
          addTiles() |>
          addAwesomeMarkers(
            layerId = ~ paste0(as.character(ID), ":", as.character(type)),
            icon = ~ pin_icon(type),
            clusterOptions = markerClusterOptions(
              showCoverageOnHover = FALSE
            ),
            data = pin_data()
          ) |>
          addPolygons(
            fillOpacity = 0.01,
            smoothFactor = 0.1,
            weight = 2,
            color = "#377EB8",
            label = as_tibble(boards)[["HBName"]],
            layerId = ~ as.character(ID),
            data = boards
          )
      })
    }
  )
}

pin_icon <- function(type, ...) {
  awesomeIcons(
    icon = "ios-close",
    iconColor = "white",
    library = "ion",
    markerColor = ifelse(type == "gp", "blue", "red"),
    ...
  )
}
