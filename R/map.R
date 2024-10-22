pin_map_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Map",
        shinyWidgets::addSpinner(
            leafletOutput(ns(id), height = 700),
            spin = "bounce", 
            color = "#377EB8"
        )
    )
}

pin_map_server <- function(id, json, meta, data, initialise_popup) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session[["ns"]]

            observe({
                leafletProxy(id) %>% clearPopups()
                event <- input[[paste0(id, "_marker_click")]]
                if (is.null(event)) return()
                isolate({initialise_popup(meta, data, event, ns)})
            })

            output[[id]] <- renderLeaflet({
                leaflet(json) %>% 
                    addTiles() %>% 
                    addAwesomeMarkers(
                        layerId = ~as.character(id),
                        clusterOptions = markerClusterOptions()
                    )
            })
        }
    )
}

grid_map_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Map",
        shinyWidgets::addSpinner(
            leafletOutput(ns(id), height = 700),
            spin = "bounce", 
            color = "#377EB8"
        )
    )
}

grid_map_server <- function(id, json, meta, data, initialise_popup) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session[["ns"]]

            observe({
                leafletProxy(id) %>% clearPopups()
                event <- input[[paste0(id, "_shape_click")]]
                if (is.null(event)) return()
                isolate({initialise_popup(meta, data, event, ns)})
            })

            output[[id]] <- renderLeaflet({
                leaflet(json) %>%
                    addTiles() %>%
                    addPolygons(
                        fillOpacity = 0.01,
                        smoothFactor = 0.1, 
                        color = "#377EB8",
                        label = as_tibble(json)[["HBName"]],
                        layerId = ~as.character(id)
                    )
            })
        }
    )
}
