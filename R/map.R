map_UI <- function(id) {
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

map_server <- function(id, data) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]
            observe({
                leafletProxy(id) %>% clearPopups()
                event <- input[[paste0(id, "_marker_click")]]
                if (is.null(event)) return()
                id <- strsplit(event[["id"]], ":")[[1]][[1]]
                type <- strsplit(event[["id"]], ":")[[1]][[2]]
                func = switch(type, "gp" = gp, "hosp" = hospital)
                obj <- data[[type]][["meta"]] %>% 
                    filter(.data[["ID"]] == id) %>% 
                    inner_join(data[[type]][["data"]], by = "ID") %>% 
                    func[["new"]]()
                isolate(obj[["popup_modal"]](ns))
            })

            output[[id]] <- renderLeaflet({
                leaflet() %>% 
                    addTiles() %>% 
                    addAwesomeMarkers(
                        layerId = ~paste0(as.character(id), ":", as.character(type)),
                        icon = pin_icon("blue"),
                        clusterOptions = markerClusterOptions(
                            showCoverageOnHover = FALSE
                        ), 
                        data = data[["gp"]][["json"]]
                    ) %>%
                    addAwesomeMarkers(
                        layerId = ~paste0(as.character(id), ":", as.character(type)),
                        icon = pin_icon("red"),
                        clusterOptions = markerClusterOptions(
                            showCoverageOnHover = FALSE
                        ), 
                        data = data[["hosp"]][["json"]]
                    ) %>%
                    addPolygons(
                        fillOpacity = 0.01,
                        smoothFactor = 0.1, 
                        weight = 2,
                        color = "#377EB8",
                        label = as_tibble(data[["board"]])[["HBName"]],
                        layerId = ~as.character(id), 
                        data = data[["board"]]
                    )
            })
        }
    )
}

pin_icon <- function(colour, ...) {
    awesomeIcons(
        icon = 'ios-close',
        iconColor = 'white',
        library = 'ion',
        markerColor = colour,
        ...
    )
}