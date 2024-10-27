map_UI <- function(id, boards) {
    ns <- NS(id)
    tabPanel(
        "Map",
        shinyWidgets::addSpinner(
            column(
                selectInput(
                    ns("board_select"), 
                    label = "Select health board",  
                    choices = boards,
                    multiple = TRUE,
                    selected = boards,
                    width = "100%"
                ),
                checkboxGroupInput(
                    ns("health_select"), 
                    label = "Display health centre types",  
                    choices = c("General practice", "Hospital"), 
                    selected = c("General practice", "Hospital"), 
                    inline = TRUE,
                    width = "100%"
                ),
                leafletOutput(ns("map"), height = 700), 
                width = 11
            ),
            spin = "bounce", 
            color = "#377EB8"
        )
    )
}

map_server <- function(id, data, boards) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]

            pin_json <- reduce(data[["json"]], rbind)
            
            observe({
                leafletProxy(id) %>% clearPopups()
                event <- input[[paste0(id, "_marker_click")]]
                if (is.null(event)) return()
                id <- strsplit(event[["id"]], ":")[[1]][[1]]
                tpe <- strsplit(event[["id"]], ":")[[1]][[2]]
                obj_data <- filter(data, .data[["type"]] == tpe)
                obj <- obj_data[["meta"]][[1]] %>% 
                    filter(.data[["ID"]] == id) %>% 
                    inner_join(obj_data[["data"]][[1]], by = "ID") %>% 
                    obj_data[["initialise"]][[1]][["new"]]()
                isolate(obj[["popup_modal"]](ns))
            })

            pin_data <- reactive({
                health_boards <- input[["board_select"]]
                centre_types <- input[["health_select"]]
                pin_json[pin_json[["type"]] %in% centre_types & pin_json[["hbcode"]] %in% health_boards, ]
            })

            output[["map"]] <- renderLeaflet({
                leaflet() %>% 
                    addTiles() %>% 
                    addAwesomeMarkers(
                        layerId = ~paste0(as.character(id), ":", as.character(type)),
                        icon = ~pin_icon(type),
                        clusterOptions = markerClusterOptions(
                            showCoverageOnHover = FALSE
                        ), 
                        data = pin_data()
                    ) %>%
                    addPolygons(
                        fillOpacity = 0.01,
                        smoothFactor = 0.1, 
                        weight = 2,
                        color = "#377EB8",
                        label = as_tibble(boards)[["HBName"]],
                        layerId = ~as.character(id), 
                        data = boards
                    )
            })
        }
    )
}

pin_icon <- function(type, ...) {
    awesomeIcons(
        icon = 'ios-close',
        iconColor = 'white',
        library = 'ion',
        markerColor = ifelse(type == "General practice", "blue", "red"),
        ...
    )
}
