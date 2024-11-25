map_UI <- function(id, boards) {
    ns <- NS(id)
    tabPanel(
        "Map",
        pickerInput(
            inputId = ns("board_select"),
            label =  "Select health board",  
            choices = boards,
            selected = boards,
            inline = TRUE,
            multiple = TRUE, 
            options = list(
                `actions-box` = TRUE, 
                `selected-text-format` = "count > 1"
            )
        ),
        pickerInput(
            ns("health_select"), 
            label = "Display health centre types",  
            choices = c("General practice", "Hospital"), 
            selected = c("General practice", "Hospital"), 
            inline = TRUE,
            multiple = TRUE
        ),
        spinner(leafletOutput(ns("map"), height = 700)),
        width = 12
    )
}

map_server <- function(id, data, boards) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]

            pin_sf <- data |> 
                map(~.x[["sf"]]()) |> 
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

            selected_pins <- reactiveVal() 
            observeEvent(input[[paste0(id, "_draw_new_feature")]], {
                out <- find_locations(
                    shape = input[[paste0(id, "_draw_new_feature")]], 
                    location_coordinates = sf::as_Spatial(pin_data()),
                    location_id_colnames = c("ID", "type")
                )
                if (is.null(selected_pins())) {
                    selected_pins(out)
                } else {
                    selected_pins(bind_rows(out, selected_pins()))
                }
            })

            observeEvent(input[[paste0(id, "_draw_deleted_features")]], {
                selected_pins(NULL)
            })

            selected_data <- reactiveVal()
            observe({
                x <- selected_pins()
                if (is.null(x) || !length(x)) {
                    selected_data(NULL)
                } else {
                    selected_data(subset_selected_data(x, data))
                }
            })

            output[["map"]] <- renderLeaflet({
                leaflet() |> 
                    addTiles() |> 
                    addAwesomeMarkers(
                        layerId = ~paste0(as.character(ID), ":", as.character(type)),
                        icon = ~pin_icon(type),
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
                        layerId = ~as.character(ID), 
                        data = boards
                    ) |>
                    leaflet.extras::addDrawToolbar(
                        polylineOptions = FALSE,
                        markerOptions = FALSE,
                        circleMarkerOptions = FALSE,
                        polygonOptions = leaflet.extras::drawPolygonOptions(
                            shapeOptions = leaflet.extras::drawShapeOptions(
                                fillOpacity = 0, 
                                color = "red"
                            )
                        ),
                        rectangleOptions = leaflet.extras::drawRectangleOptions(
                            shapeOptions = leaflet.extras::drawShapeOptions(
                                fillOpacity = 0, 
                                color = "red"
                            )
                        ),
                        circleOptions = FALSE,
                        editOptions = leaflet.extras::editToolbarOptions(
                            edit = FALSE
                        )
                    )
            })

            return(selected_data)
        }
    )
}

pin_icon <- function(type, ...) {
    awesomeIcons(
        icon = "ios-close",
        iconColor = "white",
        library = "ion",
        markerColor = ifelse(type == "General practice", "blue", "red"),
        ...
    )
}

# Based on findLocations in geoshaper (https://github.com/RedOakStrategic/geoshaper/blob/master/R/findLocations.R)
find_locations <- function(shape, location_coordinates, location_id_colnames) {
    polygon_coordinates <- shape[["geometry"]][["coordinates"]]
    feature_type <- shape[["properties"]][["feature_type"]]

    drawn_polygon <- sp::Polygon(
        do.call(
            rbind,
            lapply(polygon_coordinates[[1]], function(x){
                c(x[[1]][1],x[[2]][1])
            })
        )
    )

    selected_locs <- sp::over(
        location_coordinates, 
        sp::SpatialPolygons(
            list(sp::Polygons(list(drawn_polygon),"drawn_polygon"))
        )
    )

    out <- location_coordinates[which(!is.na(selected_locs)), location_id_colnames]
    as_tibble(out)
}

subset_selected_data <- function(x, data) {
    x <- group_split(x, .data[["type"]])
    names(x) <- map_chr(x, ~unique(.x[["type"]]))
    map(x, ~data[[unique(.x[["type"]])]][["subset"]](.x[["ID"]]))
}

map_comparison_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Comparison",
        fluidRow(uiOutput(ns("comparison_boxes")))
    )
}

map_comparison_server <- function(id, data) {
     moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]
            output[["comparison_boxes"]] <- renderUI({
                if (is.null(data())) {
                    show_alert(
                        title = "Warning",
                        text = "No data selected",
                        type = "warning"
                    )
                } else {
                    if (any(purrr::map_lgl(data(), ~length(.x[["ids"]]()) > 10))) {
                        show_alert(
                            title = "Warning",
                            text = "
                                Be aware that selecting > 10 datasets can cause
                                issues with data presentation.
                            ",
                            type = "warning"
                        )
                    }
                    purrr::walk(data(), ~.x[["server"]]())
                    purrr::map(data(), ~.x[["ui"]](ns))
                }
            })
        }
    )
}

spinner <- function(...) {
    addSpinner(..., spin = "bounce", color = "#377EB8")
}