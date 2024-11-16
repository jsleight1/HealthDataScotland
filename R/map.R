map_UI <- function(id, boards) {
    ns <- NS(id)
    tabPanel(
        "Map",
        column(
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
            addSpinner(
                leafletOutput(ns("map"), height = 700), 
                spin = "bounce", 
                color = "#377EB8"
            ),
            width = 12
        )
    )
}

map_server <- function(id, data, boards) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]

            pin_sf <- data |> 
                map(function(i) i[["sf"]]()) |> 
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

            selected_data <- reactiveVal()
            observeEvent(input[[paste0(id, "_draw_all_features")]], {
                if (identical(input[[paste0(id, "_draw_all_features")]][["features"]], list())) {
                    selected_data(NULL)
                } else {
                    dat <- format_selected_data(
                        data, 
                        pin_data(),
                        input[[paste0(id, "_draw_all_features")]][["features"]][[1]]
                    )
                    selected_data(dat)
                }
            })

            output[["map"]] <- renderLeaflet({
                leaflet() |> 
                    addTiles() |> 
                    addAwesomeMarkers(
                        layerId = ~paste0(as.character(id), ":", as.character(type)),
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
                        layerId = ~as.character(id), 
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

format_selected_data <- function(x, pins, input) {
    cloned_data <- map(x, ~.x[["clone"]](deep = TRUE))
    dat <- find_locations(
            shape = input, 
            location_coordinates = sf::as_Spatial(pins),
            location_id_colnames = c("id", "type")
        ) |>
        group_split(.data[["type"]])
    names(dat) <- map_chr(dat, ~unique(.x[["type"]]))
    dat <- dat |>
        map(~{cloned_data[[unique(.x[["type"]])]][["subset"]](.x[["id"]])})
    dat
}

map_comparison_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Comparison",
        column(
            uiOutput(ns("comparison_boxes")),
            width = 12
        )
    )
}

map_comparison_server <- function(id, data) {
     moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]
            output[["comparison_boxes"]] <- renderUI({
                purrr::iwalk(data(), ~.x[["server"]](id = ns(.y)))
                tagList(purrr::imap(data(), ~.x[["ui"]](ns, id = ns(.y))))
            })
        }
    )
}

map_data_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Data",
        column(
            addSpinner(
                reactable::reactableOutput(ns("table"), height = 700), 
                spin = "bounce", 
                color = "#377EB8"
            ),
            tags[["button"]](
                "Download as CSV", 
                onclick = paste0(
                    "Reactable.downloadDataCSV('", 
                    ns("health_data"), 
                    "', 'health_data.csv')"
                )
            ),
            width = 12
        )
    )
}

map_data_server <- function(id, data) {
    moduleServer(
        id, 
        function(input, output, session) {
            ns <- session[["ns"]]
            output[["table"]] <- reactable::renderReactable({
                map(data, ~.x[["get_download"]]()) |>
                    bind_rows(.id = "Health Centre Type") |>
                    reactable::reactable(
                        filterable = TRUE, 
                        elementId = ns("health_data")
                    )
            })
        }
    )
}

