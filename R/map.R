mapUI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Map",
        leafletOutput(ns(id), height = 700),
    )
}

mapServer <- function(id, json) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session[["ns"]]
            output[[id]] <- renderLeaflet({
                leaflet(json) %>% 
                    addTiles() %>% 
                    addAwesomeMarkers(
                        popup = ~as.character(prac_code), 
                        label = ~as.character(prac_code),
                        icon = icon("tags")
                    )
            })
        }
    )
}