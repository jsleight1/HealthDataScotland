mapUI <- function(id) {
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

mapServer <- function(id, json, meta, data) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session[["ns"]]

            observe({
                leafletProxy("map") %>% clearPopups()
                event <- input$map_marker_click
                if (is.null(event)) return()
                isolate({
                    gp_obj <- meta %>% 
                        filter(PracticeCode == event[["id"]]) %>% 
                        select(-c("HB", "HSCP", "Time")) %>% 
                        inner_join(data, by = "PracticeCode") %>% 
                        gp[["new"]](data = .)

                    showModal(modalDialog(
                        gpUI(gp_obj, ns),
                        size = "l",
                        easyClose = TRUE
                    ))
                    gpServer(gp_obj)
                })
            })

            output[["map"]] <- renderLeaflet({
                leaflet(json) %>% 
                    addTiles() %>% 
                    addAwesomeMarkers(
                        layerId = ~as.character(prac_code)
                    )
            })
        }
    )
}