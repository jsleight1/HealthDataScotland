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
                    addAwesomeMarkers(layerId = ~as.character(id))
            })
        }
    )
}

initialise_gp_popup <- function(meta, data, event, ns) {
    obj <- meta %>% 
        filter(.data[["PracticeCode"]] == event[["id"]]) %>% 
        select(-c("HB", "HSCP", "Time")) %>% 
        inner_join(data, by = "PracticeCode") %>% 
        gp[["new"]]()

    showModal(modalDialog(
        gp_UI(obj, ns),
        size = "l",
        easyClose = TRUE
    ))
    gp_server(obj)
}
