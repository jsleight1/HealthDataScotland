gp_map_UI <- function(id) {
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

gp_map_server <- function(id, json, meta, data) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session[["ns"]]

            observe({
                leafletProxy(id) %>% clearPopups()
                event <- input[[paste0(id, "_marker_click")]]
                if (is.null(event)) return()
                isolate({
                    gp_obj <- meta %>% 
                        filter(.data[["PracticeCode"]] == event[["id"]]) %>% 
                        select(-c("HB", "HSCP", "Time")) %>% 
                        inner_join(data, by = "PracticeCode") %>% 
                        gp[["new"]]()

                    showModal(modalDialog(
                        gpUI(gp_obj, ns),
                        size = "l",
                        easyClose = TRUE
                    ))
                    gpServer(gp_obj)
                })
            })

            output[[id]] <- renderLeaflet({
                leaflet(json) %>% 
                    addTiles() %>% 
                    addAwesomeMarkers(
                        layerId = ~as.character(prac_code)
                    )
            })
        }
    )
}