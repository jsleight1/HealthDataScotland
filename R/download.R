download_UI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("download_uis"))
}

download_server <- function(id, data) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session[["ns"]]
            output[["download_uis"]] <- renderUI({
                purrr::walk(data, ~.x[["download_server"]]())
                fluidRow(
                    purrr::imap(data, ~.x[["download_ui"]](ns, .y))
                )
            })
        }
    )
}
