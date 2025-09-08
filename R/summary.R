summary_UI <- function(id, title, data) {
    ns <- NS(id)
    nav_panel(
        full_screen = TRUE,
        card_header(title),
        layout_sidebar(
            sidebar = list(
                radioButtons(
                    inputId = ns("select_group_by"),
                    label = glue("Select {title} data by:"),
                    choices = data[["group_choices"]]()
                ),
                uiOutput(ns("selects")),
                actionButton(
                    inputId = ns("create_output"),
                    label = "Create comparisons"
                )
            ),
            uiOutput(ns("summary_boxes"))
        )
    )
}

summary_server <- function(id, title, data) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session[["ns"]]

            group_col <- reactive({
                data[["group_column"]](input[["select_group_by"]])
            }) |>
            bindEvent(input[["select_group_by"]])

            group_options <- reactive({
                data[["get_download"]]() |>
                    pull(group_col()) |>
                    unique()
            }) |>
            bindEvent(input[["select_group_by"]])

            group_ids <- reactive({
                data[["get_download"]]() |>
                    filter(.data[[group_col()]] %in% input[["select_group"]]) |>
                    pull("ID") |>
                    unique()
            }) |>
            bindEvent(input[["select_group"]])

            observe({
                req(input[["select_group_by"]])
                if (input[["select_group_by"]] != "National") {
                    output[["selects"]] <- renderUI({
                        list(
                            virtualSelectInput(
                                inputId = ns("select_group"),
                                label = paste0("Select ", input[["select_group_by"]]),
                                choices = group_options(),
                                search = TRUE,
                                showSelectedOptionsFirst = TRUE,
                                updateOn = "close"
                            ),
                            virtualSelectInput(
                                inputId = ns("select_id"),
                                label = glue("Select {title} by ID"),
                                multiple = TRUE,
                                choices = NULL,
                                search = TRUE,
                                showSelectedOptionsFirst = TRUE,
                                updateOn = "close"
                            )
                        )
                    })
                } else {
                    output[["selects"]] <- renderUI({
                        list(
                            virtualSelectInput(
                                inputId = ns("select_id"),
                                label = glue("Select {title} by ID"),
                                multiple = TRUE,
                                choices = setNames(
                                    data[["ids"]](),
                                    paste(
                                        data[["ids"]](),
                                        "-",
                                        data[["titles"]]()
                                    )
                                ),
                                selected = data[["ids"]](),
                                search = TRUE,
                                showSelectedOptionsFirst = TRUE,
                                updateOn = "close"
                            )
                        )
                    })
                }
            }) |>
            bindEvent(input[["select_group_by"]])

            observe({
                req(input[["select_group"]])
                ids <- group_ids()

                sub_data <- data[["subset"]](id = ids)

                id_options <- setNames(
                    sub_data[["ids"]](),
                    paste(
                        sub_data[["ids"]](),
                        "-",
                        sub_data[["titles"]]()
                    )
                ) |>
                as.list()
                updateVirtualSelect(
                    inputId = "select_id",
                    choices = setNames(
                        sub_data[["ids"]](),
                        paste(
                            sub_data[["ids"]](),
                            "-",
                            sub_data[["titles"]]()
                        )
                    ),
                    selected = sub_data[["ids"]]()
                )
            }) |>
            bindEvent(input[["select_group"]])

            selected_data <- reactive({
                data[["subset"]](id = input[["select_id"]])
            }) |>
            bindEvent(input[["create_output"]])

            observe({
                req(input[["select_id"]])
                if (length(input[["select_id"]]) > 10) {
                    confirmSweetAlert(
                        inputId = ns("confirm_comparison"),
                        title = "Warning",
                        text = "
                            Please be aware that selecting > 10 datasets can cause
                            issues with data performance and presentation.
                        ",
                        type = "warning"
                    )
                } else {
                    output[["summary_boxes"]] <- renderUI({
                        selected_data()[["server"]]()
                        selected_data()[["ui"]](ns)
                    })
                }
            }) |>
            bindEvent(input[["create_output"]])

            observe({
                if (input[["confirm_comparison"]]) {
                    output[["summary_boxes"]] <- renderUI({
                        selected_data()[["server"]]()
                        selected_data()[["ui"]](ns)
                    })
                }
            }) |>
            bindEvent(input[["confirm_comparison"]])
        }
    )
}