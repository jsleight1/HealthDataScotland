hospital <- R6Class("hospital", 
    inherit = health_unit,
    private = list(
        id_col = function() {
            "Location"
        },
        title_col = function() {
            "HospitalName"
        },
        required_cols = function() {
            c("Location", "FinancialYear", "SpecialtyName", "AllStaffedBeds")
        },
        specialty_bar = function(specialties = "All Specialties") {
            plot <- self[["data"]] %>% 
                filter(.data[["SpecialtyName"]] %in% specialties) %>% 
                ggplot(
                    aes(
                        x = .data[["FinancialYear"]], 
                        y = .data[["AllStaffedBeds"]], 
                        fill = .data[["SpecialtyName"]]
                    )
                ) + 
                    geom_bar(stat = "identity") + 
                    theme_bw() + 
                    theme(axis.text.x = element_text(angle = 90))
            ggplotly(plot, tooltip = c("FinancialYear", "SpecialtyName", "AllStaffedBeds"))
        }
    ),
    public = list(
        available_plots = function() {
            c("specialty_bar")
        },
        plot = function(type, ...) {
            type <- arg_match(type, values = self[["available_plots"]]())
            switch(type, 
                "specialty_bar" = private[["specialty_bar"]](...)
            )
        }
    )
)

hosp_UI <- function(x, ns) {
    ns <- NS(ns(x[["id"]]()))
    fluidRow(
        box(
            title = x[["title"]](), 
            width = 12, 
            status = "primary",
            solidHeader = TRUE,
            fluidRow(box(title = "Address", x[["address"]](), width = 12))
        ),
        fluidRow(
            box(
                title = "All specialties",
                plotlyOutput(ns("all_specialty")),
                width = 12
            ), 
        ),
        fluidRow(
            box(
                title = "Selected specialties",
                selectInput(
                    ns("specialty_select"), 
                    label = "Select specialty", 
                    choices = setdiff(unique(x[["data"]][["SpecialtyName"]]), 
                        "All Specialties"), 
                    multiple = TRUE, 
                    selected = setdiff(unique(x[["data"]][["SpecialtyName"]]),
                        "All Specialties")[[1]]
                ),
                plotlyOutput(ns("selected_specialties")),
                width = 12
            )
        )
    )
}

hosp_server <- function(x) {
    moduleServer(
        x[["id"]](),
        function(input, output, session) {
            ns <- session[["ns"]]

            output[["all_specialty"]] <- renderPlotly(
                x[["plot"]](type = "specialty_bar")
            )

            output[["selected_specialties"]] <- renderPlotly(
                x[["plot"]](
                    type = "specialty_bar",
                    specialties = req(input[["specialty_select"]])
                )
            )
        }
    )
}

initialise_hosp_popup <- function(meta, data, event, ns) {
    obj <- data %>% 
        filter(.data[["Location"]] == event[["id"]]) %>% 
        inner_join(meta, by = c("Location" = "HospitalCode")) %>% 
        hospital[["new"]]()

    showModal(modalDialog(
        hosp_UI(obj, ns),
        size = "l",
        easyClose = TRUE
    ))
    hosp_server(obj)
}