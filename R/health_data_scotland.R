#' Launch shiny application
#' @param ... Passed to shiny::shinyApp.
#' @export
health_data_scotland <- function(...) {

    # all_data <- list(process_gp_data(), process_hospital_data()) |> 
    #     set_names(c("General practice", "Hospital")) |>
    #     suppressMessages()

    all_data <- readRDS("data.RDS")

    ui <- dashboardPage(
        dashboardHeader(title = "Health Data Scotland"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard", tabName = "map"), 
                menuItem("References", tabName = "references")
            ),
            collapsed = TRUE
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "map", 
                    h4("This dashboard shows a summary of demographic information
                       for GP practices and hospital bed capacity per specialty across Scotland."),
                    fluidRow(
                        tabsetPanel(
                            map_UI(
                                id = "map", 
                                boards = get_sf("board") |> 
                                    as_tibble() |> 
                                    select("HBName", "id") |> 
                                    tibble::deframe()
                            ),
                            map_comparison_UI(id = "map_comparison"),
                            map_data_UI(id = "map_data")
                        )
                    ),
                    p("Please note that this application was built as a hobby project,
                      therefore should be verified using the referenced published datasets",
                      style = "color:red; padding-top:10px;")
                ), 
                tabItem(
                    tabName = "references",
                    h1("References"), 
                    h2("Data sources"),
                    p(tags$a(href = "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90", " - GP metadata")),
                    p(tags$a(href = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c", " - GP demography data")),
                    p(tags$a(href = "https://data.spatialhub.scot/dataset/gp_practices-is/resource/8389fd1d-563d-4c05-9833-26d9f07fd6cd", " - GP spatial data")),
                    p(tags$a(href = "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0", " - Hospital metadata")),
                    p(tags$a(href = "https://www.opendata.nhs.scot/dataset/7e21f62c-64a1-4aa7-b160-60cbdd8a700d", " - Hospital specialty data")),
                    p(tags$a(href = "https://data.spatialhub.scot/dataset/nhs_hospitals-is/resource/b810d206-45bd-4dff-bac7-110a50b4bd3b", " - Hospital spatial data")),
                    p(tags$a(href = "https://data.gov.uk/dataset/27d0fe5f-79bb-4116-aec9-a8e565ff756a/nhs-health-boards", " - Health board spatial data")),
                    h2("GitHub"),
                    p(tags$a(href = "https://github.com/jsleight1/HealthDataScotland", "HealthDataScotland")),
                    h1("Notes"), 
                    p("Please note that GP practices and hospitals that are included are only those that
                      are present in all three of the metadata, demography/specialty data 
                      and spatial data.")
                )
            )
        )
    )

    server <- function(input, output) {
        selected_data <- map_server("map", all_data, get_sf("board"))
        map_comparison_server("map_comparison", selected_data)
        map_data_server("map_data", all_data)
    }

    shinyApp(ui, server, ...)
}

process_gp_data <- function() {
    sf <- get_sf()

    meta <- get_gp_meta() |> 
        rename("ID" = "PracticeCode") |> 
        mutate_at("ID", as.character) |>
        inner_join(
            select(as_tibble(get_sf("board")), "id", "HBName"),
            by = c("HB" = "id")
        )

    data <- get_gp_data() |> 
        select(-matches("QF$")) |>
        rename("ID" = "PracticeCode") |>
        mutate(
            Date = as.Date(as.character(.data[["Date"]]), format = "%Y%m%d"),
            ID = as.character(.data[["ID"]])
        )

    master_ids <- list(sf[["id"]], meta[["ID"]], data[["ID"]]) |> 
        reduce(intersect)

    sf <- filter(sf, .data[["id"]] %in% master_ids) |> 
        mutate_at("uprn", as.character) |> 
        left_join(meta, by = c("id" = "ID")) |> 
        rename("hbcode" = "HB")
    meta <- filter(meta, .data[["ID"]] %in% master_ids)
    data <- filter(data, .data[["ID"]] %in% master_ids)

    meta[["ID"]] |> 
        map(function(id) {
            meta |> 
                filter(.data[["ID"]] == id) |> 
                select(-"HB", -"HSCP") |>
                inner_join(data, by = "ID") |> 
                gp[["new"]]()
        }) |> 
        set_names(meta[["ID"]]) |>
        gp_grp[["new"]](.sf = sf)
}

process_hospital_data <- function() {
    sf <- get_sf("hospital")
    
    meta <- get_hosp_meta() |> 
        rename("ID" = "HospitalCode") |>
        inner_join(
            select(as_tibble(get_sf("board")), "id", "HBName"),
            by = c("HealthBoard" = "id")
        )
    
    data <- rename(get_hosp_data(), "ID" = "Location")

    master_ids <- list(sf[["id"]], meta[["ID"]], data[["ID"]]) |> 
        reduce(intersect)
    
    sf <- filter(sf, .data[["id"]] %in% master_ids) |> 
        mutate_at("uprn", as.character)
    meta <- filter(meta, .data[["ID"]] %in% master_ids)
    data <- filter(data, .data[["ID"]] %in% master_ids)

    meta[["ID"]] |> 
        map(function(id) {
            meta |> 
                filter(.data[["ID"]] == id) |> 
                inner_join(data, by = "ID") |> 
                hospital[["new"]]()
        }) |> 
        set_names(meta[["ID"]]) |>
        hospital_grp[["new"]](.sf = sf)
}
