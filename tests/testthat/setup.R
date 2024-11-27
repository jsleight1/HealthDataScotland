capture_output(board_sf <- get_sf("board"))

gp_data <- HealthDataScotland::example_gp_metadata |> 
    select(-c("HB", "HSCP")) |>
    rename("PHS metadata ID" = "datasetID") |>
    inner_join(HealthDataScotland::example_gp_data, by = "PracticeCode") |> 
    rename("ID" = "PracticeCode", "PHS data ID" = "datasetID") |> 
    inner_join(
        select(as_tibble(board_sf), "ID", "HBName"),
        by = c("HB" = "ID")
    )

hospital_data <- HealthDataScotland::example_hospital_metadata |> 
    rename("PHS metadata ID" = "datasetID") |>
    inner_join(
        HealthDataScotland::example_hospital_data, 
        by = c("HospitalCode" = "Location")
    ) |> 
    rename("ID" = "HospitalCode", "PHS data ID" = "datasetID") |> 
    inner_join(
        select(as_tibble(board_sf), "ID", "HBName"),
        by = c("HB" = "ID")
    )
