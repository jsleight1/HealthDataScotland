capture_output(board_sf <- get_sf("board"))

gp_data <- HealthDataScotland::example_gp_metadata |> 
    select(-c("HB", "HSCP")) |>
    inner_join(HealthDataScotland::example_gp_data, by = "PracticeCode") |> 
    rename("ID" = "PracticeCode") |> 
    inner_join(
        select(as_tibble(board_sf), "ID", "HBName"),
        by = c("HB" = "ID")
    )

hospital_data <- HealthDataScotland::example_hospital_data |> 
    inner_join(
        HealthDataScotland::example_hospital_metadata, 
        by = c("Location" = "HospitalCode")
    ) |> 
    rename("ID" = "Location") |> 
    inner_join(
        select(as_tibble(board_sf), "ID", "HBName"),
        by = c("HB" = "ID")
    )
