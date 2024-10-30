capture_output(board_json <- get_geojson("board"))

gp_data <- HealthDataScotland::example_gp_metadata %>% 
    select(-c("HB", "HSCP", "Time")) %>%
    inner_join(HealthDataScotland::example_gp_data, by = "PracticeCode") %>% 
    rename("ID" = "PracticeCode") %>% 
    inner_join(
        select(as_tibble(board_json), "id", "HBName"),
        by = c("HB" = "id")
    )

hospital_data <- HealthDataScotland::example_hospital_data %>% 
    inner_join(
        HealthDataScotland::example_hospital_metadata, 
        by = c("Location" = "HospitalCode")
    ) %>% 
    rename("ID" = "Location") %>% 
    inner_join(
        select(as_tibble(board_json), "id", "HBName"),
        by = c("HB" = "id")
    )
