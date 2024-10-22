board_data <- HealthDataScotland::example_board_data %>% 
    inner_join(HealthDataScotland::example_board_metadata, by = c("HB"))

board_unit <- board_data %>% 
    filter(.data[["HB"]] == "S92000003") %>%
    board[["new"]]()

test_that("board class works", {
    "board_data" %>% 
        board[["new"]]() %>% 
        expect_error("Data set must be in data.frame")
      
    board_data %>%
        board[["new"]]() %>% 
        expect_error("Data set must contain only one unique HB")

    board_data %>% 
        select(-"HB") %>% 
        board[["new"]]() %>% 
        expect_error("HB column missing from data")

    out <- board_data %>% 
        filter(.data[["HB"]] == "S92000003") %>%
        board[["new"]]() %>% 
        expect_error(NA)

    expect_true(inherits(out, "board"))
    expect_identical(out[["id"]](), "S92000003")
    expect_identical(out[["title"]](), "S92000003")
    expect_null(out[["address"]]())
    expect_identical(out[["available_plots"]](), "specialty_bar")
})

test_that("board class can be plotted", {
    expect_s3_class(board_unit[["plot"]](type = "specialty_bar"), "plotly")
})