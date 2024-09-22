
#' R6 class representing a health unit e.g. a GP practice or hospital.
health_unit <- R6Class("health_unit", 
    public = list(
        #' @field data A data.frame storing health statistics.
        data = NA, 
        initialize = function(data) {
            self[["data"]] = data
            self[["validate"]]()
        }, 
        validate = function() {
            assert_that(inherits(self[["data"]], "data.frame"), 
                msg = "Data set must be in data.frame")
            col_check <- private[["required_cols"]]() %in% colnames(self[["data"]])
            assert_that(all(col_check), 
                msg = paste(
                    paste(private[["required_cols"]]()[!col_check], collapse = ", "), 
                    "column missing from data"
                )
            )    
            assert_that(length(unique(self[["data"]][[private[["id_col"]]()]])) == 1, 
                msg = paste("Data set must contain only one unique", 
                    private[["id_col"]]())
            )   
            self
        },
        id = function() {
            unique(self[["data"]][[private[["id_col"]]()]])
        },
        title = function() {
            paste(unique(self[["data"]][[private[["title_col"]]()]]))
        },
        address = function() {
            self[["data"]] %>% 
                select(contains("Address"), -contains("QF"), "Postcode") %>% 
                select_if(~ !any(is.na(.))) %>%
                distinct() %>% 
                tidyr::unite(col = "x", sep = ", ") %>% 
                pull("x")
        }
    )
)