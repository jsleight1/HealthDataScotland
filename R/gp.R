

gp <- R6Class("gp", 
    private = list(
        required_cols = function() {
            c("Quarter",  "Practice", "Name", "Board", "HSCP", "Cluster", 
                "AgeGroup", "Sex", "Total Patients", "LessThan1Year",
                "GreaterThan1Year")
        }
    ),
    public = list(
        data = NA,
        initialize = function(data) {
            self[["data"]] = data
            self[["validate"]]()
        }, 
        validate = function() {
            assert_that(inherits(self[["data"]], "data.frame"))
            assert_that(all(private[["required_cols"]]() %in% colnames(self[["data"]])))
            assert_that(length(unique(self[["data"]][["Name"]])) == 1)
            self
        }
    )
)
