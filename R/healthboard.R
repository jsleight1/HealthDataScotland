

healthboard <- R6Class("healthboard", 
    public = list(
        data = list(), 
        initialize = function(data) {
            self[["data"]] = data
            self[["validate"]]()
        }, 
        validate = function() {
            assert_that(inherits(self[["data"]], "list"))
            assert_that(all(map_lgl(self[["data"]], inherits, "gp")))
            self
        }
    )
)
