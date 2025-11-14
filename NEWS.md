# HealthDataScotland 2.2.0

* Add `data_type` and `specialties` to `hospital` object specialty summary.
* Remove dark mode

# HealthDataScotland 2.1.1

* Update package disclaimer

# HealthDataScotland 2.1.0

* Add `$summary` and associated helper functions for summarising `gp`, `hospital`, `gp_grp` and `hospital_grp` objects in table format.

# HealthDataScotland 2.0.0

* Refactor UI of shiny application to use `bslib` dashboard structure.
* Refactor UI to contain home, map, summary and notes pages.
* Refactor UI to use `brand.yml`.
* Convert `plotly` outputs to `echarts4r` outputs across all objects.
* Refactor `hospital_grp` and `gp_grp` objects to summarise health statistics at the national and health board level.
* Refactor data structure of `health_unit` object to contain separate metadata and data `data.frame` objects.
* Create `map` R6 class for creating interactive map.
* Use `Azure` blob external data store to house processed data set.
* Remove map pin box selection functionality.
* Add `example_` gp/gp_grp and hospital/hospital_grp object functions.

# HealthDataScotland 1.1.1

* Build docker image associated with HealthDataScotland shiny application.

# HealthDataScotland 1.1.0

* Add annual and daily hospital bed statistics using 'specialty_line' plot option for `hospital` and `hospital_grp` objects.

# HealthDataScotland 1.0.0

* First release of `HealthDataScotland`. This package generates a shiny application for the processing and presentation of GP and Hospital data across Scotland. Data is currently processed locally and saved to an RDS file. The R package and RDS
file are then hosted on [shinyio](https://jack-sleight.shinyapps.io/healthdatascotland/) using rsconnect.
