intro_card <- function() {
  card(
    h3("Welcome to HealthDataScotland"),
    p(
      "
            HealthDataScotland is an R shiny application that presents
            GP practice demographic data and hospital bed capacity data
            across Scotland. The data presented has been obtained from various
            sources (see 'Notes' page) and is available for download in the
            summary pages.
            "
    ),
    p(
      "
            The application is broadly split into two parts: 'Map' and
            'Summary'.
            "
    ),
    p(
      "
            The 'Map' page contains an interactive map visualising
            GP practices and hospitals across Scotland. This map
            can be interogated by clicking a selected GP or hospital. This will
            trigger a pop-up visualising its respective data set.
            "
    ),
    p(
      "
            The 'Summary' tab contains two pages; 'General practice' and
            'Hospital'. Each of these pages visualises the respective data
            set at a national, health board and individual practice
            level. This can be used to compare GP demography or hospital
            bed capacity data across the country.
            "
    )
  )
}

notes_card <- function() {
  card(
    h4("Data sources"),
    p(
      "
            HealthDataScotland is a shiny application that presents
            Scottish GP practice demographic and hospital bed capacity data.
            This data is derived from the NHS open data API maintained
            by Public Health Scotland. The following data sets are used:
            "
    ),
    p("- ", tags$a(href = "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90", target = "_blank", "GP metadata")),
    p("- ", tags$a(href = "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c", target = "_blank", "GP demography data")),
    p("- ", tags$a(href = "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0", target = "_blank", "Hospital metadata")),
    p("- ", tags$a(href = "https://www.opendata.nhs.scot/dataset/7e21f62c-64a1-4aa7-b160-60cbdd8a700d", target = "_blank", "Hospital bed occupancy data")),
    p("In addition, spatial data is derived from the following sources:"),
    p("- ", tags$a(href = "https://data.spatialhub.scot/dataset/nhs_hospitals-is/resource/b810d206-45bd-4dff-bac7-110a50b4bd3b", target = "_blank", "Hospital spatial data")),
    p("- ", tags$a(href = "https://data.spatialhub.scot/dataset/gp_practices-is/resource/8389fd1d-563d-4c05-9833-26d9f07fd6cd", target = "_blank", "GP spatial data")),
    p(
      "
            Please note the following caveats when interpreting data
            in the application:
            "
    ),
    p(
      "
            - Only GP practices and hospitals with matching IDs between
            the metadata and the demography and occupancy data respectively,
            are presented.
            "
    ),
    p(
      "
            - Only GP practices and hospitals with matching IDs between
            the metadata and the spatial data are presented.
            "
    ),
    p(
      "
            - Where data appears to be missing please refer to the original
            published data sets listed above.
            "
    ),
    h4("HealthDataScotland package"),
    p(
      "
            HealthDataScotland is structured as an R package hosted on
            ",
      tags$a(href = "https://github.com/jsleight1/HealthDataScotland", target = "_blank", "GitHub"),
      "If a clear bug is encountered please file an issue with a
            minimal reproducible example and I will endeavour to
            address this. HealthDataScotland also has an associated docker
            container hosted on",
      tags$a(href = "https://hub.docker.com/r/jsleight1/healthdatascotland", target = "_black", "docker.io"),
      "which has installed all the required dependencies to launch the
            application locally."
    ),
    h4("About the author"),
    p(
      "
            I am a Scottish R software developer with a background in
            Bioinformatic and Health data science. I am currently working as an
            R/R shiny developer at Audit Scotland.
            "
    )
  )
}
