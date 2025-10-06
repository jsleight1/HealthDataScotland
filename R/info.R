intro_card <- function() {
  card(
    h3("Welcome to HealthDataScotland"),
    p(
      "HealthDataScotland is an R shiny application that presents
      GP practice demographic and hospital bed capacity data
      across Scotland. The data presented has been obtained from various
      sources (see 'About' page) and is made available for download.
      "
    ),
    p(
      "The application is broadly split into two parts: 'Map' and
      'Summary'.
      "
    ),
    p(
      "The 'Map' page contains an interactive map visualising
      GP practices and hospitals across Scotland. This map
      can be interogated by clicking a selected GP or hospital, which will
      trigger a pop-up visualising the respective data set.
      "
    ),
    p(
      "The 'Summary' tab contains two pages; 'General practice' and
      'Hospital'. Each of these pages visualises the respective data
      set at a national, health board and individual location
      level. This can be used to compare GP demography or hospital
      bed capacity data across the country.
      "
    )
  )
}

about_panel <- function() {
  nav_panel(
    title = "About the author",
    card(
      min_height = "400px",
      layout_columns(
        img(src = "www/author_pic.jpg"),
        div(
          p(
            "I am an R software developer with a background in bioinformatics
            based in Glasgow, Scotland. I have always had a keen interest in
            public health and how tools such as R shiny can help visualise
            complex health-related data sets."
          ),
          p(
            "The aim of HealthDataScotland was to create a tool for visualising
            GP and hospital related statistics that may be of interest
            to a wider audience. Developing this application was also a way
            of aiding my own personal development in creating, maintaining and
            deploying public R shiny applications."
          ),
          p(
            "I hope that you find this application useful. Please feel free
            to connect with me on linkedin."
          )
        )
      )
    )
  )
}

package_panel <- function() {
  nav_panel(
    title = "HealthDataScotland R package",
    card(
      p(
        "HealthDataScotland is structured as an R package, which is
        hosted on GitHub at",
        tags$a(href = "https://github.com/jsleight1/HealthDataScotland", target = "_blank", "HealthDataScotland"),
        ". HealthDataScotland also has an associated docker container hosted on",
        tags$a(href = "https://hub.docker.com/r/jsleight1/healthdatascotland", target = "_black", "docker.io"),
        " For more information regarding the R package please refer to the ",
        tags$a(href = "https://jsleight1.github.io/HealthDataScotland/", target = "_blank", "documentation."),
      )
    )
  )
}

data_workflow_panel <- function() {
  nav_panel(
    title = "Data workflow",
    card(
      p(
        "The data used in HealthDataScotland is derived from the NHS open
        data API and is maintained by Public Health Scotland. Data is derived
        from the following links:
        "
      ),
      p("- ", tags$a(href = "https://www.opendata.nhs.scot/dataset/gp-practice-contact-details-and-list-sizes", target = "_blank", "GP metadata")),
      p("- ", tags$a(href = "https://www.opendata.nhs.scot/dataset/gp-practice-populations", target = "_blank", "GP demography data")),
      p("- ", tags$a(href = "https://www.opendata.nhs.scot/dataset/hospital-codes", target = "_blank", "Hospital metadata")),
      p("- ", tags$a(href = "https://www.opendata.nhs.scot/dataset/annual-hospital-beds-information", target = "_blank", "Hospital bed occupancy data")),
      p("In addition, spatial data is derived from the Spatial hub at the following links:"),
      p("- ", tags$a(href = "https://data.spatialhub.scot/dataset/nhs_hospitals-is/resource/b810d206-45bd-4dff-bac7-110a50b4bd3b", target = "_blank", "Hospital spatial data")),
      p("- ", tags$a(href = "https://data.spatialhub.scot/dataset/gp_practices-is/resource/8389fd1d-563d-4c05-9833-26d9f07fd6cd", target = "_blank", "GP spatial data")),
      p(
        " Further details on the data workflow can be found using the HealthDataScotland ",
        tags$a(href = "https://jsleight1.github.io/HealthDataScotland/", target = "_blank", "documentation.")
      )
    )
  )
}
