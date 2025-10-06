intro_card <- function() {
  card(
    h3("Welcome to HealthDataScotland"),
    p(
      "HealthDataScotland is an R shiny application that presents
      GP practice demographic and hospital bed capacity data
      across Scotland. The data presented has been obtained from various
      sources (see 'About' page) and is made available for download.
      The application is broadly split into two parts: 'Map' and 'Summary'.
      "
    ),
    p(
      "The 'Map' page contains an interactive map visualising
      GP practices and hospitals across Scotland. This map
      can be used to select GP or hospital, which will
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
            complex health-related data sets. The aim of HealthDataScotland
            was to create a tool for visualising
            GP and hospital related statistics that may be of interest
            to a wider audience. A secondary aim was to provide a platform for
            my own personal development in creating, maintaining and
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
    title = "Application structure",
    card(
      p(
        "HealthDataScotland is structured as an R package, which is
        hosted on GitHub at",
        tags$a(href = "https://github.com/jsleight1/HealthDataScotland", target = "_blank", "HealthDataScotland"),
        ". This application is deployed using the Azure container app platform, which uses
          a docker container that is published on",
        tags$a(href = "https://hub.docker.com/r/jsleight1/healthdatascotland", target = "_black", "docker.io."),
      ),
      p(
        "This application contains public sector information licensed under the Open Government Licence v3.0.
        The data is derived from a variety of NHS open
        data API, wcich are maintained by Public Health Scotland. Data is sourced
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
        "Details of the full data workflow can be found using the HealthDataScotland",
        tags$a(href = "https://jsleight1.github.io/HealthDataScotland/articles/data_workflow.html", target = "_blank", "documentation.")
      ),
      h3("Disclaimer"),
      p(
        "The workflows outlined in the documentation provide a rough guide as to which public data sets are
        being used and how they are being manipulated. It is worth emphasising, however, that this application is primarily a hobby project. Therefore any
        decisions should not be made using the data without consulting the original
        published data sets.",
        style="color: red"
      )
    )
  )
}
