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

about_card <- function() {
  card(
    min_height = "500px",
    card_header("About the author"),
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
}

package_card <- function() {
  card(
    min_height = "200px",
    card_header("About the HealthDataScotland R package"),
    p(
      "HealthDataScotland is structured as an R package, which is
      hosted on GitHub at",
      tags$a(href = "https://github.com/jsleight1/HealthDataScotland", target = "_blank", "HealthDataScotland"),
      ". HealthDataScotland also has an associated docker container hosted on",
      tags$a(href = "https://hub.docker.com/r/jsleight1/healthdatascotland", target = "_black", "docker.io"),
      " For more information regarding the R package please refer to the ",
      tags$a(href = "https://jsleight1.github.io/HealthDataScotland/", target = "_blank", "documentation"),
      "."
    )
  )
}

data_workflow_card <- function() {
  card(
    min_height = "800px",
    card_header("About the data workflow"),
    p(
      "The data used in HealthDataScotland is derived from the NHS open
      data API and is maintained by Public Health Scotland. The following
      data sets are used:
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
      "To aide with data transparency and reproducibility, I have outlined how
      the above data sets are retrieved and manipulated into a usable format
      by HealthDataScotland. The processed data set is stored in an Azure blob.
      This blob is updated periodically using a GitHub action."
    ),
    HTML("<b>GP data processing</b>"),
    p(
      "- GP metadata: GP metadata was obtained from the NHS open data set titled
      'gp-practice-contact-details-and-list-sizes'. This data set contains
      metadata for GP practice including name, address and phone number."
    ),
    p(
      "- GP demography data: GP demographic data was obtained from the NHS open data set titled
      'gp-practice-populations'. This data set contains numbers of
      registered patients at each GP practice split by Gender and Age."
    ),
    p(
      "- Combined data: The intersection of GP practice IDs between the
      metadata and demographic data was determined. This master set of IDs was used to
      combine the two data sets and this combined data set is used by the
      application"
    ),
    p(
      "- Map: GP spatial data was derived from the Spatial Hub and manipulated
      into a format appropriate for visualisation. Please note the spatial
      data may be out of date and have missing information."
    ),
    p(
      "- Summary: Unlike the interactive map, the 'Summary' section of the
      application uses the full combined GP data set derived from NHS open data.
      Therefore, demographic data for GP practices with missing spatial data can
      be viewed in this section. This section summarises GP demographic data
      at the national and health board level by grouping all GP practices
      within a particular group (e.g. the health board 'Greater Glasgow and Clyde') and summing
      the total number of registered patients. Please note that there may be
      unknown caveats with this approach. Therefore please refer to the
      published data sets before making any conclusions."
    ),
    HTML("<b>Hospital data processing</b>"),
    p(
      "- Hospital metadata: Hospital metadata was obtained from the NHS open data set
      titled 'hospital-codes'. This data set contains metadata for hospitals
      including name, address and health board."
    ),
    p(
      "- Hospital bed occupancy data: Hospital bed occupancy data was obtained from the NHS open data set
      titled 'annual-hospital-beds-information'. This data set was filtered to the PHS
      data set ID 'd719af13-5fb3-430f-810e-ab3360961107' to obtain bed
      capacity per specialty per hospital location."
    ),
    p(
      "- Combined data: The intersection of hospital codes between the metadata
      and bed capactiy data was determined. This master set of IDs was used
      to combine the two data sets and this combined data set is used by
      the application."
    ),
    p(
      "- Map: Hospital spatial data was derived from the Spatial Hub and
      manipulated into a format appropriate for visualisation. Please note the
      spatial data may be out of date and have missing information."
    ),
    p(
      "- Summary: Unlike the interactive map, the 'Summary' section of the
      application uses the full combined hospital data set derived from NHS open data.
      Therefore bed capacity data for hospitals with missing spatial data can
      be viewed in this section. This section summarises hospital bed capacity
      data at the national and health board level per specialty by grouping all hospitals
      within a particular group (e.g. the health board 'Greater Glasgow and Clyde') and taking the
      average percentage bed occupancy for a selected specialty (whilst filering
      non-applicable data points). Please note that there may be unknown
      caveats with this approach. Therefore please refer to the published data sets
      before making any conclusions."
    ),
    p(
      style = "color: var(--brand-red);",
      "The workflows outlined provide a rough guide as to which public
      data sets are being used and how they are being manipulated. It is worth
      emphasising once more, however, that this application is primarily a hobby
      project. Therefore any decisions should not be made using the data
      without consulting the original published data sets.
      "
    )
  )
}
