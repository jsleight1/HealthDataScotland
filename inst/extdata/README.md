scotland_gps.json contains spatial data for GP practices in Scotland as of 2021.
It was created as follows:

- .shp files obtained from spatial hub scotland https://data.spatialhub.scot/dataset/gp_practices-is/resource/8389fd1d-563d-4c05-9833-26d9f07fd6cd
- ogr2ogr -progress -t_srs WGS84 -simplify 300 scotland_gps.shp pub_gpprac.shp
- topojson -o scotland_gps.json scotland_gps.shp -p


scotland_hosp.json contains spatial data for Hospitals in Scotland as of 2021.
It was created as follows:

- .shp files obtained from spatial hub scotland https://data.spatialhub.scot/dataset/nhs_hospitals-is/resource/b810d206-45bd-4dff-bac7-110a50b4bd3b
- ogr2ogr -progress -t_srs WGS84 -simplify 300 scotland_hosp.shp pub_hosp.shp
- topojson -o scotland_hosp.json scotland_hosp.shp -p


scotland_boards.json contains spatial data for NHS health boards in Scotland as of 2019.
It was created as follows:

- .shp files obtained from data.gov.uk https://data.gov.uk/dataset/27d0fe5f-79bb-4116-aec9-a8e565ff756a/nhs-health-boards
- ogr2ogr -progress -t_srs WGS84 -simplify 300 scotland_boards.shp SG_NHS_HealthBoards_2019.shp
- topojson -o scotland_boards.json scotland_boards.shp -p