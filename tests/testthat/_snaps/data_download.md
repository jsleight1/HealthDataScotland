# process_gp_meta works

    # A tibble: 6 x 16
      ID    GPPracticeName   PracticeListSize AddressLine1 AddressLine2 AddressLine3
      <chr> <chr>                       <dbl> <chr>        <chr>        <chr>       
    1 10002 Muirhead Medica~             8251 Muirhead Me~ Liff Road    Muirhead    
    2 10017 The Blue Practi~             7272 The Blue Pr~ Crieff Medi~ King Street 
    3 10036 Aberfeldy And K~             4843 Aberfeldy A~ Taybridge R~ Aberfeldy   
    4 10106 Grove Health Ce~             6429 Grove Healt~ 129 Dundee ~ Broughty Fe~
    5 10125 Alyth Health Ce~             4590 Alyth Healt~ New Alyth R~ Alyth       
    6 10182 Arbroath Medica~             9901 Arbroath Me~ 7 Hill Place Arbroath    
    # i 10 more variables: AddressLine4 <chr>, Postcode <chr>,
    #   TelephoneNumber <chr>, PracticeType <chr>, Dispensing <lgl>, HB <chr>,
    #   HSCP <chr>, DataZone <chr>, GPCluster <chr>, HBName <chr>

# process_gp_data works

    # A tibble: 6 x 14
      Date       ID    HB        HSCP    Sex   AllAges Ages0to4 Ages5to14 Ages15to24
      <date>     <chr> <chr>     <chr>   <chr>   <dbl>    <dbl>     <dbl>      <dbl>
    1 2024-04-01 10002 S08000030 S37000~ Male     4056      186       456        415
    2 2024-04-01 10002 S08000030 S37000~ Fema~    4195      176       397        425
    3 2024-04-01 10002 S08000030 S37000~ All      8251      362       853        840
    4 2024-04-01 10017 S08000030 S37000~ Male     3652      134       393        512
    5 2024-04-01 10017 S08000030 S37000~ Fema~    3620      115       351        457
    6 2024-04-01 10017 S08000030 S37000~ All      7272      249       744        969
    # i 5 more variables: Ages25to44 <dbl>, Ages45to64 <dbl>, Ages65to74 <dbl>,
    #   Ages75to84 <dbl>, Ages85plus <dbl>

# process_hopsital_meta works

    # A tibble: 6 x 16
      ID    HospitalName       AddressLine1 AddressLine2 AddressLine2QF AddressLine3
      <chr> <chr>              <chr>        <chr>        <chr>          <chr>       
    1 A101H Arran War Memoria~ Lamlash      Isle of Arr~ <NA>           <NA>        
    2 A103H Ayrshire Central ~ Kilwinning ~ Irvine       <NA>           <NA>        
    3 A110H Lady Margaret Hos~ College St   Millport     <NA>           Isle of Cum~
    4 A111H University Hospit~ Kilmarnock ~ Kilmarnock   <NA>           Ayrshire    
    5 A114H Warrix Avenue Men~ Warrix Aven~ Irvine       <NA>           <NA>        
    6 A201H Ailsa Hospital     Dalmellingt~ Ayr          <NA>           <NA>        
    # i 10 more variables: AddressLine3QF <chr>, AddressLine4 <chr>,
    #   AddressLine4QF <chr>, Postcode <chr>, HealthBoard <chr>, HSCP <chr>,
    #   CouncilArea <chr>, IntermediateZone <chr>, DataZone <chr>, HBName <chr>

# process_hopsital_data works

    # A tibble: 6 x 16
      ID    HospitalName       AddressLine1 AddressLine2 AddressLine2QF AddressLine3
      <chr> <chr>              <chr>        <chr>        <chr>          <chr>       
    1 A101H Arran War Memoria~ Lamlash      Isle of Arr~ <NA>           <NA>        
    2 A103H Ayrshire Central ~ Kilwinning ~ Irvine       <NA>           <NA>        
    3 A110H Lady Margaret Hos~ College St   Millport     <NA>           Isle of Cum~
    4 A111H University Hospit~ Kilmarnock ~ Kilmarnock   <NA>           Ayrshire    
    5 A114H Warrix Avenue Men~ Warrix Aven~ Irvine       <NA>           <NA>        
    6 A201H Ailsa Hospital     Dalmellingt~ Ayr          <NA>           <NA>        
    # i 10 more variables: AddressLine3QF <chr>, AddressLine4 <chr>,
    #   AddressLine4QF <chr>, Postcode <chr>, HealthBoard <chr>, HSCP <chr>,
    #   CouncilArea <chr>, IntermediateZone <chr>, DataZone <chr>, HBName <chr>

