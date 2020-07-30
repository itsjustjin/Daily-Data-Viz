Who let the Dogs out?
================
Jin Chen
7/30/2020

# Purpose

To answer the question “Which county has the most dangerous dog
incidents in PA?” by analyzing a dataset from the Pennsylvania
government on dangerous dog incidents from 1996 to 2019.

# Content

I will use the RSocrata package to import the data.

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------ tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts --------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(RSocrata)
library(ggthemes)

#set the public URL for the data set 
url <- "https://data.pa.gov/Public-Safety/Dangerous-Dogs-1996-Current-County-Agriculture/3fcn-e5dk"

#call the API using RSocrata
dan_dogs_raw <- read.socrata(url)

#show the first few obs
head(dan_dogs_raw)
```

    ##    file_date determination_year status_description county_description
    ## 1 2003-06-16               2003           Deceased               YORK
    ## 2 2003-06-16               2003           Deceased               YORK
    ## 3 2003-05-22               2003           Deceased          ARMSTRONG
    ## 4 2003-08-18               2003           Deceased            CAMBRIA
    ## 5 2003-01-01               2003             Closed              BUCKS
    ## 6 2003-01-01               2003             Closed            CHESTER
    ##   dog_info_age dog_info_breed dog_info_predator county_key owner_county
    ## 1       3 YRS.        UNKNOWN               Dog         67         YORK
    ## 2       6 YRS.        UNKNOWN               Dog         67         YORK
    ## 3       4 YRS.        UNKNOWN               Dog          3    ARMSTRONG
    ## 4      UNKNOWN        UNKNOWN               Dog         11      CAMBRIA
    ## 5      UNKNOWN        UNKNOWN               Dog          9        BUCKS
    ## 6        ADULT        UNKNOWN               Dog         15      CHESTER
    ##   court_disposition hearing_date owner_first_name owner_last_name
    ## 1            Guilty   2003-06-16       GENE/DONNA  HOSTLER/KINERT
    ## 2            Guilty   2003-06-16       GENE/DONNA  HOSTLER/KINERT
    ## 3            Guilty   2003-05-22           RONALD       DZUROUCIN
    ## 4            Guilty   2003-08-18            TERRY       MELL, SR.
    ## 5            Guilty   2003-08-20           ALISON         CUSATIS
    ## 6             Other         <NA>              LUC        D'AURIOL
    ##                  owner_address owner_city  owner_state owner_zip
    ## 1 1300 YORK HAVEN ROAD, LOT 79 YORK HAVEN Pennsylvania     17370
    ## 2 1300 YORK HAVEN ROAD, LOT 79 YORK HAVEN Pennsylvania     17370
    ## 3             278 PHILIPS LANE  LEECHBURG Pennsylvania     15656
    ## 4          1147 DEVEAUX STREET     ELMORA Pennsylvania     15737
    ## 5              1293 LISA DRIVE WARRINGTON Pennsylvania     18976
    ## 6         2545 WHITEHORSE ROAD     BERWYN Pennsylvania     19312
    ##   pa_dept_of_ag_dog_law_region   account_last_first_names
    ## 1                            6 HOSTLER/KINERT, GENE/DONNA
    ## 2                            6 HOSTLER/KINERT, GENE/DONNA
    ## 3                            1          DZUROUCIN, RONALD
    ## 4                            4           MELL, SR., TERRY
    ## 5                            7            CUSATIS, ALISON
    ## 6                            7              D'AURIOL, LUC
    ##     account_first_last_name business_name
    ## 1 GENE/DONNA HOSTLER/KINERT          <NA>
    ## 2 GENE/DONNA HOSTLER/KINERT          <NA>
    ## 3          RONALD DZUROUCIN          <NA>
    ## 4           TERRY MELL, SR.          <NA>
    ## 5            ALISON CUSATIS          <NA>
    ## 6              LUC D'AURIOL          <NA>
    ##                                        full_address address_line_2 middle_name
    ## 1 1300 YORK HAVEN ROAD, LOT 79\nYORK HAVEN PA 17370                           
    ## 2 1300 YORK HAVEN ROAD, LOT 79\nYORK HAVEN PA 17370                           
    ## 3              278 PHILIPS LANE\nLEECHBURG PA 15656                           
    ## 4              1147 DEVEAUX STREET\nELMORA PA 15737                           
    ## 5              1293 LISA DRIVE\nWARRINGTON PA 18976                           
    ## 6             2545 WHITEHORSE ROAD\nBERWYN PA 19312                           
    ##                geocoded_column
    ## 1 POINT (-76.751687 40.124895)
    ## 2 POINT (-76.751687 40.124895)
    ## 3 POINT (-79.652105 40.609562)
    ## 4 POINT (-78.750988 40.605271)
    ## 5 POINT (-75.128722 40.246242)
    ## 6 POINT (-75.450961 40.002585)

# Structure

I create a separate df from the raw data. I rename a column to make it
more accessible. Then I find the count of incidents that happened in
each county.

``` r
#by county
dan_dogs_county <- dan_dogs_raw %>% 
  rename(county = county_description) %>% 
  mutate( year = lubridate::year(file_date)) %>% #create a year column
  filter(year != 2020) %>% #filter out entries with year of 2020
  group_by(county) %>% 
  summarize(count = n()) %>% #count the number of incidents in each county
  select(county, count) %>% 
  mutate(county = fct_reorder(county, count)) %>%  #order county by count for graphing purposes
  arrange(desc(count)) %>% 
  top_n(10)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Selecting by count

``` r
#Create a column to distinguish the #1 county of incidents with the rest. (could use gghighlight as an alternative)
dan_dogs_county <- dan_dogs_county %>% 
  mutate(county_highlight =
           ifelse(county == "ALLEGHENY",
                  "Allegheny",
                  "Other Counties"))
```

# Formatting

``` r
#by county
p <- ggplot(data = dan_dogs_county,
            mapping = aes(x = county,
                          y = count))
p + geom_col(aes(fill = county_highlight)) +
    coord_flip() +
    theme_tufte() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("dark blue", "light grey")) +
    labs(title = "Who Let the Dogs Out?",
       caption = "Data from 1996 - 2019",
       y = "Dangerous Dog Incidents",
       x = "") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(label = count), vjust = 0, hjust = -0.5, size = 3)
```

![](dan_dogs_files/figure-gfm/Dangerous%20Dog%20Incidents%20Graph-1.png)<!-- -->

# Conclusion

Thus conclude today’s daily data visualization\! We have found that the
PA county with the most dangerous dog incidents is Allegheny followed by
York and Westmoreland with Bucks closely behind.
