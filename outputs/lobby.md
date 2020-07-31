Pennsylvania Lobbying Disclosure Data 2017
================
Jin Chen
7/31/2020

# Purpose

To answer the question “Which subject has the most lobbying expenditures
in PA in 2017?” by analyzing a 2017 Pennsylvania Lobbying Expenditure
report.

# Content

I will use the RSocrata package to import the data.

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ----------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(gapminder)
library(RSocrata)
library(ggthemes)

#set the public URL for the data set 
url <- "https://data.pa.gov/Government-Efficiency-Citizen-Engagement/Lobbying-Disclosure-Data-2017-State/wbm6-mh8t"

#call the API using RSocrata
lobby_raw <- read.socrata(url)

#show the first few obs
#head(lobby_raw)
```

# Structure

I group by subject then calculate the sum of the total costs for each
subject. I only inlude the top 10 expenditures.

``` r
lobby <- lobby_raw %>% 
  select( name = registration_name,
          type = registration_type,
          subject, 
          date = date_of_expense,
          quarter = quarter_of_expense,
          total_direct_costs:total_costs) %>% 
  filter( total_costs != 0,
          subject != "")

lobby_top_25 <- lobby %>% 
  group_by(subject) %>% 
  summarize(cost = sum(total_costs)) %>% 
  top_n(10) %>% 
  arrange(desc(cost))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Selecting by cost

# Formatting

I use a lollipop chart to display each subject and their respective
expenditures.

``` r
p <- ggplot(data = lobby_top_25,
            mapping = aes(x = reorder(subject, cost),
                          y = cost))

p + 
  geom_point(color = "navyblue", size = 3, alpha = 0.8) +
  geom_segment(aes( xend = subject, y = 0, yend = cost)) +
  scale_y_continuous(labels = scales::label_number_si(accuracy = 1, prefix = "$")) +  #displays cost in millions
  coord_flip(clip = "off") +
  theme_light() +
  labs(title = "2017 Pennsylavnia Lobbying Expenses by Subject",
       caption = "Data from opendataPA",
       x = "",
       y = "") +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank())
```

![](lobby_files/figure-gfm/Graph%20It-1.png)<!-- -->

# Conclusion

Overwhelmingly, Health Care was the subject that lobbyists spent the
most on in 2017, reaching almost $15M. This is followed by State Budget,
and Energy.
