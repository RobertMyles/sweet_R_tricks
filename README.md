
Sweet R tricks
==============

Tips and tricks in R & RStudio. Gathered from wherever I see them. Mainly a repo for me to remember cool little R tips I've seen around the place. I hope to update it regularly -- feel free to fork, add your own, and send a PR.

Tables
------

### Totals columns

[source](https://twitter.com/andrewheiss/status/973325552596664321?s=03)

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(stringr)
library(pander)
mtcars %>% 
  mutate(cars = row.names(.),
         cars = str_extract(cars, "[A-Za-z\\b]*")) %>% 
  count(cars, am) %>% 
  bind_rows(summarise_at(., vars(n), funs(sum)) %>%
              mutate(cars = "**Total**")
  ) %>% 
  pandoc.table()
#> 
#> ---------------------
#>    cars      am   n  
#> ----------- ---- ----
#>     AMC      0    1  
#> 
#>  Cadillac    0    1  
#> 
#>   Camaro     0    1  
#> 
#>  Chrysler    0    1  
#> 
#>   Datsun     1    1  
#> 
#>    Dodge     0    1  
#> 
#>   Duster     0    1  
#> 
#>   Ferrari    1    1  
#> 
#>    Fiat      1    2  
#> 
#>    Ford      1    1  
#> 
#>    Honda     1    1  
#> 
#>   Hornet     0    2  
#> 
#>   Lincoln    0    1  
#> 
#>    Lotus     1    1  
#> 
#>  Maserati    1    1  
#> 
#>    Mazda     1    2  
#> 
#>    Merc      0    7  
#> 
#>   Pontiac    0    1  
#> 
#>   Porsche    1    1  
#> 
#>   Toyota     0    1  
#> 
#>   Toyota     1    1  
#> 
#>   Valiant    0    1  
#> 
#>    Volvo     1    1  
#> 
#>  **Total**   NA   32 
#> ---------------------
```

Also (I prefer this actually):

``` r
library(dplyr)
library(janitor)
mtcars %>% 
  mutate(cars = row.names(.),
         cars = str_extract(cars, "[A-Za-z\\b]*")) %>% 
  count(cars, am) %>% 
  adorn_totals()
#> # A tibble: 24 x 4
#>    cars        am     n Total
#>    <chr>    <dbl> <int> <dbl>
#>  1 AMC         0.     1    1.
#>  2 Cadillac    0.     1    1.
#>  3 Camaro      0.     1    1.
#>  4 Chrysler    0.     1    1.
#>  5 Datsun      1.     1    2.
#>  6 Dodge       0.     1    1.
#>  7 Duster      0.     1    1.
#>  8 Ferrari     1.     1    2.
#>  9 Fiat        1.     2    3.
#> 10 Ford        1.     1    2.
#> # ... with 14 more rows
```
