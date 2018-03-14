
Sweet R tricks
==============

Tips and tricks in R & RStudio. Gathered from wherever I see them. Mainly a repo for me to remember cool little R tips I've seen around the place. I hope to update it regularly -- feel free to fork, add your own, and send a PR.

I/O
===

### Making Saved Data Smaller

Adding `compress = "xz"` to your `save()` function can make things much smaller:

``` r
library(congressbr)
data('senate_nominal_votes')
head(senate_nominal_votes)
#> # A tibble: 6 x 9
#>   vote_date           bill_id bill    legislature senator_id senator_name 
#>   <dttm>              <chr>   <chr>   <chr>       <chr>      <chr>        
#> 1 1991-06-06 00:00:00 19615   PLC:19~ 49          31         Guilherme Pa~
#> 2 1991-06-06 00:00:00 19615   PLC:19~ 49          47         Jose Sarney  
#> 3 1991-06-06 00:00:00 19615   PLC:19~ 49          82         Amazonino Me~
#> 4 1991-06-06 00:00:00 19615   PLC:19~ 49          33         Humberto Luc~
#> 5 1991-06-06 00:00:00 19615   PLC:19~ 49          79         Valmir Campe~
#> 6 1991-06-06 00:00:00 19615   PLC:19~ 49          84         Antonio Mariz
#> # ... with 3 more variables: senator_vote <chr>, senator_party <chr>,
#> #   senator_state <chr>
save(list = ls(), file = "sen.Rda")
save(list = ls(), file = "sen2.Rda", compress = "xz")

file.info("sen.Rda")$size
#> [1] 73485
file.info("sen2.Rda")$size
#> [1] 38408
```

[Source](https://twitter.com/ikashnitsky/status/973325892956184576)

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

Also, from the comments to the above tweet (I prefer this actually):

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
