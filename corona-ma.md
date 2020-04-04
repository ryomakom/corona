corona-ma
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
uscounty <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   county = col_character(),
    ##   state = col_character(),
    ##   fips = col_character(),
    ##   cases = col_double(),
    ##   deaths = col_double()
    ## )

``` r
ma_main_counties <- uscounty %>%
  filter(state == "Massachusetts") %>%
  filter(county == "Norfolk" |
         county == "Suffolk" | 
         county == "Essex" |
         county == "Middlesex" |
         county == "Bristol" |
         county == "Plymouth")


ma_main_counties %>%  filter(date >= "2020-03-01") %>%
  ggplot(aes(date, cases, group = county, color = county)) +
  geom_line() + scale_y_log10() +
  ggtitle("Cases in Coastal Counties of MA") + labs(y = "Confirmed Cases")
```

![](corona-ma_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ma_main_counties %>% filter(date >= "2020-03-15") %>% group_by(county) %>%
    mutate(increase = cases-lag(cases), increase_rate = increase/lag(cases),
           increase_rate = rollmean(increase_rate, 7, fill = NA)) %>%
    ggplot(aes(date, increase_rate, group = county, color = county)) +
    geom_line() + ggtitle("Moving Avarage of the Increase Rate in Cases (Coastal Counties of MA)")
```

    ## Warning: Removed 42 rows containing missing values (geom_path).

![](corona-ma_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->
