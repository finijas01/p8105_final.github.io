dataset_cleaning
================
2022-12-01

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(purrr)
library(ggplot2)
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.

``` r
library(patchwork)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(fastDummies)
set.seed(1)
```

Import original dataset

``` r
childcare_inspection_df = read_csv("./data/DOHMH_Childcare_Center_Inspections.csv") %>% 
janitor::clean_names()
```

    ## Rows: 26280 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Center Name, Legal Name, Building, Street, Borough, Phone, Permit ...
    ## dbl (11): ZipCode, Permit Number, Building Identification Number, Violation ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Basic data cleaning

1.  We select 22 key variables in this dataset to finish our analysis

2.  We separate the “inspection_summary_result” variable into two new
    variables: “inspection_summary” and

“inspection_result” to better show the summary and the result of the
inspection for each center

3.  Drop NA

4.  Create a new variable “educational_worker_ratio”

5.  Make all data in “program_type” and “facility_type” columns show in
    the same format : lower case

``` r
childcare_inspection_df = childcare_inspection_df %>% 
  select(center_name, borough, zip_code, status, age_range, maximum_capacity,program_type, facility_type, 
         child_care_type, violation_category,
         violation_status,violation_rate_percent:average_critical_violation_rate,regulation_summary,
         inspection_summary_result) %>%
  drop_na(zip_code, age_range, violation_rate_percent,public_health_hazard_violation_rate, critical_violation_rate) %>% 
  filter(maximum_capacity != 0) %>% 
  mutate(
    educational_worker_ratio = total_educational_workers/maximum_capacity,
    program_type = tolower(program_type),
    facility_type = tolower(facility_type),
    borough =  as.factor(borough),
    status = as.factor(status),
    program_type = as.factor(program_type),
    facility_type = as.factor(facility_type),
    child_care_type = as.factor(child_care_type)
  )
```

We calculated a new violation rate for each distinct program using
violation category column.

``` r
childcare_inspection_df %>% 
  relocate(center_name, program_type) %>% 
  group_by(center_name, program_type) %>% 
  mutate(
    n_na = sum(is.na(violation_category)), 
    n_violation = sum(!is.na(violation_category)), 
    rate = n_violation/(n_violation+n_na)) %>% 
  arrange(center_name, program_type)
```

    ## # A tibble: 19,472 × 25
    ## # Groups:   center_name, program_type [1,917]
    ##    center_name    progr…¹ borough zip_c…² status age_r…³ maxim…⁴ facil…⁵ child…⁶
    ##    <chr>          <fct>   <fct>     <dbl> <fct>  <chr>     <dbl> <fct>   <fct>  
    ##  1 'THE STUDIO S… presch… MANHAT…   10025 Permi… 2 YEAR…      51 gdc     Child …
    ##  2 'THE STUDIO S… presch… MANHAT…   10025 Permi… 2 YEAR…      51 gdc     Child …
    ##  3 'THE STUDIO S… presch… MANHAT…   10025 Permi… 2 YEAR…      51 gdc     Child …
    ##  4 'THE STUDIO S… presch… MANHAT…   10025 Permi… 2 YEAR…      51 gdc     Child …
    ##  5 'THE STUDIO S… presch… MANHAT…   10025 Permi… 2 YEAR…      51 gdc     Child …
    ##  6 'THE STUDIO S… presch… MANHAT…   10025 Permi… 2 YEAR…      51 gdc     Child …
    ##  7 'THE STUDIO S… presch… MANHAT…   10025 Permi… 2 YEAR…      51 gdc     Child …
    ##  8 1332 FULTON  … presch… BRONX     10456 Expir… 2 YEAR…     148 gdc     Child …
    ##  9 1332 FULTON  … presch… BRONX     10456 Expir… 2 YEAR…     148 gdc     Child …
    ## 10 1332 FULTON  … presch… BRONX     10456 Expir… 2 YEAR…     148 gdc     Child …
    ## # … with 19,462 more rows, 16 more variables: violation_category <chr>,
    ## #   violation_status <chr>, violation_rate_percent <dbl>,
    ## #   average_violation_rate_percent <dbl>, total_educational_workers <dbl>,
    ## #   average_total_educational_workers <dbl>,
    ## #   public_health_hazard_violation_rate <dbl>,
    ## #   average_public_health_hazard_violation_rate <dbl>,
    ## #   critical_violation_rate <dbl>, average_critical_violation_rate <dbl>, …
