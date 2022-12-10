dataset_cleaning
================
2022-12-01

``` r
library(tidyverse)
<<<<<<< HEAD
<<<<<<< HEAD
```

    ## Warning: package 'tidyverse' was built under R version 4.2.2

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
=======
>>>>>>> f3437e85eb0b3a15d3e2b919e9941763745e572b
=======
>>>>>>> 37db35354b14f908a9cd1e396fab3259e4b6edca
library(dplyr)
library(rvest)
<<<<<<< HEAD
```

    ## 
    ## 載入套件：'rvest'
    ## 
    ## 下列物件被遮斷自 'package:readr':
    ## 
    ##     guess_encoding

``` r
=======
>>>>>>> b6b596b372cfb6c38a4e4363dfdefd877c8637da
library(purrr)
library(ggplot2)
library(modelr)
library(mgcv)
<<<<<<< HEAD
```

    ## 載入需要的套件：nlme
    ## 
    ## 載入套件：'nlme'
    ## 
    ## 下列物件被遮斷自 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.8-40. For overview type 'help("mgcv-package")'.

``` r
=======
>>>>>>> b6b596b372cfb6c38a4e4363dfdefd877c8637da
library(patchwork)
library(viridis)
library(fastDummies)
<<<<<<< HEAD
```

<<<<<<< HEAD
    ## 載入需要的套件：viridisLite

``` r
library(fastDummies)
```

    ## Warning: 套件 'fastDummies' 是用 R 版本 4.2.2 來建造的

``` r
=======
    ## Warning: package 'fastDummies' was built under R version 4.2.2

``` r
=======
>>>>>>> 37db35354b14f908a9cd1e396fab3259e4b6edca
>>>>>>> b6b596b372cfb6c38a4e4363dfdefd877c8637da
set.seed(1)
```

1.  Import original dataset

2.  Remove repeated data

``` r
childcare_inspection_df = read_csv("./data/DOHMH_Childcare_Center_Inspections.csv") %>% 
janitor::clean_names() %>% 
distinct()
```

    ## Rows: 26280 Columns: 34
    ## ── Column specification ─────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Center Name, Legal Name, Building, Street, Borough, Phone, Permit ...
    ## dbl (11): ZipCode, Permit Number, Building Identification Number, Violation ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Basic data cleaning

1.  We select 22 key variables in this dataset to finish our analysis

2.  Drop NA

3.  Create a new variable “educational_worker_ratio”

4.  Make all data in “program_type” and “facility_type” columns show in
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
    child_care_type = as.factor(child_care_type),
    age_range = as.factor(age_range)
  ) 
```

We calculated a new violation rate for each distinct program using
violation category column.

``` r
center_specific_df = childcare_inspection_df %>% 
  relocate(center_name, program_type) %>% 
  group_by(center_name, program_type) %>% 
  mutate(
    n_na = sum(is.na(violation_category)), 
    n_violation = sum(!is.na(violation_category)), 
    rate = n_violation/(n_violation + n_na)) %>% 
  arrange(center_name, program_type)
```
