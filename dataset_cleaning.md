dataset_cleaning
================
2022-12-01

``` r
library(tidyverse)
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
    ## This is mgcv 1.8-40. For overview type 'help("mgcv-package")'.

``` r
library(patchwork)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(fastDummies)
```

    ## Warning: package 'fastDummies' was built under R version 4.2.2

``` r
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
  separate(inspection_summary_result,into = c("inspection_summary","inspection_result"),sep = "-") %>% 
  drop_na(zip_code, age_range, violation_rate_percent,public_health_hazard_violation_rate, critical_violation_rate) %>% 
  filter(maximum_capacity != 0) %>% 
  mutate(
    educational_worker_ratio = total_educational_workers/maximum_capacity,
    program_type = tolower(program_type),
    facility_type = tolower(facility_type)
  )
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 2326 rows [18, 83,
    ## 92, 107, 131, 178, 184, 196, 197, 202, 204, 236, 238, 259, 263, 264, 275, 293,
    ## 326, 328, ...].

6.  Create dummy variables into a new dataframe for regression

``` r
childcare_reg_df = childcare_inspection_df %>% 
  dummy_cols(select_columns = "borough", remove_first_dummy = TRUE) %>%
  dummy_cols(select_columns = "status", remove_first_dummy = TRUE) %>% 
  dummy_cols(select_columns = "program_type", remove_first_dummy = TRUE) %>% 
  dummy_cols(select_columns = "facility_type", remove_first_dummy = TRUE) %>%
  dummy_cols(select_columns = "child_care_type", remove_first_dummy = TRUE) %>% 
  janitor::clean_names() %>% 
  select(-borough, -status, -program_type, -facility_type, -child_care_type)
```
