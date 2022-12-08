logistic regression
================
Han Bao
2022-12-08

``` r
library(tidyverse)
library(broom)
library(viridis)
library(purrr)
library(modelr)
theme_set(theme_classic())
```

``` r
child_data = read_csv("./data/DOHMH_Childcare_Center_Inspections.csv") %>%
  janitor::clean_names()%>%
  distinct()
```

``` r
child_data = child_data %>% 
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

``` r
head(child_data, 10)
```

    ## # A tibble: 10 × 22
    ##    center_name    borough zip_c…¹ status age_r…² maxim…³ progr…⁴ facil…⁵ child…⁶
    ##    <chr>          <fct>     <dbl> <fct>  <chr>     <dbl> <fct>   <fct>   <fct>  
    ##  1 TRADITIONAL E… BROOKL…   11221 Expir… 0 YEAR…      60 infant… gdc     Child …
    ##  2 BOYS & GIRL S… BROOKL…   11212 Expir… 0 YEAR…      60 all ag… camp    Camp   
    ##  3 ITTY BITTY AD… BROOKL…   11234 Permi… 2 YEAR…      42 presch… gdc     Child …
    ##  4 MAGIC MOMENTS… BROOKL…   11238 Permi… 2 YEAR…      24 presch… gdc     Child …
    ##  5 YELED VYALDA … BROOKL…   11213 Expir… 2 YEAR…      75 presch… gdc     Child …
    ##  6 EAST SIDE HOU… BRONX     10454 Expir… 2 YEAR…      80 presch… gdc     Child …
    ##  7 NAT AZAROW CH… BROOKL…   11212 Permi… 2 YEAR…      98 presch… gdc     Child …
    ##  8 BRIGHT START … QUEENS    11101 Permi… 2 YEAR…      72 presch… gdc     Child …
    ##  9 UNIVERSITY SE… BROOKL…   11201 Expir… 0 YEAR…     550 all ag… camp    Camp   
    ## 10 THE FRIENDS O… BROOKL…   11213 Permi… 2 YEAR…     100 presch… gdc     Child …
    ## # … with 13 more variables: violation_category <chr>, violation_status <chr>,
    ## #   violation_rate_percent <dbl>, average_violation_rate_percent <dbl>,
    ## #   total_educational_workers <dbl>, average_total_educational_workers <dbl>,
    ## #   public_health_hazard_violation_rate <dbl>,
    ## #   average_public_health_hazard_violation_rate <dbl>,
    ## #   critical_violation_rate <dbl>, average_critical_violation_rate <dbl>,
    ## #   regulation_summary <chr>, inspection_summary_result <chr>, …

``` r
#create binary variable according to regulation_summary
child_data %>%
  mutate(violation = if_else(.$regulation_summary == 'There were no new violations observed at the time of this inspection/visit.','0','1'))
```

    ## # A tibble: 16,454 × 23
    ##    center_name    borough zip_c…¹ status age_r…² maxim…³ progr…⁴ facil…⁵ child…⁶
    ##    <chr>          <fct>     <dbl> <fct>  <chr>     <dbl> <fct>   <fct>   <fct>  
    ##  1 TRADITIONAL E… BROOKL…   11221 Expir… 0 YEAR…      60 infant… gdc     Child …
    ##  2 BOYS & GIRL S… BROOKL…   11212 Expir… 0 YEAR…      60 all ag… camp    Camp   
    ##  3 ITTY BITTY AD… BROOKL…   11234 Permi… 2 YEAR…      42 presch… gdc     Child …
    ##  4 MAGIC MOMENTS… BROOKL…   11238 Permi… 2 YEAR…      24 presch… gdc     Child …
    ##  5 YELED VYALDA … BROOKL…   11213 Expir… 2 YEAR…      75 presch… gdc     Child …
    ##  6 EAST SIDE HOU… BRONX     10454 Expir… 2 YEAR…      80 presch… gdc     Child …
    ##  7 NAT AZAROW CH… BROOKL…   11212 Permi… 2 YEAR…      98 presch… gdc     Child …
    ##  8 BRIGHT START … QUEENS    11101 Permi… 2 YEAR…      72 presch… gdc     Child …
    ##  9 UNIVERSITY SE… BROOKL…   11201 Expir… 0 YEAR…     550 all ag… camp    Camp   
    ## 10 THE FRIENDS O… BROOKL…   11213 Permi… 2 YEAR…     100 presch… gdc     Child …
    ## # … with 16,444 more rows, 14 more variables: violation_category <chr>,
    ## #   violation_status <chr>, violation_rate_percent <dbl>,
    ## #   average_violation_rate_percent <dbl>, total_educational_workers <dbl>,
    ## #   average_total_educational_workers <dbl>,
    ## #   public_health_hazard_violation_rate <dbl>,
    ## #   average_public_health_hazard_violation_rate <dbl>,
    ## #   critical_violation_rate <dbl>, average_critical_violation_rate <dbl>, …

``` r
model_log <- glm(facility_type ~borough + status + maximum_capacity + program_type + facility_type + child_care_type + total_educational_workers + average_total_educational_workers+average_public_health_hazard_violation_rate+educational_worker_ratio,data = child_data,family = binomial(link=logit))
```
