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
child_data <- child_data %>%
  mutate(violation = if_else(.$regulation_summary == 'There were no new violations observed at the time of this inspection/visit.','0','1'),
         violation = as.factor(violation))
```

\#fit a logistic regression with violation vs no violation as the
outcome and the rest as predictors.And Obtain the estimate and CI of the
adjusted odds ratio for having violation.

``` r
model_log <- glm(violation ~borough + status + maximum_capacity + program_type + facility_type + child_care_type + total_educational_workers + average_total_educational_workers+average_public_health_hazard_violation_rate+educational_worker_ratio,data = child_data,family = binomial(link=logit))
model_log %>%
  broom::tidy(conf.int = T) %>% 
  mutate(OR = exp(estimate),
         CI_lower = exp(exp(conf.low)),
         CI_upper = exp(exp(conf.high)),
         p_val = rstatix::p_format(p.value, digits = 2)) %>% 
  select(term, OR, CI_lower,CI_upper, p_val) %>% 
  knitr::kable(digits = 3, align = "lccc", 
               col.names = c("Term", "Estimated adjusted OR", "CI lower bound", "CI upper bound", "p-value"))
```

| Term                                         | Estimated adjusted OR | CI lower bound | CI upper bound | p-value  |
|:---------------------------------------------|:---------------------:|:--------------:|:--------------:|:---------|
| (Intercept)                                  |         4.594         |     31.953     |  4.486230e+02  | \<0.0001 |
| boroughBROOKLYN                              |         0.352         |     1.376      |  1.472000e+00  | \<0.0001 |
| boroughMANHATTAN                             |         0.456         |     1.509      |  1.656000e+00  | \<0.0001 |
| boroughQUEENS                                |         0.648         |     1.798      |  2.044000e+00  | \<0.0001 |
| boroughSTATEN ISLAND                         |         0.248         |     1.231      |  1.343000e+00  | \<0.0001 |
| statusExpired-In Renewal                     |         0.446         |     1.435      |  1.731000e+00  | \<0.0001 |
| statusPermitted                              |         0.354         |     1.336      |  1.539000e+00  | \<0.0001 |
| maximum_capacity                             |         1.000         |     2.717      |  2.720000e+00  | 0.608    |
| program_typeinfant toddler                   |         0.929         |     2.133      |  3.129000e+00  | 0.484    |
| program_typepreschool                        |         1.007         |     2.325      |  3.325000e+00  | 0.941    |
| program_typeschool age camp                  |         1.683         |     1.168      |  1.173805e+16  | 0.675    |
| facility_typegdc                             |          NA           |       NA       |       NA       | NA       |
| facility_typesbcc                            |          NA           |       NA       |       NA       | NA       |
| child_care_typeChild Care - Infants/Toddlers |          NA           |       NA       |       NA       | NA       |
| child_care_typeChild Care - Pre School       |          NA           |       NA       |       NA       | NA       |
| child_care_typeSchool Based Child Care       |          NA           |       NA       |       NA       | NA       |
| total_educational_workers                    |         1.002         |     2.712      |  2.733000e+00  | 0.45     |
| average_total_educational_workers            |          NA           |       NA       |       NA       | NA       |
| average_public_health_hazard_violation_rate  |          NA           |       NA       |       NA       | NA       |
| educational_worker_ratio                     |         0.682         |     1.661      |  2.495000e+00  | 0.011    |
