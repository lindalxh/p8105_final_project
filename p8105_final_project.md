p8105_final_project
================
Xinwei Han (xh2601), Xinhui Lin (xl3054), Ruohan Lyu (rl3610), Xinyin
Miao (xm2356), Fenglin Xie (fx2212)
2025-12-06

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(hms)
```

    ## 
    ## Attaching package: 'hms'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     hms

``` r
park_ranger = read_csv("~/Desktop/Data science/p8105_final_project/data/Urban_Park_Ranger_Animal_Condition_Response_20251103.csv") %>% 
  janitor::clean_names() %>% 
  filter(date_and_time_of_ranger_response > date_and_time_of_initial_call) %>%  ## remove rows of response > initial call
  mutate(
    date_and_time_of_initial_call = mdy_hm(date_and_time_of_initial_call),
    date_and_time_of_ranger_response = mdy_hm(date_and_time_of_ranger_response),
    
    call_date = as.Date(date_and_time_of_initial_call),
    call_time = as_hms(date_and_time_of_initial_call),
    response_date = as.Date(date_and_time_of_ranger_response),
    response_time = as_hms(date_and_time_of_ranger_response)  ## split date and time
  ) %>% 
  select(-date_and_time_of_initial_call, -date_and_time_of_ranger_response) %>% 
  relocate(call_date, call_time, response_date, response_time)
```

    ## Rows: 6385 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (15): Date and Time of initial call, Date and time of Ranger response, B...
    ## dbl  (3): Duration of Response, # of Animals, Hours spent monitoring
    ## lgl  (4): PEP Response, Animal Monitored, Police Response, ESU Response
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
## reclassify animal species into 7 categories
species_mapping = c(
  "Animal (Unknown)" = "others",
  "Atlantic Canary" = "Birds + raptor",
  "Bird (Unknown)" = "Birds + raptor",
  "Budgerigar" = "Birds + raptor",
  "Budgerigar Parakeet" = "Birds + raptor",
  "Cat" = "Large mammal",
  "Cattle" = "Large mammal",
  "Chicken" = "Birds + raptor",
  "Chukar" = "Birds + raptor",
  "Cockatiel" = "Birds + raptor",
  "Dog" = "Large mammal",
  "Domestic Dove" = "Birds + raptor",
  "Domestic Duck" = "Birds + raptor",
  "Domestic Ferret" = "Small mammal",
  "Domestic Goose" = "Birds + raptor",
  "Domestic Goose Hybrid" = "Birds + raptor",
  "Domestic Quail" = "Birds + raptor",
  "Domestic Rabbit" = "Small mammal",
  "Domestic Turkey" = "Birds + raptor",
  "Domestic Waterfowl" = "Birds + raptor",
  "Fancy Dove" = "Birds + raptor",
  "Fancy Rat" = "Small mammal",
  "Gerbil" = "Small mammal",
  "Goat" = "Large mammal",
  "Guinea Fowl" = "Birds + raptor",
  "Guineafowl" = "Birds + raptor",
  "Guniea Pig" = "Small mammal",
  "Guinea Pig" = "Small mammal",
  "Hamster" = "Small mammal",
  "Honey Bee" = "others",
  "Japanese Quail" = "Birds + raptor",
  "Khaki Campbell (domestic duck breed)" = "Birds + raptor",
  "Muscovy Duck" = "Birds + raptor",
  "N/A" = "others",
  "Parakeet (Unknown)" = "Birds + raptor",
  "Pig" = "Large mammal",
  "Pitt Bull Mix" = "Large mammal",
  "Rock Dove" = "Birds + raptor",
  "Unknown" = "others" 
)

animal_class_mapping = c(
  "Birds" = "Birds + raptor",
  "Coyotes" = "Large mammal",
  "Deer" = "Large mammal",
  "Fish-numerous quantity" = "fish",
  "Marine Mammals-seals only" = "Marine mammal",
  "Marine Mammals-whales,  Dolphin" = "Marine mammal",
  "Marine Mammals-whales, Dolphin" = "Marine mammal",
  "Marine Reptiles" = "Reptile / amphibian",
  "Non Native Fish-(invasive)" = "fish",
  "Raptors" = "Birds + raptor",
  "Rare, Endangered, Dangerous" = "others",
  "Small Mammals-non RVS" = "Small mammal",
  "Small Mammals-RVS" = "Small mammal",
  "Terrestrial Reptile or Amphibian" = "Reptile / amphibian",
  "Fish-numerous quantity;#Terrestrial Reptile or Amphibian" = "fish",
  "Marine Mammals-whales,  Dolphin" = "Marine mammal",
  "Raptors, Small Mammals-RVS" = "Small mammal",
  "Rare, Endangered, Dangerous" = "others",
  "Rare,  Endangered,  Dangerous" = "others",
  "Terrestrial Reptile or Amphibian, Fish-numerous quantity" = "Reptile / amphibian"
)

park_ranger$animal_class_new = species_mapping[park_ranger$species_description]

missing_species = is.na(park_ranger$animal_class_new)
park_ranger$animal_class_new[missing_species] = animal_class_mapping[park_ranger$animal_class[missing_species]]

table(park_ranger$animal_class_new)
```

    ## 
    ##      Birds + raptor                fish        Large mammal       Marine mammal 
    ##                1664                  11                 574                  29 
    ##              others Reptile / amphibian        Small mammal 
    ##                  14                 180                1456
