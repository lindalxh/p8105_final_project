
# This script contains the cleaned and processed data frames.
# It is intended to be sourced by other R Markdown files in the project.

# Load packages

library(tidyverse)
library(hms)
library(lubridate)
library(viridis)
library(ggthemes)
library(scales)
library(glmnet)
library(patchwork)
library(fuzzyjoin)
library(knitr)
library(base64enc)

############################## park_ranger_new ##############################

# 1. Split date and time
# 2. Remove incorrect records based on call & response time
# 3. Rename variable `duration_of_response` to `duration_of_action`

park_ranger = read_csv("data/Urban_Park_Ranger_Animal_Condition_Response_20251103.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    date_and_time_of_initial_call = mdy_hm(date_and_time_of_initial_call),
    date_and_time_of_ranger_response = mdy_hm(date_and_time_of_ranger_response),
    
    call_date = as.Date(date_and_time_of_initial_call),
    call_time = as_hms(date_and_time_of_initial_call),
    response_date = as.Date(date_and_time_of_ranger_response),
    response_time = as_hms(date_and_time_of_ranger_response)
  ) %>% 
  filter(date_and_time_of_ranger_response >= date_and_time_of_initial_call) %>% 
  rename(duration_of_action = duration_of_response)

# 4. Reclassify animal classes into 7 categories
# 5. Factor categorical variables & set reference level

park_ranger_new = park_ranger %>% 
  mutate(
    animal_class_new = case_when(
      animal_class %in% c(
        "[\"Birds\"]", "[\"Raptors\"]", "Birds", "Raptors", 
        "Raptors, Small Mammals-RVS"
      ) ~ "Birds / Raptors",
      
      animal_class %in% c(
        "[\"Small Mammals-non RVS\"]", "[\"Small Mammals-RVS\"]",
        "Small Mammals-non RVS", "Small Mammals-RVS"
      ) ~ "Small Mammals",
      
      animal_class %in% c(
        "Coyotes", "Deer", "[\"Coyotes\"]", "[\"Deer\"]"
      ) ~ "Large Mammals",
      
      animal_class %in% c(
        "[\"Marine Mammals-whales, Dolphin\"]", "[\"Marine Mammals-seals only\"]",
        "Marine Mammals-seals only", "Marine Mammals-whales,  Dolphin", 
        "Marine Mammals-whales, Dolphin"
      ) ~ "Marine Mammals",
      
      animal_class %in% c(
        "[\"Fish-numerous quantity\"]", "[\"Non Native Fish-(invasive)\"]", 
        "Fish-numerous quantity", "Fish-numerous quantity;#Terrestrial Reptile or Amphibian", 
        "Non Native Fish-(invasive)"
      ) ~ "Fish",
      
      animal_class %in% c(
        "[\"Marine Reptiles\"]", "[\"Terrestrial Reptile or Amphibian\"]", 
        "Marine Reptiles", "Terrestrial Reptile or Amphibian", 
        "Terrestrial Reptile or Amphibian, Fish-numerous quantity"
      ) ~ "Reptiles / Amphibians",
      
      animal_class %in% c(
        "[\"Rare, Endangered, Dangerous\"]", "Rare,  Endangered,  Dangerous",
        "Rare, Endangered, Dangerous"
      ) ~ "Others",
      
      species_description %in% c(
        "Atlantic Canary","Bird (Unknown)","Budgerigar","Budgerigar Parakeet",
        "Chicken","Chukar","Cockatiel","Domestic Dove","Domestic Duck", "Domestic duck",
        "Domestic Goose","Domestic Goose Hybrid","Domestic Quail","Domestic Turkey",
        "Domestic Waterfowl","Fancy Dove","Guinea Fowl","Guineafowl","Japanese Quail",
        "Khaki Campbell","Muscovy Duck","Rock Dove",
        "Parakeet (Unknown)"
      ) ~ "Birds / Raptors",
      
      species_description %in% c(
        "Domestic Rabbit","Domestic Ferret","Fancy Rat","Gerbil",
        "Guinea Pig", "Guniea Pig", "Guinea pig", "Hamster"
      ) ~ "Small Mammals",
      
      species_description %in% c(
        "Dog","Pitt Bull Mix","Cat","Goat","Cattle","Pig"
      ) ~ "Large Mammals",
      
      species_description %in% c(
        "Animal (Unknown)","Honey Bee", "Unknown","N/A"
      ) ~ "Others"),
    
    animal_class_new = factor(animal_class_new, 
                              levels = c("Birds / Raptors", "Small Mammals", "Large Mammals", 
                                         "Marine Mammals", "Reptiles / Amphibians", "Fish", "Others")),
    
    borough = factor(borough, levels = c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")),
    
    call_source = factor(call_source, levels = c("Public", "Central", "Employee", 
                                                 "Conservancies/\"Friends of\" Groups", 
                                                 "Observed by Ranger", "WBF", "WINORR", "Other")),
    
    species_status = ifelse(species_status == "N/A" | is.na(species_status), 
                            "Unknown", species_status),
    species_status = factor(species_status, levels = c("Native", "Invasive", "Domestic", 
                                                       "Exotic", "Unknown")),
    
    animal_condition = ifelse(animal_condition == "N/A" | is.na(animal_condition), 
                              "Unknown", animal_condition),
    animal_condition = factor(animal_condition, levels = c("Healthy", "Unhealthy", "Injured", 
                                                           "DOA", "Unknown")),
    
    adult   = if_else(str_detect(age, "Adult"), 1L, 0L, missing = 0L),
    juvenile = if_else(str_detect(age, "Juvenile"), 1L, 0L, missing = 0L),
    infant  = if_else(str_detect(age, "Infant"), 1L, 0L, missing = 0L)
  )

############################## map_boro ##############################

# import borough dataset
boro_bound_df = read_csv("data/Borough_Boundaries_20251107.csv") |> 
  janitor::clean_names()

#create a dataframe for plotting
# get all animal classes
animal_classes = c(
  "Birds / Raptors", 
  "Small Mammals", 
  "Large Mammals",
  "Marine Mammals", 
  "Reptiles / Amphibians",
  "Fish", 
  "Others")

# select a subset of park ranger data, and only keep Unhealthy and Injured cases
# group by borough and animal class
# get the number of cases for each animal class in each borough
# get total number of cases for each borough
# join the multipolygon geometry into this dataset
map_boro = park_ranger_new |> 
  select(borough, animal_class_new, animal_condition) |> 
  group_by(borough, animal_class_new) |> 
  summarize(n = n(), .groups = "drop") |> 
  pivot_wider(
    names_from = animal_class_new,
    values_from = n) |> 
  mutate(
    total_n = rowSums(across(all_of(animal_classes)), na.rm = TRUE)) |> 
  left_join(boro_bound_df |> select(boro_name, the_geom),
            by = c("borough" = "boro_name"))

############################## map_park ##############################

# Match park names with Park Properties names
# 1. Import parks properties dataset
park_prop_df = read_csv("data/Parks_Properties_20251107.csv") |> 
  janitor::clean_names()

# 2. Get park names from both datasets
park_name_park_ranger = park_ranger_new |>
  transmute(name_park_ranger = str_to_lower(str_trim(property))) |>
  distinct() |>
  arrange(name_park_ranger)

park_name_park_prop = park_prop_df |>
  transmute(name_park_prop = str_to_lower(str_trim(signname))) |>
  distinct() |>
  arrange(name_park_prop)

# 3. Join datasets on inexact matching, left join on ranger data
# 4. Use Jaro-Winkler algorithm, save pairs with lowest distance (highest similarity)
fuzzy_matches = 
  stringdist_join(
    park_name_park_ranger, park_name_park_prop,
    by = c("name_park_ranger" = "name_park_prop"),
    mode = "left",
    method = "jw", max_dist = 0.25,
    distance_col = "distance") |> 
  group_by(name_park_ranger) |>
  slice_min(distance, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    similarity = 1 - distance,
    original_park = name_park_ranger,
    matched_park = name_park_prop) |> 
  select(original_park, matched_park, similarity) |> 
  arrange(original_park)

# 5. Output as a .csv file for manual matching 
write.csv(fuzzy_matches, "data/fuzzy_park_match.csv", row.names = FALSE)

# Create a new dataframe for mapping
# 1. Import dataset with final matched park names
park_match_df = read_csv("data/park_match_final.csv") |> 
  janitor::clean_names() |> 
  transmute(
    original_park = str_to_lower(str_trim(original_park)),
    final_match = str_to_lower(str_trim(final_match)))

# 2. Join the matched name to park_ranger_new, remove those don't have a matched park
map_park = park_ranger_new |> 
  select(property, animal_class_new) |> 
  mutate(property = str_to_lower(str_trim(property))) |> 
  left_join(park_match_df,
            by = c("property" = "original_park")) |> 
  select(final_match, animal_class_new) |> 
  filter(!final_match %in% c("na"))

# 3. Maintain the order of parks for efficient map rendering 
# 4. Keep those with largest acres if there are more than one parks have the same name
# 5. Join map_park to park property with row expansions, remove parks with no cases
map_park = park_prop_df |> 
  select(borough, signname, multipolygon, acres) |> 
  mutate(
    order = row_number(),
    name_park_prop = str_to_lower(str_trim(signname)),
    acres = as.numeric(acres)) |> 
  group_by(name_park_prop) |> 
  slice_max(order_by = acres, n = 1) |> 
  ungroup() |> 
  select(!acres) |> 
  left_join(map_park,
            by = c("name_park_prop" = "final_match")) |> 
  filter(!is.na(animal_class_new)) 

# 6. Get the number of cases for each animal class in each park
# 7. Keep one row for each park and arrange the df based on original mapping order
# 8. Change borough name to its full name
# 9. Get total number of cases for each park
map_park = map_park |> 
  group_by(name_park_prop, animal_class_new) |> 
  summarize(n = n(), .groups = "drop") |> 
  pivot_wider(
    names_from = animal_class_new,
    values_from = n) |> 
  left_join(map_park |> select(!animal_class_new),
            by = c("name_park_prop" = "name_park_prop")) |> 
  distinct(name_park_prop, .keep_all = TRUE) |> 
  arrange(order) |> 
  mutate(
    borough = case_match(borough,
                         "M" ~ "Manhattan",
                         "B" ~ "Brooklyn",
                         "R" ~ "Staten Island",
                         "Q" ~ "Queens",
                         "X" ~ "Bronx"),
    total_n = rowSums(across(all_of(animal_classes)), na.rm = TRUE))









