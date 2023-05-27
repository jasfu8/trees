library(tidyverse)
library(stringdist)

# Function for species fuzzyjoin

is_string_distance_below_three <- function(left, right) {
  stringdist(left, right, method = "dl") < 3}

# IBP CSV FILE CONTAINS ALL THE CORRECT SPECIES NAMES AND CODES

ibp <-
  read_csv("IBP-AOS-LIST22.csv") %>%
  mutate(species_name = `COMMONNAME`,
         species_code = `SPEC`,
         species_tag = tolower(str_replace_all(species_name, " ", "-")),
         .keep = "none")


#Read in migrant data

migrant <-
  read_csv("migrant.csv") %>%
  
  # Replace "NULL" with actual NA values
  
  mutate_all(~ if_else(. == "NULL", NA, .)) %>%
  
  # Clean columns
  
  mutate(
    
    # Turn following variables into numeric
    
    across(c(bp, cp, fat, body_molt, ff_wear, wing_chord, mass, status),
           as.numeric),
    
    #Capitalize species codes
    
    species_code = toupper(species_code),
    
    #Create a species_tag column for data cleaning
    
    species_tag = tolower(str_replace_all(species_name, " ", "-")),
    
    #Combine band prefix and number into one string
    
    band_full = str_c(band_prefix, band_number, sep = "-")) %>%
  
  # Fix species names
  
  filter(
    
    #Remove all species codes that have more than 4 letters
    
    nchar(species_code) == 4 &
      
      #Remove all species codes that are unknown
      
      !startsWith(species_code, "U") &
      !species_name %in% "?" &
      
      #Remove all codes that refer to "NULL" or band issues
      
      !species_code %in% c("BADE", "BALO", "NULL")) %>%
  
  #Removing all NA values from species names (via species tag) and codes
  
  drop_na(species_tag, species_code) %>%
  
  # Fuzzy join migrant data with 'ibp' to correct species names and codes
  
  fuzzyjoin::fuzzy_left_join(
    ibp,
    by = c("species_tag", "species_code"),
    
    # Only join if string distance is below 3
    
    match_fun = c("species_tag" = is_string_distance_below_three,
                  "species_code" = is_string_distance_below_three)) %>% 
  
  # Use the correct codes and names
  
  mutate(
    species_code = species_code.y,
    species_name = species_name.y) %>%
  
  #Make row id for each observation
  
  rowid_to_column()

rm(ibp, is_string_distance_below_three)

# The below code is how to make datetime from capture/release times

# if(nchar(capture_time) < 4) {
#   x = as_datetime(str_c(year, "-", month, "-", day, " ", "0", capture_time),
#                   format  = "%Y-%m-%d %H%M")
# } else if(str_detect(capture_time, ":")) {
#   x = as_datetime(str_c(year, "-", month, "-", day, " ", capture_time),
#                   format  = "%Y-%m-%d %H:%M")
# } else if(str_detect(capture_time, ";")) {
#   x = as_datetime(str_c(year, "-", month, "-", day, " ", capture_time),
#                   format  = "%Y-%m-%d %H;%M")
# } else if(nchar(capture_time) == 4) {
#   x = as_datetime(str_c(year, "-", month, "-", day, " ", capture_time),
#                   format  = "%Y-%m-%d %H%M")
# }