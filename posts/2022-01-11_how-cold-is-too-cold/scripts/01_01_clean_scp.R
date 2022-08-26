
# Libraries ---------------------------------------------------------------

if (!require(pacman)) install.packages("pacman")
if (!require(rio)) install.packages("rio")
pacman::p_load(tidyverse)
pacman::p_load(lubridate)
pacman::p_load(here)
pacman::p_load(survminer)
pacman::p_load(survival)
pacman::p_load(doBy)
conflicted::conflict_prefer("here", "here")

# Set ggplot theme
theme_set(theme_classic())

# Read data ---------------------------------------------------------------

# Convert .xlsx to .csv
rio::convert(here("data", "2019_supercooling_winter", "mortality_obs.xlsx"),
        here("data", "2019_supercooling_winter", "mortality_obs.csv"))

dat <- read.csv(here("data",
                     "2019_supercooling_winter",
                     "mortality_obs.csv"),
                na.strings = ".")
dat$channel <- as.character(dat$channel)
# Clean data
dat <- dat %>% 
   # Convert dates to dates
   mutate_at(vars(contains("date")), dmy) %>%
   
   # Convert temp to numeric
   mutate(temp_pulled = case_when(
      temp_pulled == "c1" ~ NA_real_,
      temp_pulled == "c2" ~ NA_real_,
      temp_pulled == "c3" ~ NA_real_,
      temp_pulled == "c4" ~ NA_real_,
      TRUE ~ as.numeric(as.character(temp_pulled))
   ),
   # Set degree_min as factor
   degree_min = as.factor(degree_min),
   # Change control name to be all control
   channel = as.factor(case_when(channel == "c1" ~ "control",
                       channel == "c2" ~ "control",
                       channel == "c3" ~ "control",
                       channel == "c4" ~ "control",
                       TRUE ~ channel))
   ) %>% 
   
   # Clean up these factors by converting "" to NA. Could only figure out how to
   # do it by first converting the variables to character.
   mutate_at(vars(discolor_7day,
                  discolor_3day,
                  movement_7day,
                  movement_3day,
                  stage_l_p_w_3day,
                  stage_l_p_w_7day,
                  eclose),
             as.character) %>% 
   mutate_at(vars(discolor_7day,
                  discolor_3day,
                  movement_7day,
                  movement_3day,
                  stage_l_p_w_3day,
                  stage_l_p_w_7day,
                  eclose),
             .funs = list(~case_when(
                . == "" ~ NA_character_,
                TRUE ~ as.character(.)
             ))) %>% 
   # Convert movement variables to 0 (no movement obs) or 1 (mvmt obs)
   mutate_at(vars(movement_3day,
                  movement_7day,
                  eclose),
             .funs = list(~case_when(
                . == "N" ~ 0,
                . == "Y" ~ 1,
                . == "COCC" ~ 0,
                TRUE ~ as.numeric(.)
             ))) %>% 
   # Convert discolored variables to 0 (discolored) or 1 (not discolored)
   mutate_at(vars(discolor_3day,
                  discolor_7day),
             .funs = list(~case_when(
                . == "N" ~ 1,
                . == "Y" ~ 0,
                TRUE ~ as.numeric(.)
             ))) %>% 
   # Add a second temp_pulled category to group all temperatures
   # The chiller bath runs into issues at ~32 C, so we'll group all < -32
   mutate(temp_pulled_grouped = case_when(
      temp_pulled <= -32 ~ -32,
      temp_pulled > -32 ~ temp_pulled
   )) %>% 
   # Add UID
   mutate(uid = 1:nrow(.)) 


   # Add trial_id for random effects model
dat$trial_id <- paste0(as.character(dat$date_SC),
                       as.character(dat$trial)) %>% 
   as.factor(.)

dat$degree_min_continuous <- as.numeric(as.character(dat$degree_min))

# Remove unneeded columns
dat <- dat %>% 
   select(date_SC, trial, channel, degree_min, temp_pulled,
          discolor_3day, est_scp, eclose, temp_pulled_grouped,
          uid, trial_id, degree_min_continuous)

#write_csv(here("data", "cleaned_scp_data.csv"))



