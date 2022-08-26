
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)

# Read data ---------------------------------------------------------------
dat <- read_csv(here::here("data/sp_galinae_emergence_peeling_data.csv")) %>%
   clean_names()   

# Add more cleaning steps as necessary


# Save cleaned data -------------------------------------------------------

write_csv(dat, path = here::here("data/cleaned_sgalinae_emergence_peeling.csv"))
