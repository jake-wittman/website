
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

# Read in data ------------------------------------------------------------
UMN_air <- read_csv(here::here("data/hobo_data/UMN_Air.csv"),
                    skip = 1,
                    col_names = c("date_time", "temp_F", "RH"),
                    col_types = "cdd")
UMN_snow <- read_csv(here::here("data/hobo_data/UMN_Snow.csv"),
                     skip = 1,
                     col_names = c("date_time", "temp_F", "RH"),
                     col_types = "cdd")
waseca_air <- read_csv(here::here("data/hobo_data/Waseca_Air.csv"),
                       skip = 1,
                       col_names = c("date_time", "temp_F", "RH"),
                       col_types = "cdd")
waseca_snow <- read_csv(here::here("data/hobo_data/Waseca_Snow.csv"),
                        skip = 1,
                        col_names = c("date_time", "temp_F", "RH"),
                        col_types = "cdd")
morris_air <- read_csv(here::here("data/hobo_data/Morris_Air.csv"),
                       skip = 1,
                       col_names = c("date_time", "temp_F", "RH"),
                       col_types = "cdd")
morris_snow <- read_csv(here::here("data/hobo_data/Morris_Snow.csv"),
                        skip = 1,
                        col_names = c("date_time", "temp_F", "RH"),
                        col_types = "cdd")
open_air_insectary <- read_csv(here::here("data/hobo_data/open_air_insectary.csv"),
                               skip = 1,
                               col_names = c("date_time", "temp_F", "RH"),
                               col_types = "cdd")
incubator7 <- read_csv(here::here("data/hobo_data/right_incubator.csv"),
                       skip = 1,
                       col_names = c("date_time", "temp_F", "RH"),
                       col_types = "cdd")
incubator8 <- read_csv(here::here("data/hobo_data/left_incubator.csv"),
                       skip = 1,
                       col_names = c("date_time", "temp_F", "RH"),
                       col_types = "cdd")

all_hobos <- bind_rows(Morris_Air = morris_air,
                       Morris_Snow = morris_snow,
                       DE_Air = open_air_insectary,
                       incubator7 = incubator7,
                       incubator8 = incubator8,
                       UMN_Air = UMN_air,
                       UMN_Snow = UMN_snow,
                       Waseca_Air = waseca_air,
                       Waseca_Snow = waseca_snow,
                       .id = "location")

all_hobos <- all_hobos %>% 
   mutate(date_time_true = mdy_hm(date_time),
          temp_C = (temp_F - 32) * (5 / 9),
          location = case_when(
             date_time_true > as.Date("2020-03-25") & location == "Morris_Air" ~ "incubator1",
             date_time_true > as.Date("2020-03-25") & location == "Morris_Snow" ~ "incubator2",
             date_time_true > as.Date("2020-03-25") & location == "UMN_Air" ~ "incubator3",
             date_time_true > as.Date("2020-03-25") & location == "UMN_Snow" ~ "incubator4",
             date_time_true > as.Date("2020-03-25") & location == "Waseca_Air" ~ "incubator5",
             date_time_true > as.Date("2020-03-25") & location == "Waseca_Snow" ~ "incubator6",
             TRUE ~ location))

write_csv(all_hobos, here::here("data/cleaned_hobo_dat.csv"))

all_hobos %>% 
   filter(!str_detect(location, "incubator") &
             date_time_true > as.Date("2020-01-01") &
             date_time_true < as.Date("2020-03-25")) %>%
   separate(location, into = c("site", "treatment"), remove = FALSE) %>% 
   ggplot(aes(x = date_time_true, y = temp_C, group = location, color = site)) +
   geom_line() +
   facet_grid(treatment~.) +
   theme_bw() +
   theme(axis.title = element_text(size = 20),
         axis.text = element_text(size = 14, colour = "black"),
         legend.text = element_text(size = 16),
         legend.title = element_text(size = 20),
         strip.text = element_text(size = 20)) +
   labs(y = "Temperature (C)", x = "Date") +
   scale_color_discrete(name = "Site", 
                        labels = c("Delaware", "Morris", "St. Paul", "Waseca"))

all_hobos %>% 
   filter(!str_detect(location, "incubator") &
             date_time_true > as.Date("2020-01-01") &
             date_time_true < as.Date("2020-03-25")) %>%
   separate(location, into = c("site", "treatment"), remove = FALSE) %>% 
   ggplot(aes(x = date_time_true, y = RH, group = location, color = site)) +
   geom_line() +
   facet_grid(treatment~.) +
   theme_bw() +
   theme(axis.title = element_text(size = 20),
         axis.text = element_text(size = 14)) +
   labs(y = "Relative Humidity", x = "Date")

all_hobos <- all_hobos %>% 
   mutate(date_time = mdy_hm(date_time), 
          date = date(date_time))

daily_dat <- all_hobos %>% 
   separate(location, into = c("site", "treatment"), remove = FALSE) %>% 
   group_by(location, date) %>% 
   summarise(site = site,
             treatment = treatment, 
             daily_mintemp = min(temp_C, na.rm = TRUE),
             daily_temp = mean(temp_C, na.rm = TRUE),
             daily_meanRH = mean(RH, na.rm = TRUE),
             daily_maxtemp = max(temp_C, na.rm = TRUE)) %>% 
   ungroup()

daily_dat$lag_mintemp <- lag(daily_dat$daily_mintemp)
daily_dat$temp_diff <- daily_dat$daily_maxtemp - daily_dat$lag_mintemp
daily_dat$rate_change <- daily_dat$temp_diff / 720
daily_dat %>%
   group_by(treatment) %>% 
   summarise(mean_rate = mean(rate_change, na.rm = TRUE),
             li = quantile(rate_change, 0.025, na.rm = TRUE),
             ui = quantile(rate_change, 0.975, na.rm = TRUE),
             max_rate = max(rate_change, na.rm = TRUE),
             min_rate = min(abs(rate_change), na.rm = TRUE))


avg_daily <- daily_dat %>% 
   group_by(location) %>% 
   summarise(avg_min_temp = mean(daily_mintemp),
             var_min_temp = sd(daily_mintemp),
             var_RH = sd(daily_meanRH))

mn_hourly_avg <- all_hobos %>% 
   separate(location, into = c("site", "treatment"), remove = FALSE) %>% 
   filter(site == "Morris" | site == "UMN" | site == "Waseca") %>%
   filter(date > "2019-12-31" & date < "2020-03-15") %>% 
   group_by(location) %>%
   mutate(order = 1:n()) %>% 
   ungroup() %>% 
   group_by(treatment, order) %>% 
   summarise(avg_hourly_temp = mean(temp_C),
             stdev = sd(temp_C, na.rm = TRUE))

mn_hourly_avg %>% 
   group_by(treatment) %>% 
   summarise(avg_stdev = mean(stdev),
             max_stdev = max(stdev))
# Average difference between Delaware and other air sites
site_list <- all_hobos %>% 
   separate(location, into = c("site", "treatment"), remove = FALSE) %>% 
   filter(treatment == "Air") %>% 
   group_split(site)

dates_MN <- unique(site_list[[4]]$date)
site_list <- map(site_list, function(.x) {
   .x <- .x %>% 
      filter(date %in% dates_MN) %>% 
      filter(!is.na(temp_F))
})

site_list[[1]] <- site_list[[1]][-c(1:13),]
site_list[[1]] <- site_list[[1]][seq(1, nrow(site_list[[1]]), by = 2),]
DE <- site_list[[1]]
site_list <- site_list[2:4]
site_list <- map(site_list, function(.x) {
   .x$temp_diff_DE <- DE$temp_C - .x$temp_C
   .x
})

map(site_list, ~mean(.x$temp_diff_DE))


# Average minimum temp over the two weeks preceeding the extreme cold on Feb 12
daily_dat %>% 
   separate(location, into = c("site", "treatment"), remove = FALSE) %>% 
   filter(site != "DE") %>% 
   filter(date > "2020-01-29" & date < "2020-02-12") %>% 
   group_by(treatment) %>% 
   summarise(avg_min_temp = mean(daily_mintemp))
# Avg minim temp on the cold snaps
daily_dat %>% 
   separate(location, into = c("site", "treatment"), remove = FALSE) %>% 
   filter(site != "DE") %>% 
   filter(date > "2020-02-12" & date < "2020-02-14") %>% 
   group_by(treatment) %>% 
   summarise(avg_min_temp = mean(daily_mintemp),
             date = date)

daily_dat %>% 
   separate(location, into = c("site", "treatment"), remove = FALSE) %>% 
   filter(site != "DE") %>% 
   filter(date > "2020-02-19" & date < "2020-02-21") %>% 
   group_by(treatment) %>% 
   summarise(avg_min_temp = mean(daily_mintemp),
             date = date)

# Look at avg minimum by treatment excluding DE
daily_dat %>% 
   separate(location, into = c("site", "treatment"), remove = FALSE) %>% 
   filter(site != "DE") %>% 
   group_by(treatment, date) %>% 
   summarise(avg_min_temp = mean(daily_mintemp, na.rm = TRUE),
             avg_avg_RH = mean(daily_meanRH, na.rm = TRUE),
             var_min_temp = sd(daily_mintemp, na.rm = TRUE),
             var_RH = sd(daily_meanRH, na.rm = TRUE)) %>% 
   mutate(min_temp_diff = avg_min_temp - lag(avg_min_temp)) %>% 
   View()
# Summarise temperature time series ---------------------------------------
hobo_summary <- all_hobos %>% 
   group_by(location) %>% 
   summarise(min_temp = min(temp_C, na.rm = TRUE),
             max_temp = max(temp_C, na.rm = TRUE),
             mean_temp = mean(temp_C, na.rm = TRUE),
             sd_temp = sd(temp_C, na.rm = TRUE),
             mean_RH = mean(RH, na.rm = TRUE),
             min_RH = min(RH, na.rm = TRUE),
             max_RH = max(RH, na.rm = TRUE),
             sd_RH = sd(RH, na.rm = TRUE),
             n_temp = n()) %>% 
   left_join(., avg_daily, by = "location") %>% 
   select(location, min_temp, avg_min_temp, mean_RH, everything())


# Save output -------------------------------------------------------------

write_csv(hobo_summary, here::here("data/hobo_data/hobo_summary.csv"))

