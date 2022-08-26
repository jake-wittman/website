
# Libraries ---------------------------------------------------------------

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)
pacman::p_load(testthat)
pacman::p_load(furrr)
pacman::p_load(lubridate)
pacman::p_load(janitor)
pacman::p_load(lemon)
pacman::p_load(zoo)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
# Set up parallel for furrr
plan(multisession)

# Read in data ------------------------------------------------------------
files <- fs::dir_ls(path = "data/2019_supercooling_winter", glob = "*trial*csv")
scp_files <- future_map(files,
            read_csv,
            skip = 7)
# Test that all data frames have 19 columns. If TRUE, good to go.
all(map_lgl(scp_files, function(.x) {
   test_that("The right number of columns are in each data.frame",
             expect_equal(ncol(.x), 19))
   })
)


# Clean data --------------------------------------------------------------
# Clean up date time stuff
scp_files <- map(scp_files, function(.x) {
   .x <- clean_names(.x, case = "snake")
   .x <- .x %>% 
      mutate(uid = 1:nrow(.x))
   .x$date_time <- mdy_hms(.x$date_time)
   .x$date <- date(.x$date_time)
   .x$seconds <- time(.x$date_time)
   return(.x)
})
# Convert to long. Also clean up temperature data to make graphing easier
scp_files_long <- map(scp_files, function(.x) {
   .x <- .x %>% 
      gather(channel, temp, channel0:channel15)
   .x$channel <- as.factor(.x$channel)
   .x$temp <- as.numeric(.x$temp)
   .x <- .x %>% 
      mutate(temp_NA = case_when(
         temp > 5 ~ NA_real_,
         temp > -45 ~ temp,
         temp < -45 ~ NA_real_
   ))
   return(.x)
})

# Cooling rates -----------------------------------------------------------
# This code doesn't seem to be working for some reason?
# lagTemp <- function(x) {
#    x - lag(x)
# }
# coolRates <- function(x) {
#    (x / 0.5) * 60
# }
# cooling_rates <- map(scp_files, function(.x) {
#    y <- .x %<>% 
#       mutate_at(vars(contains("channel")), list("lag" = ~lagTemp)) %>% 
#       mutate_at(vars(contains("lag")), list("rate" = ~coolRates)) %>% 
#       select(contains("rate"), seconds) %>% 
#       map_dfc(function(.x) {
#       rollmean(.x, k = 60)
#    })
#    return(y)
# })
# 
# 
# cooling_rates_long <- map(cooling_rates, function(.x) {
#    .x <- .x %>% 
#       select(contains("rate"), seconds) %>% 
#       gather(channel, cool_rate, channel0_lag_rate:channel15_lag_rate)
#    .x$channel <- as.factor(.x$channel)
#    .x$cool_rate <- as.numeric(.x$cool_rate)
#    return(.x)
# })
# 
# avg_cooling_rates <- cooling_rates_long %>% 
#    map(function(.x) {
#       .x %>% 
#          group_by(channel) %>% 
#          summarise(mean_cool_rate = mean(cool_rate, na.rm = TRUE),
#                    sd_cool_rate = sd(cool_rate, na.rm = TRUE)) %>% 
#          filter(mean_cool_rate != 0)
#    })
# avg_cooling_rates
# 
# cool_rate_plots <- map2(cooling_rates_long,
#                         names(cooling_rates_long),
#                         function(.x, .y) {
#                            .x %>%
#                               filter(cool_rate < 0.25 &
#                                         cool_rate > -1.5) %>%
#                               ggplot(aes(
#                                  x = seconds,
#                                  y = cool_rate,
#                                  group = channel,
#                                  colour = channel
#                               )) +
#                               geom_point(alpha = 0.01) +
#                               geom_smooth(alpha = 1) +
#                               labs(x = "Time (s)",
#                                    y = "Cooling Rate (C/min)",
#                                    title = paste(.y)) +
#                               theme_bw() +
#                               theme(legend.position = "none") +
#                               facet_wrap(~channel)
#                         })
# for(i in 1:length(cool_rate_plots)){
#    ggsave(cool_rate_plots[[i]],
#           filename = paste0("figures/cool_rate_", i, ".png"),
#           width = 12,
#           height = 8,
#           units = "in")
# }

# Plots -------------------------------------------------------------------

plots <- map(scp_files_long, function(.x) {
   .x %>% 
      filter(temp < 0) %>% 
      ggplot(aes(x = seconds, y = temp, group = channel, colour = channel)) +
      geom_line() +
      labs(x = "Time (s)", y = "Temp (C)") +
      scale_y_continuous(limits = c(-28, -20)) +
      theme_bw()
})
# Create a custom function to manually adjust the graphs to find super cooling
# points
scpPlotter <- function(plot,
                       low_y = -28,
                       high_y = -20,
                       low_x,
                       high_x,
                       facet = FALSE) {
   if (facet == FALSE) {
      plot +
         scale_x_continuous(limits = c(low_x, high_x)) +
         scale_y_continuous(limits = c(low_y, high_y))
   } else {
      plot +
         scale_x_continuous(limits = c(low_x, high_x)) +
         scale_y_continuous(limits = c(low_y, high_y)) +
         facet_rep_wrap(~ channel)
   }

}

# Work flow
# First, plot just calling object plot$`filename`
# If no lines visible, use scpPlotter to set max y to 0. Check for exotherms on full graph
# If it looks like there are any exotherms, facet
# If it's a trial at the lower reaches of the temperature, don't need to set y values.
scpPlotter(plots$`data/2019_supercooling_winter/12_3_2019_ct_trial_c.csv`,
           high_y = 0,
           low_x = 0, high_x = 6000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/12_3_2019_ct_trial_f.csv`,
           low_x = 0, high_x = 6000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/13_3_2019_ct_trial_a.csv`,
           high_y = 0,
           low_x = 0, high_x = 30000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/14_3_2019_ct_trial_c.csv`,
           high_y = 0,
           low_x = 0, high_x = 6000,
           facet = TRUE) 

scpPlotter(plots$`data/2019_supercooling_winter/14_3_2019_ct_trial_d.csv`,
           low_x = 6000, high_x = 8000,
           low_y = -28, high_y = -22,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/14_3_2019_ct_trial_e.csv`,
           low_x = 0, high_x = 4000,
           low_y = -20, high_y = 0,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/15_2_2019_ct_trial_e.csv`,
           low_x = 0, high_x = 2000,
           low_y = -10, high_y = -5,
           facet = TRUE)


scpPlotter(plots$`data/2019_supercooling_winter/15_3_2019_ct_trial_c1.csv`,
           high_y = 0,
           low_x = 0, high_x = 6000)

scpPlotter(plots$`data/2019_supercooling_winter/15_3_2019_ct_trial_c2.csv`,
           high_y = 0,
           low_x = 0, high_x = 6000)

scpPlotter(plots$`data/2019_supercooling_winter/18_2_2019_ct_trial_d_pt1.csv`,
           low_y = -12, high_y = -4,
           low_x = 2500, high_x = 4000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/15_3_2019_ct_trial_e.csv`,
           low_y = -20, high_y = 0,
           low_x = 0, high_x = 4000,
           facet = TRUE)


# Not actually cold enough, but check Channel8 - it looks like there is an
# exotherm on channel 8
# this one below is the second half of another trial and is the relevant half
scpPlotter(plots$`data/2019_supercooling_winter/18_2_2019_ct_trial_d_pt2.csv`,
           low_y = -15, high_y = 0,
           low_x = 2500, high_x = 4000,
           facet = TRUE)

# Not cold enough
scpPlotter(plots$`data/2019_supercooling_winter/1_3_2019_ct_trial_e.csv`,
           low_y = -20, high_y = 0,
           low_x = 0, high_x = 8000,
           facet = TRUE)

# Not cold enough
scpPlotter(plots$`data/2019_supercooling_winter/20_2_2019_ct_trial_c.csv`,
           low_y = -20, high_y = 0,
           low_x = 0, high_x = 8000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/21_2_2019_ct_trial_f.csv`,
           low_y = -28, high_y = -22,
           low_x = 3500, high_x = 4500,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/21_2_2019_ct_trial_a.csv`,
           low_y = -10, high_y = -5,
           low_x = 10000, high_x = 15000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/22_2_2019_ct_trial_b.csv`,
           low_y = -28, high_y = -20,
           low_x = 28000, high_x = 35000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/26_3_2019_ct_trial_b.csv`,
           low_y = -28, high_y = -20,
           low_x = 29000, high_x = 36000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/27_3_2019_ct_trial_d.csv`,
           low_y = -28, high_y = -20,
           low_x = 6000, high_x = 8000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/28_3_2019_ct_trial_b.csv`,
           low_y = -28, high_y = -20,
           low_x = 29000, high_x = 38000,
           facet = TRUE)

# Strangely, channel 4 never exothermed at <-20 so check higher
scpPlotter(plots$`data/2019_supercooling_winter/28_3_2019_ct_trial_b.csv`,
           low_y = -6, high_y = -2,
           low_x = 8000, high_x = 10000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/2_4_2019_ct_trial_b.csv`,
           low_y = -28, high_y = -20,
           low_x = 28000, high_x = 38000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/3_4_2019_ct_trial_c.csv`,
           low_y = -20, high_y = -0,
           low_x = 0, high_x = 5000,
           facet = TRUE)

# Misplaced the specimens for this run
scpPlotter(plots$`data/2019_supercooling_winter/4_6_2019_ct_trial_e.csv`,
           low_y = -20, high_y = -0,
           low_x = 0, high_x = 5000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/6_6_2019_ct_trial_d.csv`,
           low_y = -30, high_y = -10,
           low_x = 0, high_x = 10000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/13_6_2019_ct_trial_d.csv`,
           low_y = -30, high_y = -10,
           low_x = 5000, high_x = 15000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/13_6_2019_ct_trial_d2.csv`,
           low_y = -28, high_y = -23,
           low_x = 7000, high_x = 8000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/14_6_2019_ct_trial_f.csv`,
           low_y = -25, high_y = -18,
           low_x = 6000, high_x = 10000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/19_6_2019_ct_trial_a.csv`,
           low_y = -20, high_y = -0,
           low_x = 0, high_x = 15000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/26_6_2019_ct_trial_f.csv`,
           low_y = -29, high_y = -23,
           low_x = 4500, high_x = 8000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/26_6_2019_ct_trial_f2.csv`,
           low_y = -29, high_y = -23,
           low_x = 4500, high_x = 8000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/28_6_2019_ct_trial_a.csv`,
           low_y = -29, high_y = 0,
           low_x = 0, high_x = 36000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/9_7_2019_ct_trial_b.csv`,
           low_y = -29, high_y = -0,
           low_x = 0, high_x = 30000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/9_7_2019_ct_trial_b.csv`,
           low_y = -29, high_y = -22,
           low_x = 30000, high_x = 36000,
           facet = TRUE)

scpPlotter(plots$`data/2019_supercooling_winter/11_7_19_ct_trial_b.csv`,
           low_y = -27, high_y = -20,
           low_x = 30000, high_x = 36000,
           facet = TRUE)
