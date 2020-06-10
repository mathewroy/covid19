# Title; Calculating the time-varying reproduction number
# Author: Mathew Roy
# Created on: June 10, 2020 
# Last updated on: June 10, 2020

# README:

# Assumptions:,
# The first case is imported,
# Serial interval follows a parametric distribution with mean ", my_si_mean, " and standard deviation ", my_si_std.
# The most accurate episode date is a very close approximation of the onset date.

# Other notes:
# Reproductive number was estimated using a sliding window length of 7 days.
# Rt estimates for the most recent week may be underestimates due to reporting delays.
# Created using EpiEstim package for R, version 2.2-1

# Load required packages
library(readxl)
library(dplyr)
library(janitor)
library(incidence)
library(EpiEstim)
library(lubridate)
library(ggplot2)

# Import confirmed case data (line list form)
cd <- read_excel(path = "C:\\case_line_list_data.xlsx",  sheet = "sheet_name")

# Incidence object
daily_i <- incidence(dates = cd$accurate_episode_date)

# Configuration
# Serial interval mean and standard deviation
# Source: https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4.full.pdf
my_si_mean <- 3.96
my_si_std <- 4.75

# Estimate Rt
res_parametric_si <- estimate_R(incid = daily_i, 
                                method = "parametric_si", 
                                config = make_config(list(mean_si = my_si_mean, 
                                                          std_si = my_si_std)))
res_parametric_si

# Plot Rt
first_date_plot <- as.Date("2020-02-23") # Sunday
last_date_plot <- floor_date(Sys.Date(), "weeks", 7) + 7

plot(res_parametric_si, what = "R") +
  scale_x_date(name = "Week of symptom onset (2020)",
               breaks = seq(first_date_plot, last_date_plot, 7),
               labels = seq(first_date_plot, last_date_plot, 7),
               limits = c(first_date_plot - 1, last_date_plot),
               date_labels = "%b %d"
  ) 
  
# Export to csv
rt_data <- plot(res_parametric_si, what = "R")$data

write.csv(x = rt_data, 
          file = paste0("C:\\",Sys.Date(),"_covid_reproductive_number.csv"))
