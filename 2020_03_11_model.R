# Visualizes the COVID-19 Transmission model data using R
# Models by: Ashleigh Tuite, Amy Greer, and David Fisman
# Model as of: 10-Mar-2020 (Model 3)
# R Syntax by: Mathew Roy
# Last updated: March 20, 2020

# Clear environment
rm(list = ls())


# Load packages
packages <- c("readxl", "dplyr", "stringr", "janitor", "tidyr", "lubridate", "ggplot2", "scales")
sapply(packages, require, character.only = TRUE)


# Set working directory and other parameters
setwd("C:\\covid_19_model")
folder_with_model_data <- "Model 3" ## subfolder with model data 
model_files <- dir(path = folder_with_model_data, pattern = "daily.*.xlsx$")
my_regions <- c("Essex, Ontario")


# Summarize population estimates by region and age group
# 2019 (July 1) population estimates downloaded from: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710008801
pop <- read_excel(path = "stats_can_population_estimates_2019.xlsx", sheet = "data") %>% 
  select(year = "REF_DATE", geo = GEO, age = `Age group`, sex = Sex, pop = VALUE) %>% 
  filter(grepl("Ontario", geo)) %>% 
  mutate(age = as.numeric(stringr::str_extract(string = age, pattern = "\\d{1,}")),
         age_group = ifelse(age < 15, "Under 15 years", "15+ years")) %>% 
  group_by(geo, year, age_group) %>% 
  summarize(pop = sum(pop)) %>% 
  bind_rows((.) %>% group_by(year, geo) %>% summarise(pop = sum(pop)) %>% mutate(age_group = "All")) %>% 
  ungroup %>% 
  mutate(age_group = factor(x = age_group, levels = c("All", "Under 15 years", "15+ years"))) %>% 
  arrange(geo, year, age_group)


# Region of interest (used for merging with model_data later on to get counts for a particular region)
# table(pop$geo)
pop_region <- pop %>% filter(geo %in% my_regions) %>% select(geo, year, age_group, pop)


# Import and combine model data from the folder with model data
# Convert wide dataset into long for eventual use with vizualization tools
model_data <- purrr::map2_df(.x = paste0(folder_with_model_data,"\\",model_files), .y = model_files, .f = function(f, g){
  model_sheet_names <- excel_sheets(path = f)
  # combine every sheet from one spreadsheet into one dataset
  purrr::map_df(.x = model_sheet_names, .f = function(s){
    data <- read_excel(path = f, sheet = s)
  }) %>% 
  mutate(scenario = str_replace_all(string = g, pattern = "daily-outputs-|dailyOutputs_|.xlsx", replacement = ""),
         scenario = str_replace_all(string = scenario, pattern = "_", replacement = " ")) %>% 
  select(scenario, everything())
}) %>% 
  janitor::clean_names(case = "snake") %>% 
  #mutate(day = lubridate::as.period(x = day, unit = "day")) %>% 
  mutate(age_group = factor(x = age_group, levels = c("All", "Under 15 years", "15+ years"))) %>% 
  # convert to long
  gather(key = "var", value = "value", -c(scenario, day, age_group)) %>% 
  mutate(var = ifelse(grepl("_cr_i",var), var, paste0(var, "_est")),
         type = case_when(grepl("_upper_cr_i",var) ~ "ucr", 
                          grepl("_lower_cr_i",var) ~ "lcr", 
                          TRUE ~ "est")) %>% 
  group_by(type) %>% 
  # convert to wide (making estimates and intervals into 3 separate columns)
  group_split() %>% 
  purrr::map(., function(x){
    x %>% spread(key = type, value = value) %>% 
      mutate(var = str_replace_all(var, "_cr_i|_est",""))
  }) %>% 
  bind_cols() %>% 
  select(-ends_with("1"),-ends_with("2")) %>% 
  mutate_at(vars(est, ucr, lcr), as.numeric) %>% 
  mutate(rate_or_count = "rate") %>% 
  select(scenario, age_group, var, rate_or_count, day, est, ucr, lcr) %>% 
  arrange(scenario, age_group, var, rate_or_count, day)


# Calculate counts for specific region(s)
# Add daily counts and cumulative counts
final <- merge(model_data, pop_region, by = "age_group") %>%
  mutate_if(names(.) %in% c("est", "lcr", "ucr"), .funs = ~ ./1000*pop) %>% 
  mutate(rate_or_count = case_when(grepl("^total_", var) ~ "cumulative count",
                                   TRUE ~ "count")) %>% 
  bind_rows( (.) %>% 
    filter(rate_or_count == "count") %>% 
    group_by(geo, scenario,  age_group, var) %>% 
    arrange(geo, scenario,  age_group, var, day) %>% 
    mutate_at(vars(c(est, ucr, lcr)), .funs = ~cumsum(.)) %>% 
    mutate(rate_or_count = "cumulative count")
  ) %>% 
  bind_rows(model_data %>% 
              mutate(geo = "All")) %>% 
  mutate_at(vars(scenario,var,rate_or_count),.funs =  ~str_to_sentence(str_replace_all(string = ., pattern = "_|-", replacement =" "))) %>% 
  arrange(geo, scenario, age_group, var, rate_or_count, day) %>% 
  select(Region = geo, Scenario = scenario, `Age group` = age_group, Day = day, Outcome = var, `Rate or count` = rate_or_count, Estimate = est, lcr, ucr) 


## Sample plots
# Outcome of interest
x <- c("New cases", "Total deaths")
# Units of interest
y <- c("Count", "Cumulative count", "Rate")
# Age group of interest
z <- c("All")
# Scenarios of interest
s <-
  c(
    "Basecase",
    "Interv 1 school closure",
    "Interv 2 remote work",
    "Interv 3 social distancing",
    "Interv 4 improved detection and isolation",
    "Interv 5 improved detection plus social distancing"
  )

# Create multiple plots at one go
# Iterate through each outcome (x)
plots <- purrr:::map(.x = x, .f = function(x){
  # Iterate through each unit (e.g. rate, count etc. (y))
  purrr:::map(.x = y, .f = function(y){
    # Iterate through each age group (z)
    purrr:::map(.x = z, .f = function(z){
      # Filter dataset based on values of x, y, and z
      dataset <- final %>% filter(Region %in% c(my_regions, "All") | is.na(Region), `Age group` == z, `Rate or count` == y, Outcome == x)
      
      my_var <- dataset$Outcome %>% as.character() %>% .[1]
      my_rate_or_count <- dataset$`Rate or count` %>% as.character() %>% .[1]
      my_age_group <- dataset$`Age group`  %>% as.character() %>% .[1]
      my_regions_names <- paste(my_regions, sep = "", collapse =", ")
      
      # Plot theme that will be applied to every plot
      plot_theme <- list(
        geom_line(aes(y = Estimate), size = 1,show.legend = F),
        geom_ribbon(aes(ymin = lcr, ymax=ucr), linetype=3, alpha=0.1, size = 0.75,show.legend = F),
        scale_y_continuous(name = ifelse(grepl("Rate", my_rate_or_count, ignore.case = T), "Rate per 1,000 residents", "Number of people"),
                           labels = ifelse(grepl("Rate", my_rate_or_count, ignore.case = T), scales::number_format(accuracy = 0.1), scales::comma_format())),
        scale_x_continuous(name = "Days since day of first 20 local cases"),
        labs(title = paste0(ifelse(my_var == "Total deaths" & my_rate_or_count == "Cumulative count", "Cumulative death count", 
                            ifelse(my_var == "Total deaths" & my_rate_or_count == "Rate", "Cumulative death rate",
                            paste0(my_rate_or_count," of ", str_to_lower(my_var)))), " (Age group: ",my_age_group,")"),
             subtitle = ifelse(my_rate_or_count == "Rate", "", my_regions_names)),
        theme(plot.title = element_text(size = 18),
              plot.subtitle = element_text(size = 14),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 11),
              axis.text.y = element_text(size = 11),
              strip.text = element_text(size = 11))
      )
      
      # Iterate through each scenario and make the plot, and name the plot (list element) based on the value of s
      individual <- ggplot(data = dataset, aes(x = Day, group = Scenario, colour = Scenario)) +
        facet_wrap(~Scenario, labeller = labeller(Scenario = label_wrap_gen(30,multi_line = T))) +
        plot_theme
  
    }) %>% rlang::set_names(z)
  })   %>% rlang::set_names(y)
}) %>% rlang::set_names(x)


# Function to unlist a nested list into a single level
# Source: https://stackoverflow.com/questions/16300344/how-to-flatten-a-list-of-lists
flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

plots <- flattenlist(plots)

# Remove any plots with outcome total deaths and unit type count
plots <- plots[-grep("Total deaths.*Count",names(plots))]

# Show one plot
plots[["New cases.Count.All"]]
plots[[2]]

# Show all plots
plots

# Saving plots as jpegs into a subfolder called "Plots"
#dir.create(path = "Plots")
purrr::map2(.x = plots, .y = names(plots),  function(plot_object, plot_name){
  ggsave(paste0("Plots\\",plot_name,".jpg"), plot_object, width = 11, height = 8.5)
})


## For Power BI

# Export to CSV for use with Power BI dashboards
# final %>% write.csv(x = ., file = "2020_03_11_model_data_essex_cd.csv", fileEncoding = "utf8", row.names = F)

# Other tables for Power BI (to help with sorting variables)
# bind_cols(data.frame(order = seq(1:3)),data.frame(`Age group` =unique(final$`Age group`))) %>% 
#   write.csv(x = ., file = "order_age_group.csv", row.names = F)
# bind_cols(data.frame(order = seq(1:7)),data.frame(`Outcome` = c("New cases",
#                                                                "Mild cases seeking care",
#                                                                "Number in quarantine isolation",
#                                                                "New hospitalizations",
#                                                                "Cases in hospital",
#                                                                "Cases in icu",
#                                                                "Total deaths"))) %>% 
#   write.csv(x = ., file = "order_outcome.csv", row.names = F)


# Sample script to add inside Power BI's "R script visual"
# library(dplyr)
# library(ggplot2)
# library(scales)
# library(stringr)
# 
# my_var <- dataset$Outcome %>% as.character() %>% .[1]
# my_rate_or_count <- dataset$`Rate or count` %>% as.character() %>% .[1]
# my_age_group <- dataset$`Age group`  %>% as.character() %>% .[1]
# my_regions_names <- "Windsor and Essex County"
# 
# ggplot(data = dataset, aes(x = Day, group = Scenario, colour = Scenario)) +
#   geom_line(aes(y = Estimate), size = 1) +
#   {if(length(unique(dataset$Scenario)<6) == TRUE) 
#     geom_ribbon(aes(ymin = lcr, ymax=ucr), linetype=3, alpha=0.1, size = 0.75)
#   } +
#   scale_y_continuous(name = ifelse(grepl("Rate", my_rate_or_count, ignore.case = T), "Rate per 1,000 residents", "Number of people"),
#                      labels = ifelse(grepl("Rate", my_rate_or_count, ignore.case = T), scales::number_format(accuracy = 0.1), scales::comma_format())) +
#   scale_x_continuous(name = "Days since day of first 20 local cases") +
#   labs(title = paste0(ifelse(my_var == "Total deaths" & my_rate_or_count == "Cumulative count", "Cumulative death count", 
#                              ifelse(my_var == "Total deaths" & my_rate_or_count == "Rate", "Cumulative death rate",
#                                     paste0(my_rate_or_count," of ", str_to_lower(my_var)))), " (Age group: ",my_age_group,")")) +
#   theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 11),
#         plot.title = element_text(size = 18),
#         plot.subtitle = element_text(size = 14),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         axis.text.x = element_text(size = 11),
#         axis.text.y = element_text(size = 11))
# 
