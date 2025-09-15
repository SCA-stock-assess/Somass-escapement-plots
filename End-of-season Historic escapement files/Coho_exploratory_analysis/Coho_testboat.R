# Code by Mikayla Hamilton - September 2025

#Load the libraries
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(ggridges)
library(dplyr)
library(lubridate)
# library(ggplot2) #not needed
library(geomtextpath)

# Enter the current analysis year
curr_year <- 2025



############### READ IN THE CREST FILE ##################

#This comes from ??

test_boat1 <- read_xlsx(
  "End-of-season Historic escapement files/Coho_exploratory_analysis/PFMA23_TF_DetailedCatch.xlsx",
  sheet = "Export Worksheet",
  na = ""
)

#coho return info

############### READ IN THE COHO AND CHINOOK ESCAPEMENT FILES ##################
#Note: this is the same code that Nick used in the other escapement code

# Load historical escapement data from August onward
stamp_coho <- read_xlsx(
  "input/Stampfalls.xlsx",
  sheet = "STAMP Escapement Data",
  skip = 28,
  na = ""
) |> 
  select(1:14) |> 
  pivot_longer(
    cols = CO:UNK,
    names_to = "species",
    values_to = "count"
  ) |> 
  rename_with(tolower) |> 
  mutate(
    date = as.Date(date),
    count = if_else(year < max(year) & is.na(count), 0, count)
  ) |> 
  group_by(year, species) |> 
  arrange(date, .by_group = TRUE) |> 
  # Get cumulative counts for each year and cumulative proportions
  mutate(
    cum_count = cumsum(count),
    ann_ttl = sum(count, na.rm = TRUE),
    cum_prop = cum_count/ann_ttl,
    julian = date |> format("%j") |> as.numeric()
  ) |> 
  ungroup()

unique(stamp_coho$species)

#################### CLEAN THE DATA #####################

#Step 1) Decide which columns to keep in all of these files:
test_boat <- test_boat1 %>%
  select(
    DATE, SET, AREA, COHO
  ) %>%
  
#Step 2) Re-name the columns to something that makes sense (more descriptive)
  rename(
    date = DATE,
    set = SET,
    area = AREA,
    coho = COHO
  )    %>%
mutate(
    date = dmy(date),
    year  = year(date),
    month = month(date),
    day   = day(date),
    week  = week(date)
  ) %>%

#Step 3) Come up with a column that calculates catch:
summarise(
    .by = c(year, area, week),
    across(coho, sum, na.rm = TRUE),
    boat_trips = n()
  ) |>
  mutate(area = case_when(
    str_detect(area, "Ten Mile") ~ "Ten Mile Point",
    str_detect(area, "Coleman")  ~ "Coleman Creek",
    str_detect(area, "Dunsmuir")  ~ "Dunsmuir Point",
    str_detect(area, "Pocahontas") ~ "Pocahontas Point",
    str_detect(area, "Coyote") ~ "Coyote Bluff",
    str_detect(area, "Hissin") ~ "Hissin Point",
    str_detect(area, "Hocking") ~ "Hocking Point",
    str_detect(area, "China") ~ "China Creek",
    str_detect(area, "Bilton") ~ "Bilton Point",
    str_detect(area, "Limestone") ~ "Limestone Island",
    str_detect(area, "Stamp") ~ "Stamp Narrows",
    str_detect(area, "Underwood") ~ "Underwood Cove",
    TRUE ~ area   # keep others unchanged
  )) 
unique(test_boat$area)

test2025 <- test_boat |> filter(year == 2025, week > 32)
area2025 <- unique(test2025$area)

test2025areas <- test2025


test_boat <- test_boat |>
  filter(area %in% area2025, week > 32, week < 36)


#to see if there are some areas that have spotty coverage
year_area_week <- test_boat |>
  distinct(year, area, week) |>
  arrange(year, area, week)

# Stopped here need to get coho return info
#|> 
  # Add column with adult return data
  left_join(select(.data = bs_cn, year, Somass_term_adult_return)) |> 
  rename("return" = "Somass_term_adult_return")




#################### CREATE A CPUE DATABASE #####################

# #Step 0) Check for duplicates in interview_number
# duplicate_check <- test_boat %>%
#   group_by(interview_number) %>%
#   summarise(
#     count = n(),
#     unique_sub_areas = n_distinct(sub_area)
#   ) %>%
#   filter(count > 1)
# 
# duplicate_check
# #this tells me that there are interviews that are separated into sub-areas



#Step 1) Sum by date and sub-area:
CPUE_by_subarea <- test_boat %>%
  group_by(date, sub_area) %>%
  summarise(
    number_of_interviews = n_distinct(interview_number), #count the number of interviews
    total_anglers = sum(coalesce(number_of_anglers, 0)), #sum the total number of anglers
    total_hours_fished = sum(coalesce(hours_fished, 0)), #sum the total number of hours fished
    total_catch_kr = sum(coalesce(Catch_KR, 0)), #sum the total catch (kept and released) fish
    total_catch_k = sum(coalesce(Catch_K, 0)), #sum the total catch (kept)
    
    #Calculate CPUE:
    CPUE_KR = total_catch_kr / number_of_interviews,#from kept and released fish
    CPUE_K = total_catch_k / number_of_interviews, #from kept fish only
    
    .groups = "drop" #drop all the other groups
  )



#Step 2) Sum by date only:
CPUE_total <- test_boat %>%
  group_by(date) %>%
  summarise(
    number_of_interviews = n_distinct(interview_number), #count the number of interviews
    total_anglers = sum(coalesce(number_of_anglers, 0)), #sum the total number of anglers
    total_hours_fished = sum(coalesce(hours_fished, 0)), #sum the total number of hours fished
    total_catch_kr = sum(coalesce(Catch_KR, 0)), #sum the total catch (kept and released) fish
    total_catch_k = sum(coalesce(Catch_K, 0)), #sum the total catch (kept)
    
    #Calculate CPUE:
    CPUE_KR = total_catch_kr / number_of_interviews,#from kept and released fish
    CPUE_K = total_catch_k / number_of_interviews, #from kept fish only
    
    .groups = "drop" #drop all the other groups
  )


#Step 3) Now calculate it for the entire month we have data for:
CPUE_by_subarea_monthly <- test_boat %>%
  group_by(year,month, sub_area) %>%
  summarise(
    number_of_interviews = n_distinct(interview_number), #count the number of interviews
    total_anglers = sum(coalesce(number_of_anglers, 0)), #sum the total number of anglers
    total_hours_fished = sum(coalesce(hours_fished, 0)), #sum the total number of hours fished
    total_catch_kr = sum(coalesce(Catch_KR, 0)), #sum the total catch (kept and released) fish
    total_catch_k = sum(coalesce(Catch_K, 0)), #sum the total catch (kept)
    
    #Calculate CPUE:
    CPUE_KR = total_catch_kr / number_of_interviews,#from kept and released fish
    CPUE_K = total_catch_k / number_of_interviews, #from kept fish only
    
    .groups = "drop" #drop all the other groups
  )

CPUE_total_monthly <- test_boat %>%
  group_by(year,month) %>%
  summarise(
    number_of_interviews = n_distinct(interview_number), #count the number of interviews
    total_anglers = sum(coalesce(number_of_anglers, 0)), #sum the total number of anglers
    total_hours_fished = sum(coalesce(hours_fished, 0)), #sum the total number of hours fished
    total_catch_kr = sum(coalesce(Catch_KR, 0)), #sum the total catch (kept and released) fish
    total_catch_k = sum(coalesce(Catch_K, 0)), #sum the total catch (kept)
    
    #Calculate CPUE:
    CPUE_KR = total_catch_kr / number_of_interviews,#from kept and released fish
    CPUE_K = total_catch_k / number_of_interviews, #from kept fish only
    
    .groups = "drop" #drop all the other groups
  )

############### READ IN THE COHO AND CHINOOK ESCAPEMENT FILES ##################
#Note: this is the same code that Nick used in the other escapement code

# Load historical escapement data from August onward
stamp_cn <- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/TERMINAL_AREAS/TERMRBT/Stampfalls.xlsx",
  sheet = "STAMP Escapement Data",
  skip = 28,
  na = ""
) |> 
  select(1:14) |> 
  pivot_longer(
    cols = CO:UNK,
    names_to = "species",
    values_to = "count"
  ) |> 
  rename_with(tolower) |> 
  mutate(
    date = as.Date(date),
    count = if_else(year < max(year) & is.na(count), 0, count)
  ) |> 
  group_by(year, species) |> 
  arrange(date, .by_group = TRUE) |> 
  # Get cumulative counts for each year and cumulative proportions
  mutate(
    cum_count = cumsum(count),
    ann_ttl = sum(count, na.rm = TRUE),
    cum_prop = cum_count/ann_ttl,
    julian = date |> format("%j") |> as.numeric()
  ) |> 
  ungroup()|> 
  filter(species =="CO")



########################## CLEAN UP THE ESCAPEMENT #############################
#we want to get the total escapement of Coho only for August, September and October, as well as
#the final escapement for the year:

coho_escapement_summary <- stamp_cn %>%
  filter(species == "CO") %>%
  mutate(month = month(date)) %>%
  group_by(year) %>%
  summarise(
    coho_august = sum(count[month == 8], na.rm = TRUE),
    coho_sept   = sum(count[month == 9], na.rm = TRUE),
    coho_oct    = sum(count[month == 10], na.rm = TRUE),
    final_escapement = max(ann_ttl, na.rm = TRUE),
    .groups = "drop"
  )




########################## LINEAR REGRESSION ANALYSIS ###########################
#Come up with a linear regression to determine whether there is a relationship between CPUE and Escapement

#Here I create a function that plots the linear model based off of which CPUE we use, which month we use,
#and which sub-area we use (as an option)

#The function is named CPUE_LM (Catch per unit effort Linear Model)


########################## CREATE A FUNCTION TO LOOK AT CPUE VS ESCAPEMENT ###########################
test_LM <- function(
    test_data = test_boat,                 # can be: CPUE_by_subarea_monthly, CPUE_total_monthly
    escapement_data,           # coho_escapement_summary data frame
    weeks = c(33,34,35),   # enter it as a character - ex. "08" or "09"
    area = NULL            # can be: 23A or 23B
) {
 
  
  ### Part 1: Prepare Data ###
  
  # Step 1: Filter CPUE data by month, year range, and optional area:
  filtered_data <- test_data %>%
    filter(week %in% weeks,
           as.numeric(year) >= 2000,
           as.numeric(year) < curr_year) %>%
    mutate(year = as.numeric(year))
  
  # Step 1a: Filter to sub-area if selected:
  if (!is.null(area) && "area" %in% colnames(filtered_data)) {
    filtered_data <- filtered_data %>%
      filter(area == area)
  }
  
  # Step 1b: Select only needed columns AFTER filtering:
  #filtered_data <- filtered_data %>%
  #  select(year, coho, boat_trips, )
  
  # Step 2) Join with escapement data by year:
  regression_data <- filtered_data %>%
    inner_join(escapement_data, by = "year") %>%
    select(year, all_of(cpue_metric), final_escapement)
  
  }
  
  
  
  
  ### PART 2: Run model ###
  
  #Step 1) Create the model:
  formula <- as.formula(paste("final_escapement ~", cpue_metric))
  # formula <- as.formula(paste(cpue_metric, "~", "final_escapement"))
  
  #Step 2) Get the summary statistics:
  model <- lm(formula, data = regression_data)
  r_squared <- round(summary(model)$r.squared, 3)
  
  
  
  
  #### PART 3: Plot the Model ###
  
  #Step 0) Create a dynamic title for the plot (so if there is sub-area it uses it):

  base_title <- paste("CPUE vs Final Coho Escapement for Month", Month)
  if (!is.null(area)) {
    base_title <- paste(base_title, "and sub-area", area)
  }
  
  
  
  
  
  #Step 1) Create a simple plot:
  plot_simple <- 
    ggplot(regression_data, aes_string(x = cpue_metric, y = "final_escapement")) +
    geom_point(size = 3, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    geom_text(aes(label = year), vjust = -1, size = 4, color = "steelblue") +
    annotate("text",
             x = max(regression_data[[cpue_metric]]) * 0.6,
             y = max(regression_data$final_escapement) * 0.9,
             label = paste("R² =", r_squared),
             size = 5, color = "black") +
    labs(
      title = base_title,
      x = cpue_metric,
      y = "Final Coho Escapement"
    ) +
    theme_minimal()
  
  
  
  #Step 2) Create a more colourful plot: years are different colours in this one:
  plot_colourful <- 
    ggplot(regression_data, aes_string(x = cpue_metric, y = "final_escapement", color = "factor(year)")) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    annotate("text",
             x = max(regression_data[[cpue_metric]]) * 0.6,
             y = max(regression_data$final_escapement) * 0.9,
             label = paste("R² =", r_squared),
             size = 5, color = "black") +
    labs(
      title = base_title,
      x = cpue_metric,
      y = "Final Coho Escapement",
      color = "Year"
    ) +
    theme_minimal()
  
  
  
  
  # Return list of model and plots
  return(list(
    # model = model,
    # r_squared = r_squared,
    plot_simple = plot_simple
    # plot_colourful = plot_colourful
  ))
}






######################### CALL THE FUNCTION ###########################













