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

#This comes from doing the CREST-pull. A photo is in the repo, but in-case it gets deleted, here are the steps:
#1) Click "view Reports" on the home screen
#2) Select years: 2000-current year
#3) Months: 8,9,10
#4) Programs = Barkley Sound
#5) Projects: Barkley Sound
#6) Interview Summary: "+ Interview Summary"
#and then have it print the output to Excel (click "Generate Report"). The format should be the same (I didn't do anything to change column names or anything fancy)


crest_pull <- read_xlsx(
  "Interview_summary_CREST_recdata_2000-2025.xlsx",
  sheet = "Interview_Summary",
  na = ""
)

#################### CLEAN THE DATA #####################

#Step 1) Decide which columns to keep in all of these files:
crest_pull <- crest_pull %>%
  select(
    YEAR, MONTH, DATE, INTNO, ANGLERS, STATSUB, HOURS_FISHED, 
    REGION, AREA, CO_ALL_K, CO_UNK_K, CO_AD_K, CO_NM_K, 
    CO_RL, CO_RSL, B_CO
  ) %>%
  
  #Step 2) Re-name the columns to something that makes sense (more descriptive)
  rename(
    year = YEAR,
    month = MONTH,
    date = DATE,
    interview_number = INTNO,
    number_of_anglers = ANGLERS,
    sub_area = STATSUB,
    hours_fished = HOURS_FISHED,
    region = REGION,
    area = AREA,
    coho_all_kept = CO_ALL_K,
    coho_unknown_kept = CO_UNK_K,
    coho_marked_kept = CO_AD_K,
    coho_unmarked_kept = CO_NM_K,
    coho_released_legalsize = CO_RL,
    coho_released_sublegalsize = CO_RSL,
    B_coho = B_CO
  )

#Step 3) Come up with a column that calculates catch:

#Catch_C&R: Here I am considering "Catch" what was caught AND what was released
crest_pull <- crest_pull %>%
  mutate(
    Catch_KR = coalesce(coho_all_kept, 0) +  #note this ",0" means if it is NA make it a zero instead
      coalesce(coho_released_legalsize, 0) + 
      coalesce(coho_released_sublegalsize, 0)
  )

#Catch_C: Here I am considering "Catch" what was caught but NOT what was released
crest_pull <- crest_pull %>%
  mutate(
    Catch_K = coalesce(coho_all_kept, 0) #note this ",0" means if it is NA make it a zero instead
  )


#Step 4) Filter to only have Alberni Inlet (we aren't interested in the other areas):
crest_pull <- crest_pull %>%
  filter(
    region == "Area 23 (Alberni Canal)"
  )


#################### CREATE A CPUE DATABASE #####################

# #Step 0) Check for duplicates in interview_number
# duplicate_check <- crest_pull %>%
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
CPUE_by_subarea <- crest_pull %>%
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
CPUE_total <- crest_pull %>%
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
CPUE_by_subarea_monthly <- crest_pull %>%
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

CPUE_total_monthly <- crest_pull %>%
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
  ungroup()



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
CPUE_LM <- function(
    CPUE_data,                 # can be: CPUE_by_subarea_monthly, CPUE_total_monthly
    escapement_data,           # coho_escapement_summary data frame
    Month = NULL,              # enter it as a character - ex. "08" or "09"
    cpue_metric = NULL,        # either "CPUE_KR" or "CPUE_K"
    remove_years = NULL,       # vector of years to remove, e.g. c(2001, 2013)
    Sub_area = NULL            # can be: 23A or 23B
) {
 
  
  ### Part 1: Prepare Data ###
  
  # Step 1: Filter CPUE data by month, year range, and optional sub_area:
  filtered_cpue <- CPUE_data %>%
    filter(month %in% Month,
           as.numeric(year) >= 2015,
           as.numeric(year) < curr_year) %>%
    mutate(year = as.numeric(year))
  
  # Step 1a: Filter to sub-area if selected:
  if (!is.null(Sub_area) && "sub_area" %in% colnames(filtered_cpue)) {
    filtered_cpue <- filtered_cpue %>%
      filter(sub_area == Sub_area)
  }
  
  # Step 1b: Select only needed columns AFTER filtering:
  filtered_cpue <- filtered_cpue %>%
    select(year, all_of(cpue_metric))
  
  # Step 2) Join with escapement data by year:
  regression_data <- filtered_cpue %>%
    inner_join(escapement_data, by = "year") %>%
    select(year, all_of(cpue_metric), final_escapement)
  
  
  # Step 3) Remove outlier years (if we want to):
  if (!is.null(remove_years)) {
    regression_data <- regression_data %>%
      filter(!year %in% remove_years)
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
  if (!is.null(Sub_area)) {
    base_title <- paste(base_title, "and sub-area", Sub_area)
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


### AUGUST ###

#For August only separated into sub-area (23A), using kept AND released fish: #0.089
CPUE_LM(
  CPUE_data = CPUE_by_subarea_monthly,
  escapement_data = coho_escapement_summary,
  Month = "08",
  cpue_metric = "CPUE_KR",
  Sub_area = "23A",
  remove_years = c(2000:2014)
  # remove_years = c(2000, 2013)
)

#For August only separated into sub-area (23B), using kept AND released fish: #0.545
CPUE_LM(
  CPUE_data = CPUE_by_subarea_monthly,
  escapement_data = coho_escapement_summary,
  Month = "08",
  cpue_metric = "CPUE_KR",
  Sub_area = "23B",
  remove_years = c(2000:2014) #regulations similar to 2025 in 2021-2024
)

#For August only, using kept AND released fish:#0.189
CPUE_LM(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "08",
  cpue_metric = "CPUE_KR",
  remove_years = c(2000:2014)
)


# # #For August only, using kept fish only: #0.131
# CPUE_LM(
#   CPUE_data = CPUE_total_monthly,
#   escapement_data = coho_escapement_summary,
#   Month = "08",
#   cpue_metric = "CPUE_K"
#   # remove_years = c(2001, 2013)
# )





### SEPTEMBER ###

#For September only separated into sub-area (23A), using kept AND released fish:#0.281 OR #0.314 removing the specified years
CPUE_LM(
  CPUE_data = CPUE_by_subarea_monthly,
  escapement_data = coho_escapement_summary,
  Month = "09",
  cpue_metric = "CPUE_KR",
  Sub_area = "23A",
  remove_years = c(2000:2014)
)

#For September only separated into sub-area (23B), using kept AND released fish:#0.315
CPUE_LM(
  CPUE_data = CPUE_by_subarea_monthly,
  escapement_data = coho_escapement_summary,
  Month = "09",
  cpue_metric = "CPUE_KR",
  Sub_area = "23B",
  remove_years = c(2000:2014) #regulations similar to 2025 in 2021-2024
  )


#For September only, using kept AND released fish: #0.324 OR #0.406 removing the specified years
CPUE_LM( 
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "09",
  cpue_metric = "CPUE_KR",
  remove_years = c(2000:2014)
)

# # #For September only, using kept fish only: #0.296
# CPUE_LM(
#   CPUE_data = CPUE_total_monthly,
#   escapement_data = coho_escapement_summary,
#   Month = "09",
#   cpue_metric = "CPUE_K"
#   # remove_years = c(2001, 2013)
# )





### AUGUST & SEPTEMBER ###

#For August & September  separated into sub-area (23A), using kept AND released fish: #0.16 OR #0.183 removing the specified years
CPUE_LM(
  CPUE_data = CPUE_by_subarea_monthly,
  escapement_data = coho_escapement_summary,
  Month = c("09","08"),
  cpue_metric = "CPUE_KR",
  Sub_area = "23A",
  remove_years = c(2000:2014)
)

#For August & September  separated into sub-area (23B), using kept AND released fish: #0.279
CPUE_LM(
  CPUE_data = CPUE_by_subarea_monthly,
  escapement_data = coho_escapement_summary,
  Month = c("09","08"),
  cpue_metric = "CPUE_KR",
  Sub_area = "23B"
)


#For  August & September, using kept AND released fish: #0.204 OR #0.25 removing the specified years
CPUE_LM(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = c("09","08"),
  cpue_metric = "CPUE_KR",
  remove_years = c(2000:2020) #regulations similar to 2025 in 2021-2024
  # remove_years = c(2001, 2013)
)

# # #For  August & September, using kept fish only: #0.176
# CPUE_LM(
#   CPUE_data = CPUE_total_monthly,
#   escapement_data = coho_escapement_summary,
#   Month = c("09","08"),
#   cpue_metric = "CPUE_K"
#   # remove_years = c(2001, 2013)
# )
# 













######################### READ IN QUARTILE DATA ###########################

#Read in the Quartile data:
  RCH_Quartiles <- read_xlsx(
    "RbtObsQuart.xlsx",
    sheet = "Sheet1",
    na = ""
  ) 

#set Quartiles based off of year:

Quartile_1 <- RCH_Quartiles %>%
  filter(ObsQuart == 1) %>%
  pull('Return Year')

Quartile_2 <- RCH_Quartiles %>%
  filter(ObsQuart == 2) %>%
  pull('Return Year')

Quartile_3 <- RCH_Quartiles %>%
  filter(ObsQuart == 3) %>%
  pull('Return Year')

Quartile_4 <- RCH_Quartiles %>%
  filter(ObsQuart == 4) %>%
  pull('Return Year')



################### CREATE FUNCTION TO LOOK AT QUARTILES #######################
CPUE_LM_Quartiles <- function(
    CPUE_data,                 # can be: CPUE_by_subarea_monthly, CPUE_total_monthly
    escapement_data,           # coho_escapement_summary data frame
    Month = NULL,              # enter it as a character - ex. "08" or "09"
    cpue_metric = "CPUE_KR",   # either "CPUE_KR" or "CPUE_K"
    remove_years = NULL,       # vector of years to remove, e.g. c(2001, 2013)
    Sub_area = NULL,           # can be: 23A or 23B
    years_to_use = NULL        # use the quartiles specified above here
) {
  
  ### Part 1: Prepare Data ###
  
  # Step 1) Filter CPUE data by month(s), year range, optional sub_area, and years_to_use if provided:
  filtered_cpue <- CPUE_data %>%
    filter(month %in% Month,
           as.numeric(year) >= 2000,
           as.numeric(year) < curr_year)
  
  # Filter by sub-area if specified
  if (!is.null(Sub_area) && "sub_area" %in% colnames(filtered_cpue)) {
    filtered_cpue <- filtered_cpue %>%
      filter(sub_area == Sub_area)
  }
  
  filtered_cpue <- filtered_cpue %>%
    mutate(year = as.numeric(year)) %>%
    select(year, all_of(cpue_metric))
  
  # Filter by years_to_use if provided
  if (!is.null(years_to_use)) {
    filtered_cpue <- filtered_cpue %>%
      filter(year %in% years_to_use)
  }
  
  # Step 2) Join with escapement data by year:
  regression_data <- filtered_cpue %>%
    inner_join(escapement_data, by = "year") %>%
    select(year, all_of(cpue_metric), final_escapement)
  
  # Filter by years_to_use in escapement data as well to be safe
  if (!is.null(years_to_use)) {
    regression_data <- regression_data %>%
      filter(year %in% years_to_use)
  }
  
  # Step 3) Remove outlier years (if specified)
  if (!is.null(remove_years)) {
    regression_data <- regression_data %>%
      filter(!year %in% remove_years)
  }
  
  ### PART 2: Run model ###
  
  formula <- as.formula(paste("final_escapement ~", cpue_metric))
  
  # get the summary statistics:
  model <- lm(formula, data = regression_data)
  r_squared <- round(summary(model)$r.squared, 3)
  
  ### PART 3: Plot the Model ###
  
  # Create a dynamic title that will change with whatever was input:
  base_title <- paste("CPUE vs Final Coho Escapement for Month", paste(Month, collapse = ", "))
  if (!is.null(Sub_area)) {
    base_title <- paste(base_title, "and sub-area", Sub_area)
  }
  
  # Step 1) create a simple plot:
  plot_simple <- 
    ggplot(regression_data, aes_string(x = cpue_metric, y = "final_escapement")) +
    geom_point(size = 3, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    geom_text(aes(label = year), vjust = -1, size = 4, color = "steelblue") +
    annotate("text",
             x = max(regression_data[[cpue_metric]], na.rm = TRUE) * 0.6,
             y = max(regression_data$final_escapement, na.rm = TRUE) * 0.9,
             label = paste("R² =", r_squared),
             size = 5, color = "black") +
    labs(
      title = base_title,
      x = cpue_metric,
      y = "Final Coho Escapement"
    ) +
    theme_minimal()
  
  # Step 2) Create a more colourful plot: years are different colours in this one:
  plot_colourful <- 
    ggplot(regression_data, aes_string(x = cpue_metric, y = "final_escapement", color = "factor(year)")) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    annotate("text",
             x = max(regression_data[[cpue_metric]], na.rm = TRUE) * 0.6,
             y = max(regression_data$final_escapement, na.rm = TRUE) * 0.9,
             label = paste("R² =", r_squared),
             size = 5, color = "black") +
    labs(
      title = base_title,
      x = cpue_metric,
      y = "Final Coho Escapement",
      color = "Year"
    ) +
    theme_minimal()
  
  ### Return results including years analyzed ###
  return(list(
    # model = model,
    # r_squared = r_squared,
    # years_analyzed = unique(regression_data$year),
    # plot_simple = plot_simple,
    plot_colourful = plot_colourful
  ))
}



######################### CALL THE FUNCTION ###########################


### AUGUST ###

#Quartile: 1 #0.054
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "08",
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_1
  # remove_years = c(2001, 2013)
)

#Quartile: 2 #0.313
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "08",
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_2
  # remove_years = c(2001, 2013)
)


#Quartile: 3 #0.571
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "08",
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_3
  # remove_years = c(2001, 2013)
)


#Quartile: 4 #0.002
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "08",
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_4
  # remove_years = c(2001, 2013)
)





### SEPTEMBER ###


#Quartile: 1 #0.008
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "09",
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_1
  # remove_years = c(2001, 2013)
)

#Quartile: 2 #0.379
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "09",
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_2
  # remove_years = c(2001, 2013)
)


#Quartile: 3 #0.000
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "09",
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_3
  # remove_years = c(2001, 2013)
)


#Quartile: 4 #0.066
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = "09",
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_4
  # remove_years = c(2001, 2013)
)





### AUGUST & SEPTEMBER ###

 
#Quartile: 1 #0.007
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = c("09","08"),
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_1,
  remove_years = c(2000: 2014)
)

#Quartile: 2 #0.08
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = c("09","08"),
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_2,
  remove_years = c(2000: 2014)
)


#Quartile: 3 #0.009
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = c("09","08"),
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_3,
  remove_years = c(2000: 2014)
)


#Quartile: 4 #0.025
CPUE_LM_Quartiles(
  CPUE_data = CPUE_total_monthly,
  escapement_data = coho_escapement_summary,
  Month = c("09","08"),
  cpue_metric = "CPUE_KR",
  years_to_use = Quartile_4,
  #remove_years = c(2000: 2014)
)

##### Exploring relationship between recreational CPUE and marine survival quartiles #########
#suggested by Christie Morrison on 9/16/2025 -> VP
#The code below bypasses MH codes and used the updated SC Creel data avialable on 
# salmon drive: FMCR_Fishery_Monitoring_Catch_Reporting\Recreational_CM\Catch_Data


CrestCatch <- read_xlsx(
  "CohoCPUESep19VP.xlsx",
  sheet = "SC Creel",
  na = ""
) 
#filtering for subarea, years, months, plus coho catch and effort
AlberniCohoCrestData <- CrestCatch |> 
  filter(CREEL_SUB_AREA %in% c("23A","23B"),
         YEAR >= 2015,
         MONTH %in% c("August", "September"),
         SPECIES_CODE %in% c("115", "B_TRIPS")
      )

# Step 1: Sum Coho catch and released for each sub_area, year and month
cohoEst <- AlberniCohoCrestData |> 
  filter(
    SPECIES_CODE == "115",          # only Coho
    DISPOSITION != "Effort"          # exclude Effort rows
  ) %>%
  group_by(YEAR, MONTH, CREEL_SUB_AREA) %>%
  summarise(
    CohoKR = sum(ESTIMATE, na.rm = TRUE),
    .groups = "drop"
  )
# Step 2: Summarise BO_TRIPS
trips_summary <- AlberniCohoCrestData %>%
  filter(SPECIES_CODE == "B_TRIPS") %>%
  group_by(YEAR, MONTH, CREEL_SUB_AREA) %>%
  summarise(
    BoatTrips = sum(ESTIMATE, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Join them together and calculate CPUE
CatchEffortData <- cohoEst %>%
  left_join(trips_summary, by = c("YEAR", "MONTH", "CREEL_SUB_AREA")) |> 
  mutate(CPUE=CohoKR/BoatTrips)

CatchEffortSummary <- CatchEffortData %>%
  group_by(YEAR) %>%
  summarise(AugSep_CPUE = sum(CPUE, na.rm = TRUE)) %>%
  arrange(YEAR)



#step 4: Load in the quartile data
ObsQuart <- read_xlsx("RbtObsQuart.xlsx")
ObsQuart<- head(ObsQuart, -2)


ObsQuartRecentOnly <- ObsQuart %>%
  filter(`Return Year` %in% 2015:2024) %>%
  rename(YEAR = `Return Year`, ObservedQuart = ObsQuart) %>%
  dplyr::select(YEAR, ObservedQuart)

#Join the two datasets of MS quartiles and Rec CPUE
CombinedCPUEQuartile <- CatchEffortSummary %>%
  left_join(ObsQuartRecentOnly, by = "YEAR")

CombinedCPUEQuartile<- CombinedCPUEQuartile |> 
filter(YEAR != "2025")

#Relationship between CPUE and Quartiles
plot(CombinedCPUEQuartile$ObservedQuart~CombinedCPUEQuartile$AugSep_CPUE)

#step 5: Load historical escapement data from August onward
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
  ungroup()



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
#filter out the older years
coho_escapement_summary <- coho_escapement_summary |> 
  filter(`year` %in% c(2015:2024))
#bring in the historical mark rate calculated by MH
AvgMarkRate<- read_csv("MonthlyCohoMarkRateHistorical.csv") |> 
filter(Month %in% c(8, 9, 10)) %>%
  select(MonthLabel, prop_marked, prop_unmarked) #Mark rate for aug, sep and Oct


#Pivot escapement to long format for Aug/Sep/Oct
escapement_long <- coho_escapement_summary %>%
  pivot_longer(
    cols = starts_with("coho_"),
    names_to = "MonthLabel",
    values_to = "count"
  ) %>%
  mutate(
    MonthLabel = sub("coho_", "", MonthLabel),   # remove prefix
    MonthLabel = tools::toTitleCase(MonthLabel)  # "August", "Sept", "Oct"
  )

# Join proportions to escapement
escapement_long <- escapement_long %>%
  mutate(
    prop_marked = case_when(
      MonthLabel == "August" ~ 0.2010347,
      MonthLabel == "Sept" ~ 0.4225110,
      MonthLabel == "Oct" ~ 0.2437434,
      TRUE ~ 0  # fallback, should never be used if data is clean
    ),
    prop_unmarked = case_when(
      MonthLabel == "August" ~ 0.7989653,
      MonthLabel == "Sept" ~ 0.5774890,
      MonthLabel == "Oct" ~ 0.7562566,
      TRUE ~ 0
    )
  ) |>  #calculate marked and unamarked coho counts based on expected proportions
  mutate(MarkedCoho=count * prop_marked, UnmarkedCoho= count * prop_unmarked)
#Bring in the observed quartiles and attach it escapement dataframe
# Step 1: Rename 'Return Year' to 'year' for joining
ObsQuartRecent <- ObsQuartRecent %>%
  rename(year = `Return Year`) %>%
  select(year, ObsQuart)

# Step 2: Join with escapement_long
escapement_long <- escapement_long %>%
  left_join(ObsQuartRecent, by = "year")

###Now test the relationship: how unmarked Coho escapement varies across observation quartiles for each month
# changing the quartiles to Ordinal
escapement_long <- escapement_long %>%
  mutate(ObsQuart = factor(ObsQuart, ordered = TRUE))

#faceted Strip plot
FacetStripPlot<- ggplot(escapement_long, aes(x = UnmarkedCoho, y = ObsQuart)) +
  geom_point(color = "steelblue") +
  facet_wrap(~ MonthLabel, nrow = 1) +
  labs(
    title = "Marine survival Quartile vs. Unmarked Coho Escapement by Month 2015-2024",
    x = "Unmarked Coho Escapement",
    y = ""
  ) +
  theme_minimal()
#### Save to the network folder
ggsave(
  plot = FacetStripPlot, 
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Exploratory/Coho/",
    "CohoEscObsQuart",
    format(Sys.Date(), "%Y-%m-%d"), "_",  # Add current date here
    ".png"
  ),
  height = 4.5,
  width = 8,
  units = "in"
)

#only sept
SepOnlyPlot<- escapement_long %>%
  filter(MonthLabel == "Sept") %>%
  mutate(ObsQuart = factor(ObsQuart, ordered = TRUE)) %>%
  ggplot(aes(x = UnmarkedCoho, y = ObsQuart)) +
  geom_jitter(height = 0.2, alpha = 0.7, color = "steelblue", size = 3) +
  labs(
    title = "Observation Quartile vs. Unmarked Coho Escapement (September) 2015-2024",
    x = "Unmarked Coho Escapement",
    y = ""
  ) +
  theme_minimal()

ggsave(
  plot = SepOnlyPlot, 
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Exploratory/Coho/",
    "CohoEscObsQuartSepOnly",
    format(Sys.Date(), "%Y-%m-%d"), "_",  # Add current date here
    ".png"
  ),
  height = 4.5,
  width = 8,
  units = "in"
)


#Multiniminal logistic regression to test the significance
library(nnet)

# Step 1: Filter for September
sept_data <- escapement_long %>%
  filter(MonthLabel == "Sept") %>%
  mutate(
    UnmarkedCoho_log = log1p(UnmarkedCoho),        # Step 2: Transform predictor
    ObsQuart = factor(ObsQuart)                    # Treat quartile as nominal
  )

# Step 3: Fit multinomial logistic regression
model_multinom <- multinom(ObsQuart ~ UnmarkedCoho_log, data = sept_data)

# Step 4: Compute p-values: This code converts the z-score into a p-value using the normal distribution.
#The pnorm() function gives the probability of observing a value that extreme by chance.

z <- summary(model_multinom)$coefficients / summary(model_multinom)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z))) #Multiplying by 2 gives a two-tailed test (testing for both positive and negative effects).
round(p_values, 4)

# Step 5: Predict quartile for a new escapement value (e.g., 8000)
new_value <- data.frame(UnmarkedCoho_log = log1p(8000))
predicted_quartile <- predict(model_multinom, newdata = new_value)
predicted_quartile
#If we want predicted probabilities instead of just the most likely quartile:
predict(model_multinom, newdata = new_value, type = "probs")
###### What if we use raw escapement instead of transorming

model_multinom_raw <- multinom(ObsQuart ~ UnmarkedCoho, data = sept_data)

#Compute p-values manually
z_raw <- summary(model_multinom_raw)$coefficients / summary(model_multinom_raw)$standard.errors
p_values_raw <- 2 * (1 - pnorm(abs(z_raw)))
round(p_values_raw, 4)


summary(model_multinom_raw)$coefficients

ggplot(sept_data, aes(x = UnmarkedCoho, y = ObsQuart)) +
  geom_point( size = 3, color = "darkred") +
  labs(title = "Visual Check for Separation in September Data")

predict(model_multinom_raw, newdata = data.frame(UnmarkedCoho = 6000), type = "probs")
