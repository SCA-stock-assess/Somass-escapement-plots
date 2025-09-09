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
    Catch_CR = coalesce(coho_all_kept, 0) +  #note this ",0" means if it is NA make it a zero instead
      coalesce(coho_released_legalsize, 0) + 
      coalesce(coho_released_sublegalsize, 0)
  )

#Catch_C: ere I am considering "Catch" what was caught but NOT what was released
crest_pull <- crest_pull %>%
  mutate(
    Catch_C = coalesce(coho_all_kept, 0) #note this ",0" means if it is NA make it a zero instead
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




############### READ IN THE CURRENT YEAR'S MARKED VS UNMARKED ESCAPEMENT DATA ##################
#We get this data from Graham Murrel with Hupcasath - he broke it down into marked vs unmarked during
#the in-season chinook run this year for us to be able to analyze, but that doesn't typically get done
#from what I understand


current_data <- read_xlsx(
  "Daily Totals by Age 2025.xlsx",
  sheet = "Stamp CN&CO",
  na = ""
) %>%
  #Only select the columns we are interested in:
  select(Date, "Co  Mark", "Co  NoMark") %>%
  mutate(
    year = curr_year,
    # normalize to fixed year:
    MonthDay = as.Date(format(Date, "2000-%m-%d")),  
    Co_Mark = `Co  Mark`,
    Co_NoMark = `Co  NoMark`
  ) %>%
  # ensure dates are in order for cumulative sums
  arrange(Date) %>%
  #we want this data as a cumulative sum:
  mutate(
    Co_Mark_Cumulative = cumsum(replace_na(Co_Mark, 0)),
    Co_NoMark_Cumulative = cumsum(replace_na(Co_NoMark, 0))
  )




