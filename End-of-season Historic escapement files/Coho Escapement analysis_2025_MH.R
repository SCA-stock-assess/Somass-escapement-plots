# Packages ----------------------------------------------------------------

pkgs <- c("tidyverse", "readxl", "ggridges", "geomtextpath")
#install.packages(pkgs)
  
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(ggridges)
library(dplyr)
library(lubridate)
# library(ggplot2) #not needed
library(geomtextpath)

# Enter the current analysis year
curr_year <- 2025



############### READ IN THE HISTORIC FILES ##################

#2015:
data_2015_stamp <- read_xlsx(
  "2015 Inseason Somass Counts.xlsx",
  sheet = "Stamp Daily",
  na = ""
) %>%
  mutate(
    year = 2015, #make a column that says the year
    `Review Date` = as.Date(`Review Date`) #make sure that the Review Date is "as.date"
  )

data_2015_sproat <- read_xlsx(
  "2015 Inseason Somass Counts.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2015, 
    `Review Date` = as.Date(`Review Date`) 
  )

#2016:

data_2016_stamp <- read_xlsx(
  "2016 Inseason Somass Counts.xlsx",
  sheet = "Stamp Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2016, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30") #this accounts for the fact that the date is an excel serial date number
  )

data_2016_sproat <- read_xlsx(
  "2016 Inseason Somass Counts.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2016, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

#2017:

data_2017_stamp <- read_xlsx(
  "2017 Inseason Somass Counts.xlsx",
  sheet = "Stamp Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2017, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

data_2017_sproat <- read_xlsx(
  "2017 Inseason Somass Counts.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2017, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

#2018:

data_2018_stamp <- read_xlsx(
  "2018 Inseason Somass Counts Final.xlsx",
  sheet = "Stamp Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2018, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

data_2018_sproat <- read_xlsx(
  "2018 Inseason Somass Counts Final.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2018, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

#2019:
data_2019_stamp <- read_xlsx(
  "2019 Inseason Somass Counts Final update.xlsx",
  sheet = "Stamp Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2019, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

data_2019_sproat <- read_xlsx(
  "2019 Inseason Somass Counts Final update.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2019, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

#2020:

data_2020_stamp <- read_xlsx(
  "2020 Inseason Somass Counts Final.xlsx",
  sheet = "Stamp Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2020, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30") 
  )

data_2020_sproat <- read_xlsx(
  "2020 Inseason Somass Counts Final.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2020, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )


#2021:
data_2021_stamp <- read_xlsx(
  "2021 Somass Counts Final.xlsx",
  sheet = "Stamp Daily Expanded",
  na = ""
)  %>%
  mutate(
    year = 2021, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

data_2021_sproat <- read_xlsx(
  "2021 Somass Counts Final.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2021, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

#2022:
data_2022_stamp <- read_xlsx(
  "2022 Somass Counts Final.xlsx",
  sheet = "Stamp Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2022, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

data_2022_sproat <- read_xlsx(
  "2022 Somass Counts Final.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2022, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

#2023:
data_2023_stamp <- read_xlsx(
  "2023 Somass Counts Final.xlsx",
  sheet = "Stamp Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2023, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30") 
  )

data_2023_sproat <- read_xlsx(
  "2023 Somass Counts Final.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2023, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

#2024:
data_2024_stamp <- read_xlsx(
  "2024 Somass Counts Final.xlsx",
  sheet = "Stamp Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2024, 
    `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )

data_2024_sproat <- read_xlsx(
  "2024 Somass Counts Final.xlsx",
  sheet = "Sproat Daily Expanded",
  na = ""
) %>%
  mutate(
    year = 2024, 
      `Review Date` = as.Date(as.numeric(`Review Date`), origin = "1899-12-30")
  )



#################### FILTER THE DATA #####################

#Step 1: Decide which columns to keep in all of these files:
keep_cols <- c(
  "Site", "Review Date", "Co", "CoJk","Co  NoMark", "Co  Mark", "CoJk  NoMark", "CoJk  Mark", "year"
)

#Step 2: List all of the data:
all_data <- list(
  data_2015_stamp = data_2015_stamp,
  data_2015_sproat = data_2015_sproat,
  data_2016_stamp = data_2016_stamp,
  data_2016_sproat = data_2016_sproat,
  data_2017_stamp = data_2017_stamp,
  data_2017_sproat = data_2017_sproat,
  data_2018_stamp = data_2018_stamp,
  data_2018_sproat = data_2018_sproat,
  data_2019_stamp = data_2019_stamp,
  data_2019_sproat = data_2019_sproat,
  data_2020_stamp = data_2020_stamp,
  data_2020_sproat = data_2020_sproat,
  data_2021_stamp = data_2021_stamp,
  data_2021_sproat = data_2021_sproat,
  data_2022_stamp = data_2022_stamp,
  data_2022_sproat = data_2022_sproat,
  data_2023_stamp = data_2023_stamp,
  data_2023_sproat = data_2023_sproat,
  data_2024_stamp = data_2024_stamp,
  data_2024_sproat = data_2024_sproat
)

#Step 3: Apply the filter to all the data:
filtered_data <- lapply(all_data, function(df) {
  df[, intersect(keep_cols, names(df)), drop = FALSE]
})


#Step 4: Combine everything into one database:
historic_data_2015_2024 <- bind_rows(filtered_data, .id = "source")


#Step 5: remove any rows where the site is not stamp or sproat:
historic_data_2015_2024 <- historic_data_2015_2024 %>%
  filter(str_detect(Site, regex("stamp|sproat", ignore_case = TRUE))) #make sure it isn't case sensitive




#remove everything but historic_data_2015_2024:
rm(list = setdiff(ls(), c("historic_data_2015_2024", "curr_year")))


############### Calculate proportion of Marked vs Unmarked by Month #############
#Marked fish are considered "Hatchery" and unmarked fish are considered "wild"


historic_monthly_props <- historic_data_2015_2024 %>%
  #Extract the month number (ex. January = 01, etc. and call it the month's name instead of the numbers):
  mutate(
    Month = month(as.Date(`Review Date`)),
    MonthLabel = format(as.Date(paste0("2000-", Month, "-01")), "%b")
  ) %>%
  
  #Group the data by month:
  group_by(Month, MonthLabel) %>%
  
  #Sum the total marked and unmarked coho:
  summarise(
    total_marked = sum(`Co  Mark`, na.rm = TRUE),
    total_unmarked = sum(`Co  NoMark`, na.rm = TRUE)
  ) %>%
  
  #Calculate the total coho counted as marked and unmarked, and calculate the proportion of marked vs unmarked from it:
  mutate(
    total_marked_unmarked = total_marked + total_unmarked,
    prop_marked = total_marked / total_marked_unmarked,
    prop_unmarked = total_unmarked / total_marked_unmarked
  ) %>%
  
  #Keep only the month name and the proportion to be the output
  select(MonthLabel, prop_marked, prop_unmarked) %>%
  #Arrange the months chronologically:
  arrange(Month) %>%
  ungroup()


#Print the proportions monthly:
historic_monthly_props





############### Read in Coho & Chinook escapement data to date #############

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


########################## Apply Proportion of unmarked to current year Coho ################################
#to project the amount of wild coho returning to date, we apply the proportion by month to the current year's return:

#Step 1: Get current year monthly total Coho (assuming raw daily counts exist or cumulative counts converted to daily)
current_year_monthly <- stamp_cn %>%
  filter(year == curr_year, species == "CO") %>%
  mutate(
    Date = as.Date(julian, origin = paste0(curr_year - 1, "-12-31")),
    Month = month(Date),
    MonthDay = as.Date(format(Date, "2000-%m-%d")) # Dummy year for x-axis
  ) %>%
  group_by(Month, MonthDay) %>%
  summarise(
    monthly_count = sum(cum_count, na.rm = TRUE),  # Sum daily counts for each month-day combo
    .groups = "drop"  # Ungroup after summarise
  )

#Step 2: Join with historic monthly proportions
proj_marked_unmarked <- current_year_monthly %>%
  left_join(historic_monthly_props, by = "Month") %>%
  mutate(
    est_marked = monthly_count * prop_marked,
    est_unmarked = monthly_count * prop_unmarked
  )

#Step 3: Prepare for plotting: convert to long format for marked/unmarked lines
proj_long <- proj_marked_unmarked %>%
  select(MonthDay, est_marked, est_unmarked) %>%
  pivot_longer(cols = c(est_marked, est_unmarked),
               names_to = "Type",
               values_to = "Estimated_Count")



#Step 4: make a summary of marked vs unmarked by month:
historic_summary_mark_unmark <- historic_data_2015_2024 %>%
  filter(Site == "Stamp") %>%
  mutate(
    ReviewDate = as.Date(`Review Date`),
    MonthDay = as.Date(format(ReviewDate, "2000-%m-%d"))
  ) %>%
  group_by(MonthDay) %>%
  summarise(
    mean_marked = mean(`Co  Mark`, na.rm = TRUE),
    sd_marked = sd(`Co  Mark`, na.rm = TRUE),
    n_marked = sum(!is.na(`Co  Mark`)),
    mean_unmarked = mean(`Co  NoMark`, na.rm = TRUE),
    sd_unmarked = sd(`Co  NoMark`, na.rm = TRUE),
    n_unmarked = sum(!is.na(`Co  NoMark`)),
    .groups = "drop"
  ) %>%
  mutate(
    se_marked = sd_marked / sqrt(n_marked),
    lower_marked = mean_marked - qt(0.975, df = pmax(n_marked - 1, 1)) * se_marked,  # Prevent df < 1
    upper_marked = mean_marked + qt(0.975, df = pmax(n_marked - 1, 1)) * se_marked,
    se_unmarked = sd_unmarked / sqrt(n_unmarked),
    lower_unmarked = mean_unmarked - qt(0.975, df = pmax(n_unmarked - 1, 1)) * se_unmarked,
    upper_unmarked = mean_unmarked + qt(0.975, df = pmax(n_unmarked - 1, 1)) * se_unmarked
  )




#Plot the marked vs unmarked for this year, AND the past few years:
#A) Select which years you are interested in:
selected_years <- c(2024, 2023, 2022)


co_years_long <- historic_data_2015_2024 %>%
  filter(year %in% selected_years) %>%
  mutate(
    MonthDay = as.Date(format(as.Date(`Review Date`), "2000-%m-%d"))
  ) %>%
  select(year, MonthDay, `Co  Mark`, `Co  NoMark`) %>%
  pivot_longer(cols = c(`Co  Mark`, `Co  NoMark`),
               names_to = "Type",
               values_to = "Count") %>%
  mutate(
    MarkStatus = case_when(
      Type == "Co  Mark" ~ "Marked",
      Type == "Co  NoMark" ~ "Unmarked",
      TRUE ~ NA_character_
    )
  )


#B) Compute the cumulative sums for marked and unmarked given the historic data:
historic_cumulative <- historic_data_2015_2024 %>%
  filter(Site == "Stamp") %>%
  mutate(
    ReviewDate = as.Date(`Review Date`),
    MonthDay = as.Date(format(ReviewDate, "2000-%m-%d"))
  ) %>%
  arrange(year, MonthDay) %>%
  group_by(year, MonthDay) %>%
  summarise(
    Co_Mark = sum(`Co  Mark`, na.rm = TRUE),
    Co_NoMark = sum(`Co  NoMark`, na.rm = TRUE)
  ) %>%
  group_by(year) %>%
  arrange(MonthDay) %>%
  mutate(
    cum_marked = cumsum(Co_Mark),
    cum_unmarked = cumsum(Co_NoMark)
  ) %>%
  ungroup()



#C) Summarize across all the years by Month&Day:
historic_summary_cumulative <- historic_cumulative %>%
  group_by(MonthDay) %>%
  summarise(
    mean_marked = mean(cum_marked, na.rm = TRUE),
    sd_marked = sd(cum_marked, na.rm = TRUE),
    n_marked = sum(!is.na(cum_marked)),
    
    mean_unmarked = mean(cum_unmarked, na.rm = TRUE),
    sd_unmarked = sd(cum_unmarked, na.rm = TRUE),
    n_unmarked = sum(!is.na(cum_unmarked))
  ) %>%
  mutate(
    se_marked = sd_marked / sqrt(n_marked),
    lower_marked = mean_marked - qt(0.975, df = n_marked - 1) * se_marked,
    upper_marked = mean_marked + qt(0.975, df = n_marked - 1) * se_marked,
    
    se_unmarked = sd_unmarked / sqrt(n_unmarked),
    lower_unmarked = mean_unmarked - qt(0.975, df = n_unmarked - 1) * se_unmarked,
    upper_unmarked = mean_unmarked + qt(0.975, df = n_unmarked - 1) * se_unmarked
  ) %>%
  ungroup()



#D) Get the total coho data (not separated into marked vs unmarked):
actual_total_data <- stamp_cn %>%
  mutate(MonthDay = format(as.Date(date), "%m-%d")) %>%
  filter(species == "CO", year == curr_year) %>%  
  group_by(MonthDay) %>%
  summarise(Daily_Count = sum(count, na.rm = TRUE)) %>%
  arrange(MonthDay) %>%
  mutate(
    Cumulative_Count = cumsum(Daily_Count),
    # Convert MonthDay to a Date for plotting on x-axis:
    PlotDate = as.Date(paste0("2000-", MonthDay))
  ) %>%
  ungroup()


#Plot the data:
ggplot() +

    #HISTORIC RIBBONS & LINES:
    geom_ribbon(data = historic_summary_cumulative,
                aes(x = MonthDay, ymin = lower_marked, ymax = upper_marked),
                fill = "blue", alpha = 0.2) +
    geom_line(data = historic_summary_cumulative,
              aes(x = MonthDay, y = mean_marked),
              color = "blue", size = 1) +

    geom_ribbon(data = historic_summary_cumulative,
                aes(x = MonthDay, ymin = lower_unmarked, ymax = upper_unmarked),
                fill = "darkgreen", alpha = 0.2) +
    geom_line(data = historic_summary_cumulative,
              aes(x = MonthDay, y = mean_unmarked),
              color = "darkgreen", size = 1) +


    #PREVIOUS YEAR'S LINES:
    # #If you want them smoothed:
    # geom_smooth(data = co_years_long,
    #             aes(x = MonthDay, y = Count, color = factor(year), linetype = MarkStatus, group = interaction(year, MarkStatus)),
    #             method = "loess", se = FALSE, size = 1) +

    #If you want the raw data (unsmoothed):
    # geom_line(data = co_years_long,
    #           aes(x = MonthDay, y = Count, color = factor(year), linetype = MarkStatus, group = interaction(year, MarkStatus)),
    #           size = 1) +


    #CURRENT YEAR'S LINES:
    geom_line(data = proj_long %>% filter(Type == "est_marked"),
              aes(x = MonthDay, y = Estimated_Count, linetype = "Marked (Projected)"),
              color = "black", size = 1) +

    geom_line(data = proj_long %>% filter(Type == "est_unmarked"),
              aes(x = MonthDay, y = Estimated_Count, linetype = "Unmarked (Projected)"),
              color = "black", size = 1) +
  
  # ACTUAL TOTAL LINE (red)
  geom_line(data = actual_total_data,
            aes(x = PlotDate, y = Cumulative_Count, linetype = "Actual Total"),
            color = "red", size = 1)  +
  
  # LEGENDS & SCALES:
    
    # scale_color_manual(
    #   name = "Year",
    #   values = c("2022" = "yellow", "2023" = "orange", "2024" = "red")
    # ) +

    scale_linetype_manual(
      name = "Mark Status",
      values = c(
        "Marked" = "solid",
        "Unmarked" = "dashed",
        "Marked (Projected)" = "solid",
        "Unmarked (Projected)" = "dashed",
        "Actual Total" = "solid"
      )
    ) +

    #LIMIT THE X AXIS:
    scale_x_date(date_labels = "%b-%d", date_breaks = "1 week",
                 limits = as.Date(c("2000-08-01", "2000-10-20"))) +
  
    scale_y_continuous(limits = c(0, 20000)) +

    labs(
      title = "Historic and Current Marked (Blue) vs Unmarked (Green) Coho",
      x = "Month-Day",
      y = "Coho Count"
    ) +

    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
  
  



#NOT-CUMULATIVE:

#A) Convert cumulative to daily estimates:
proj_daily <- proj_long %>%
  arrange(Type, MonthDay) %>%
  group_by(Type) %>%
  mutate(
    Daily_Count = Estimated_Count - lag(Estimated_Count, default = 0)
  ) %>%
  ungroup()


ggplot() +

  #HISTORIC RIBBONS & LINES:
  geom_ribbon(data = historic_summary_mark_unmark,
              aes(x = MonthDay, ymin = lower_marked, ymax = upper_marked),
              fill = "blue", alpha = 0.2) +
  geom_line(data = historic_summary_mark_unmark,
            aes(x = MonthDay, y = mean_marked),
            color = "blue", size = 1) +

  geom_ribbon(data = historic_summary_mark_unmark,
              aes(x = MonthDay, ymin = lower_unmarked, ymax = upper_unmarked),
              fill = "darkgreen", alpha = 0.2) +
  geom_line(data = historic_summary_mark_unmark,
            aes(x = MonthDay, y = mean_unmarked),
            color = "darkgreen", size = 1) +


  #PREVIOUS YEAR'S LINES:
  # #If you want them smoothed:
  # geom_smooth(data = co_years_long,
  #             aes(x = MonthDay, y = Count, color = factor(year), linetype = MarkStatus, group = interaction(year, MarkStatus)),
  #             method = "loess", se = FALSE, size = 1) +

  #If you want the raw data (unsmoothed):
  # geom_line(data = co_years_long,
  #           aes(x = MonthDay, y = Count, color = factor(year), linetype = MarkStatus, group = interaction(year, MarkStatus)),
  #           size = 1) +


  #CURRENT YEAR'S LINES:
  geom_line(data = proj_daily %>% filter(Type == "est_marked"),
            aes(x = MonthDay, y = Daily_Count, linetype = "Marked (Projected)"),
            color = "black", size = 1) +
  
  geom_line(data = proj_daily %>% filter(Type == "est_unmarked"),
            aes(x = MonthDay, y = Daily_Count, linetype = "Unmarked (Projected)"),
            color = "black", size = 1) +
  # 
  # #LEGENDS & SCALES:
  # scale_color_manual(
  #   name = "Year",
  #   values = c("2022" = "yellow", "2023" = "orange", "2024" = "red")
  # ) +

  scale_linetype_manual(
    name = "Mark Status",
    values = c(
      "Marked" = "solid",
      "Unmarked" = "dashed",
      "Marked (Projected)" = "solid",
      "Unmarked (Projected)" = "dashed"
    )
  ) +

  #LIMIT THE X AXIS:
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 week",
               limits = as.Date(c("2000-08-01", "2000-10-01"))) +

  labs(
    title = "Historic and Current Marked (Blue/Red) vs Unmarked (Green/Dashed) Coho",
    x = "Month-Day",
    y = "Coho Count"
  ) +

  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )



########################## Coho Spaghetti Plot ################################


# Summarise data and feed into plot
(co_spaghetti_p <- stamp_cn |> 
  # Compare to the last 10 years
  filter(
    between(year, max(year) - 11, max(year) -1),
    species == "CO",
    julian < 310
  ) |> 
  group_by(year) |> 
  mutate(hjust = runif(1, 0.8, 1)) |> # Add random hjust values to reduce overlap between labels in geom_textline
  ggplot(
    aes(
      as.Date(julian, origin = paste0(curr_year - 1, "-12-31")), 
      cum_count
    )
  ) +
  # Historical data as thin grey lines
  geom_textline(
    aes(label = year, group = year, hjust = hjust),
    colour = "grey50",
    alpha = 0.7
  ) +
  # 2023 as thick red line with semi-transparent label
  geom_labelline(
    data = filter(
      stamp_cn, 
      species == "CO", 
      year == max(year)
    ), 
    aes(y = cum_count),
    label = curr_year,
    colour = "red",
    hjust = 0.9,
    vjust = 0.1,
    linewidth = 1.25,
    boxcolour = "white",
    alpha = 0.75,
    label.padding = unit(0.1, "lines"),
    gap = TRUE,
    text_smoothing = 60
  ) +
  scale_x_date(
    breaks = "2 weeks", date_labels = "%d %b"
  ) +
  scale_y_continuous(position = "right") + # Put y axis on right to show count values at the end of the time series
  guides(colour = "none") +
  coord_cartesian(
    xlim = as.Date(
      c(
        paste0(curr_year, "-08-01"), 
        paste0(curr_year, "-11-05")
      )
    ),
    expand = FALSE
  ) +
  labs(
    x = NULL, 
    y = "Cumulative Stamp Falls Coho escapement"
  ) +
  theme(
    axis.title.y.right = element_text( # Increase y-axis title margin
      margin = margin(l = 0.5, unit = "lines")
    )
  ) 
)


# Save to the network folder
ggsave(
  plot = co_spaghetti_p, 
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Escapement plot/",
    "R-PLOT_2025_CO_cum-esc-timing",
    format(Sys.Date(), "%Y-%m-%d"), "_",  # Add current date here
    ".png"
  ),
  height = 4.5,
  width = 8,
  units = "in"
)



