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
  "Site", "Review Date", "Co", "CoJk",
  "Co NoMark", "Co Mark", "CoJk NoMark", "CoJk Mark", "year"
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

#Step 4: ensure that the Review date is all a character
filtered_data <- lapply(filtered_data, function(df) {
  if ("Review Date" %in% names(df)) {
    df$`Review Date` <- as.character(df$`Review Date`)
  }
  df
})

#Step 5: Combine everything into one database:
historic_data_2015_2024 <- bind_rows(filtered_data, .id = "source")

#Step 6: force Review Date into a date-type:
historic_data_2015_2024$`Review Date` <- mdy(historic_data_2015_2024$`Review Date`)

#Step 7: remove any rows where the site is not stamp or sproat:
historic_data_2015_2024 <- historic_data_2015_2024 %>%
  filter(str_detect(Site, regex("stamp|sproat", ignore_case = TRUE))) #make sure it isn't case sensitive

#remove everything but historic_data_2015_2024:
rm(list = setdiff(ls(), "historic_data_2015_2024"))



############### Coho escapement data (same as what comes from the escapement R code) #############

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


# Coho curves -------------------------------------------------------------


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



