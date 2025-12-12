
curr_year <- 2025
#STAMP FALLS:
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




#SPROAT:

########################Load historical escapement data from August onward

sproat_cn <- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/TERMINAL_AREAS/TERMRBT/Stampfalls.xlsx",
  sheet = "Sproat Escapement Data ",
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

#################### READ IN Sproat files with Jack/Adult/Marked/Unmarked From GM

#2015:

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
 
  data_2015_sproat = data_2015_sproat,

  data_2016_sproat = data_2016_sproat,

  data_2017_sproat = data_2017_sproat,

  data_2018_sproat = data_2018_sproat,

  data_2019_sproat = data_2019_sproat,

  data_2020_sproat = data_2020_sproat,

  data_2021_sproat = data_2021_sproat,

  data_2022_sproat = data_2022_sproat,

  data_2023_sproat = data_2023_sproat,

  data_2024_sproat = data_2024_sproat
)

#Step 3: Apply the filter to all the data:
filtered_data <- lapply(all_data, function(df) {
  df[, intersect(keep_cols, names(df)), drop = FALSE]
})


#Step 4: Combine everything into one database:
historic_data_2014_2024 <- bind_rows(filtered_data, .id = "source")


#Step 5: remove any rows where the site is not stamp or sproat:
historic_data_2014_2024 <- historic_data_2014_2024 %>%
  filter(str_detect(Site, regex("stamp|sproat", ignore_case = TRUE))) #make sure it isn't case sensitive




#remove everything but historic_data_2015_2024:
rm(list = setdiff(ls(), c("historic_data_2014_2024", "curr_year")))




#################### READ IN THE CURRENT YEAR'S MARKED VS UNMARKED ESCAPEMENT DATA ##################
#

#stamp-Sproat mark rate is not provided in-season:
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
#################### Sproat WILD COHO Quartile Colored ################################
#Can be done when this years sproat mark rate data is available (IGNORE FOR NOW)

RCH_Quartiles <- read_xlsx(
  "RbtObsQuart.xlsx",
  sheet = "Sheet1",
  na = ""
) 

RCH_Quartiles <- RCH_Quartiles %>%
  rename(year = `Return Year`)

historic_cumulative <- historic_data_2014_2024 %>%
  filter(Site == "Sproat") %>%
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

# Merge historic cumulative data with quartiles
historic_cumulative_quart <- historic_cumulative %>%
  left_join(RCH_Quartiles, by = "year") %>%
  mutate(
    MonthDay = as.Date(MonthDay),  # ensure date format
    ObsQuart = factor(ObsQuart)    # factor for coloring
  )

# Define quartile colors
quartile_colors <- c(
  "4" = "darkgreen",    # Dark green
  "3" = "#6DA544",
  "2" = "#D55E00",    # Orange
  "1" = "#8B0000"     # Dark red
)

quartileLinetypes <- c(
  "4" = "solid",
  "3" = "twodash",
  "2" = "dotdash",
  "1" = "dotted"  
)



# Plot
CohoUnmarkedSProatWQuartiles<- ggplot() +
  
  
  # Historic lines with year labels (except current year):
  geom_textline(
    data = historic_cumulative_quart %>% filter(year != curr_year),
    
    #look only at the unmarked coho (wild):
    aes(x = MonthDay, y = cum_unmarked, label = year, 
        group = year, colour = ObsQuart, linetype =ObsQuart),
    linewidth = 1,
    alpha = 0.9,
    show.legend = TRUE,
    key_glyph = "path",   # <- critical for showing the legend properly
    hjust = runif(1, 0.8, 1)
  ) +
  
  # Current year (2025) bold black line with label:
  geom_labelline(
    data = current_data,
    
    #Look only at the unmarked coho (wild):
    aes(x = MonthDay, y = Co_NoMark_Cumulative),
    label = curr_year,
    colour = "black",
    linewidth = 1.5,
    boxcolour = "transparent",
    alpha = 0.9,
    label.padding = unit(0.15, "lines"),
    hjust = 0.8,
    vjust = 0.1,
    gap = TRUE,
    text_smoothing = 60
  ) +
  
  
  
  # Proper X axis labels
  scale_x_date(
    date_labels = "%b-%d",
    date_breaks = "1 week",
    limits = as.Date(c("2000-08-01", "2000-10-30"))) + xlab("") +
  
  
  scale_color_manual(
    name = "Marine Survival Quartile",
    na.translate = FALSE,  # removes NA from legend
    values = quartile_colors,
    labels = c(
      "1"= "Quartile 1: Critical",
      "2"= "Quartile 2: Low",
      "3"= "Quartile 3: Moderate",
      "4"= "Quartile 4: High"
    )
  ) +
  scale_linetype_manual(
    name = "Marine Survival Quartile",
    na.translate = FALSE,  # removes NA from legend
    values = quartileLinetypes,
    labels = c(
      "1"= "Quartile 1: Critical",
      "2"= "Quartile 2: Low",
      "3"= "Quartile 3: Moderate",
      "4"= "Quartile 4: High"
    )
  )+
  
  
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line()) +
  scale_y_continuous(name = "Sproat River Unmarked Coho Escapement", position = "right", breaks = seq(0, 20000, by = 2000) )  


# Display plot
plot(CohoUnmarkedSProatWQuartiles)


ggsave(
  plot = CohoUnmarkedSProatWQuartiles,
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Escapement plot/",
    "CohoUnmarkedSProatWQuartiles",
    format(Sys.Date(), "%Y-%m-%d"), "_",  # Adds current date here
    ".png"
  ),
  height = 5,
  width = 10,
  units = "in"
)


################### Sproat Hatchery COHO Quartile Colored ################################
#only 2025 seen thousands of marked Coho in Sproat
CohoMARKEDSproatWQuartiles<- ggplot() +
  
  
  # Historic lines with year labels (except current year):
  geom_textline(
    data = historic_cumulative_quart %>% filter(year != curr_year),
    
    #look only at the Marked coho
    aes(x = MonthDay, y = cum_marked, label = year, 
        group = year, colour = ObsQuart, linetype =ObsQuart),
    linewidth = 1,
    alpha = 0.9,
    show.legend = TRUE,
    key_glyph = "path",   # <- critical for showing the legend properly
    hjust = runif(1, 0.8, 1)
  ) +
  
  # Current year (2025) bold black line with label:
  geom_labelline(
    data = current_data,
    
    aes(x = MonthDay, y = Co_Mark_Cumulative),
    label = curr_year,
    colour = "black",
    linewidth = 1.5,
    boxcolour = "transparent",
    alpha = 0.9,
    label.padding = unit(0.15, "lines"),
    hjust = 0.9,
    vjust = 0.1,
    gap = TRUE,
    text_smoothing = 60
  ) +
  
  
  
  # Proper X axis labels
  scale_x_date(
    date_labels = "%b-%d",
    date_breaks = "1 week",
    limits = as.Date(c("2000-08-01", "2000-10-30"))) + xlab("") +
  
  
  scale_color_manual(
    name = "Marine Survival Quartile",
    na.translate = FALSE,  # removes NA from legend
    values = quartile_colors,
    labels = c(
      "1"= "Quartile 1: Critical",
      "2"= "Quartile 2: Low",
      "3"= "Quartile 3: Moderate",
      "4"= "Quartile 4: High"
    )
  ) +
  scale_linetype_manual(
    name = "Marine Survival Quartile",
    na.translate = FALSE,  # removes NA from legend
    values = quartileLinetypes,
    labels = c(
      "1"= "Quartile 1: Critical",
      "2"= "Quartile 2: Low",
      "3"= "Quartile 3: Moderate",
      "4"= "Quartile 4: High"
    )
  )+
  
  
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line()) +
  scale_y_continuous(name = "Sproat River Marked Coho Escapement", position = "right", breaks = seq(0, 20000, by = 2000) )  

# Display plot
print(CohoMARKEDSproatWQuartiles)
