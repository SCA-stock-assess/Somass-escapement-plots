# Code by Vahab based on pre-exisiting work of MH.
#Septmber 25, 2025

#Load the libraries
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(ggridges)
library(dplyr)
library(lubridate)
library(ggplot2) 
library(geomtextpath)
library(ggrepel)

# Enter the current analysis year
curr_year <- 2025



#################### READ IN THE HISTORIC FILES ##################
#2014:
data_2014_stamp <- read_xlsx(
  "2014 stamp counts.xlsx",
  sheet = "Coho",
  na = ""
) %>%
  mutate(
    year = 2014, #make a column that says the year
    `Review Date` = as.Date(`DATE`)) #make sure that the Review Date is "as.date" 

data_2014_stamp<- data_2014_stamp |> 
rename("Co  Mark" = `Sum of Co  Mark`,
       "Co  NoMark" = `Sum of Co  NoMark`,
       "CoJk  Mark"=`Sum of CoJk  Mark`,
       "CoJk  NoMark"= `Sum of CoJk  NoMark`) |> 
  mutate("Co"= `Co  Mark` +`Co  NoMark`, "CoJk"=`CoJk  Mark`+ `CoJk  NoMark`)
  

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
  data_2014_stamp = data_2014_stamp,
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
historic_data_2014_2024 <- bind_rows(filtered_data, .id = "source")


#Step 5: remove any rows where the site is not stamp or sproat:
historic_data_2014_2024 <- historic_data_2014_2024 %>%
  filter(str_detect(Site, regex("stamp|sproat", ignore_case = TRUE))) #make sure it isn't case sensitive




#remove everything but historic_data_2015_2024:
rm(list = setdiff(ls(), c("historic_data_2014_2024", "curr_year")))





#################### READ IN THE COHO AND CHINOOK ESCAPEMENT FILES ##################
#Note: this is the same code that Nick used in the other escapement code

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

# Load historical escapement data from August onward
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




#################### READ IN THE CURRENT YEAR'S MARKED VS UNMARKED ESCAPEMENT DATA ##################
#We get this data from Graham Murrel with Hupcasath - he broke it down into marked vs unmarked during
#the in-season chinook run this year for us to be able to analyze, but that doesn't typically get done
#from what I understand

#Sproat mark rate is not provided in-season:
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



#################### STAMP WILD COHO Quartile Colored ################################

RCH_Quartiles <- read_xlsx(
  "RbtObsQuart.xlsx",
  sheet = "Sheet1",
  na = ""
) 

RCH_Quartiles <- RCH_Quartiles %>%
  rename(year = `Return Year`)

historic_cumulative <- historic_data_2014_2024 %>%
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
CohoUnmarkedStampWQuartiles<- ggplot() +
  
  
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
  scale_y_continuous(name = "Stamp River Unmarked Coho Escapement", position = "right", breaks = seq(0, 20000, by = 2000) )  


# Display plot
plot(CohoUnmarkedStampWQuartiles)





# Save to the network folder
ggsave(
  plot = CohoUnmarkedStampWQuartiles,
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Escapement plot/",
    "CohoUnmarkedStampWQuartiles",
    format(Sys.Date(), "%Y-%m-%d"), "_",  # Add current date here
    ".png"
  ),
  height = 5,
  width = 10,
  units = "in"
)



################### STAMP Hatchery COHO Quartile Colored ################################

 CohoMARKEDStampWQuartiles<- ggplot() +
  
  
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
  scale_y_continuous(name = "Stamp River Marked Coho Escapement", position = "right", breaks = seq(0, 20000, by = 2000) )  

# Display plot
print(CohoMARKEDStampWQuartiles)


# Save to the network folder
ggsave(
  plot = CohoMARKEDStampWQuartiles,
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Escapement plot/",
    "CohoHatcheryStampWQuartiles",
    format(Sys.Date(), "%Y-%m-%d"), "_",  # Adds current date here
    ".png"
  ),
  height = 5,
  width = 10,
  units = "in"
)



################### STAMP Coho Quartile Colored ################################
#Adding a cumulative all Coho column (wild and Hatchery) to both datasets

historic_cumulative_quart<- historic_cumulative_quart |> 
  mutate(cum_AllCoho=cum_marked + cum_unmarked)

current_data <- current_data |> 
  mutate(CohoAll_Cumulative= Co_NoMark_Cumulative +Co_Mark_Cumulative)

StampCohoSpagethiQuartile<- ggplot() +
  
  
  # Historic lines with year labels (except current year):
  geom_textline(
    data = historic_cumulative_quart %>% filter(year != curr_year),
    
    #look only at the Marked coho
    aes(x = MonthDay, y = `cum_AllCoho`, label = year, 
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
    
    aes(x = MonthDay, y = `CohoAll_Cumulative`),
    label = curr_year,
    colour = "black",
    linewidth = 1.5,
    boxcolour = "white",
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
    limits = as.Date(c("2000-08-01", "2000-10-30"))
  ) +
  
  
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
  
  
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 1),
        axis.ticks = element_line(color = "black",linewidth = 2),
        legend.position = "top",
        legend.title = element_text(face = "bold")) + xlab("") + 
  scale_y_continuous(name = "Stamp River All Coho Escapement", position = "right", breaks = seq(0, 34000, by = 4000) ) 
# Display plot
print(StampCohoSpagethiQuartile)


# Save to the network folder
ggsave(
  plot = StampCohoSpagethiQuartile,
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Escapement plot/",
    "StampCohoSpagethiQuartile",
    format(Sys.Date(), "%Y-%m-%d"), "_",  # Add current date here
    ".png"
  ),
  height = 5,
  width = 10,
  units = "in"
)


###################  SPROAT COHO Quartile Colored ################################

 

 RCH_Quartiles <- read_xlsx("RbtObsQuart.xlsx", sheet = "Sheet1", na = "") %>%
    rename(year = `Return Year`) %>%
    mutate(year = as.integer(year))
  
  
  #B) Merge the quartiles with plotting data:
  Sproat_cn_with_quartiles <- sproat_cn %>%
    left_join(RCH_Quartiles, by = "year")
  
  Sproat_cn_with_quartiles |> 
    filter(!is.na(ObsQuart))
  
  quartile_colors <- c(
    "4" = "darkgreen",  
    "3" = "#6DA544",
    "2" = "#D55E00",  
    "1" = "#8B0000" 
  )
  quartileLinetypes <- c(
    "4" = "solid",
    "3" = "twodash",
    "2" = "dotdash",
    "1" = "dotted"  
  )
  
  legend_quartiles <- tibble(
    ObsQuart = factor(1:4),
    x = as.Date("2000-08-01"),
    y = 0
  )
  
  # Summarise data and save it
  plot_data <- Sproat_cn_with_quartiles |>
    filter(
      between(year, max(year) - 11, max(year) - 1),
      species == "CO",
      between( julian ,205, 320)
    ) |>
    group_by(year) |>
    mutate(hjust = runif(1, 0.8, 1))
  
  labelline_data <- Sproat_cn_with_quartiles %>%
    filter(species == "CO", year == max(year), julian > 205)
  
  # Main plot
  CohoSproatQuartile <- ggplot(plot_data,
                           aes(x = as.Date(julian, origin = paste0(curr_year - 1, "-12-31")),
                               y = cum_count,
                               group = year)
  ) +
    # Spaghetti lines
    geom_line(
      aes(group = interaction(year, ObsQuart),
          colour=factor(ObsQuart),
          linetype = factor(ObsQuart)),
      linewidth = 1,
      alpha=0.7
      
    ) +
    
    # Add year labels at the end of each line
    geom_text(
      data = plot_data %>%
        group_by(year) %>%
        filter(julian == max(julian)), # end of line
      aes(label = year,
          colour = factor(ObsQuart)),
      hjust = 0.3,
      vjust = 0.5,
      size = 3,
      show.legend = FALSE
    ) +
    
    # Highlight current year with thicker line
    
    geom_line(
      data = labelline_data,
      aes(x = as.Date(julian, origin = paste0(curr_year - 1, "-12-31")),
          y = cum_count),
      colour = "black",
      linewidth = 1.6
    )+
    
    # Highlight current year label at end
    
    geom_text(
      data = labelline_data %>%
        filter(julian == max(julian)),
      aes(x = as.Date(julian, origin = paste0(curr_year - 1, "-12-31")),
          y = cum_count,
          label = curr_year),
      colour = "black",
      hjust = -0.3,
      vjust = 0.4,
      fontface = "bold"
    )+
    
    # Scales
    
    scale_x_date(
      date_labels = "%b-%d",
      date_breaks = "2 week") + xlab("") +
    
  
    scale_color_manual(
      name = "Observed Marine Survival Quartile",
      na.translate = FALSE,  # removes NA from legend
      values = quartile_colors,
      labels = c(
        "1"= "Quartile 1: Very Low",
        "2"= "Quartile 2: Low",
        "3"= "Quartile 3: Moderate",
        "4"= "Quartile 4: High"
      )
    ) +
    scale_linetype_manual(
      name = "Observed Marine Survival Quartile",
      na.translate = FALSE,  # removes NA from legend
      values = quartileLinetypes,
      labels = c(
        "1"= "Quartile 1: Very Low",
        "2"= "Quartile 2: Low",
        "3"= "Quartile 3: Moderate",
        "4"= "Quartile 4: High"
      )
    ) +
    
    theme(legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          legend.title = element_text(face = "bold")) + xlab("") + 
    scale_y_continuous(name = "Sproat River All Coho Escapement", position = "right",
                       breaks = seq(0, 12000, by = 2000) ) 
  
  # Print the plot
  plot(CohoSproatQuartile)
  
  ggsave(
    plot = CohoSproatQuartile,
    filename = paste0(
      "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
      curr_year,
      "/A23/Escapement plot/",
      "SproatAllCohoQuartile",
      format(Sys.Date(), "%Y-%m-%d"), "_",  # Add current date here
      ".png"
    ),
    height = 6,
    width = 12,
    units = "in"
  )
  
  
  






  
###############################################  Less used Plots#######################################
###################  STAMP Coho not colorcoded ################################
#Read in the Quartile data:

RCH_Quartiles <- read_xlsx(
  "RbtObsQuart.xlsx",
  sheet = "Sheet1",
  na = ""
) 


#NOT COLOURED ACCORDING TO QUARTILES:
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
      hjust = 0.8,
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
      ylim = c(0,40000),
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

###################  SPROAT COHO Not Color-coded ################################

  RCH_Quartiles <- read_xlsx(
    "RbtObsQuart.xlsx",
    sheet = "Sheet1",
    na = ""
  ) 
  
  # Summarise data and feed into plot
  (SproatCohoSpaghettiGrey <- sproat_cn |> 
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
      # 2025 as thick red line with semi-transparent label
      geom_labelline(
        data = filter(
          sproat_cn, 
          species == "CO", 
          year == max(year)
        ), 
        aes(y = cum_count),
        label = curr_year,
        colour = "red",
        hjust = 0.7,
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
        ylim = c(0,12000),
        expand = FALSE
      ) +
      labs(
        x = NULL, 
        y = "Cumulative Sproat Falls Coho escapement"
      ) +
      theme(
        axis.title.y.right = element_text( # Increase y-axis title margin
          margin = margin(l = 0.2, unit = "lines")
        )
      ) 
  )
  
  plot(SproatCohoSpaghettiGrey)
  
################## Coho Spaghetti Plot SPROAT and STAMP ################################
  StampCoho<- stamp_cn |> 
    filter(species=="CO")
  SproatCoho<- sproat_cn |> 
    filter(species=="CO")
  #Combine stamp and sproat database into one database called Somass:
  somassCoho <- bind_rows(StampCoho, SproatCoho) |>
    group_by(year, date, species) |>
    summarise(
      count = sum(count, na.rm = TRUE),
      cum_count = sum(cum_count, na.rm = TRUE),
      ann_ttl = sum(ann_ttl, na.rm = TRUE),
      julian = first(julian)
    ) |>
    ungroup() |>
    mutate(
      cum_prop = cum_count / ann_ttl
    )
  
  #Do the plotting:
  
  RCH_Quartiles <- read_xlsx(
    "RbtObsQuart.xlsx",
    sheet = "Sheet1",
    na = ""
  ) 
  
  
  #NOT COLOURED ACCORDING TO QUARTILES:
  # Summarise data and feed into plot
  (co_spaghetti_p <- SomassCoho |> 
      # Compare to the last 10 years
      filter(
        between(year, max(year, na.rm = TRUE) - 11, max(year,na.rm=TRUE) -1),
        species == "CO",
        julian < 310
      ) |> 
      group_by(year) |> 
      mutate(hjust = runif(1, 0.8, 1)) |> # Add random hjust values to reduce overlap between labels in geom_textline
      ggplot(
        aes(
          as.Date(julian, origin = paste0(curr_year - 1, "-12-31")), 
          cum_count,
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
          SomassCoho, 
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
        y = "Cumulative Somass Coho escapement"
      ) +
      theme(
        axis.title.y.right = element_text( # Increase y-axis title margin
          margin = margin(l = 0.5, unit = "lines")
        )
      ) 
  )
  
  
  
  
  #COLOURED ACCORDING TO QUARTILES:
  
  #A) Rename the columns:
  RCH_Quartiles <- RCH_Quartiles %>%
    rename(year = `Return Year`)
  
  #B) Merge the quartiles with plotting data:
  somass_cn_with_quartiles <- somassCoho %>%
    left_join(RCH_Quartiles, by = "year")
  
  somass_cn_with_quartiles |> 
    filter(!is.na(ObsQuart))
  
  #C) Set the quartiles to specific colours:
  # quartile_colors <- c(
  #   "1" = "#1b9e77",  
  #   "2" = "#7570b3",
  #   "3" = "#e7298a",  
  #   "4" = "#d95f02" 
  # )
  
  quartile_colors <- c(
    "4" = "darkgreen",  
    "3" = "#6DA544",
    "2" = "#D55E00",  
    "1" = "#8B0000" 
  )
  
  legend_quartiles <- tibble(
    ObsQuart = factor(1:4),
    x = as.Date("2000-08-01"),
    y = 0
  )
  
  # Summarise data and feed into plot
  (co_spaghetti_p_quart <- somass_cn_with_quartiles |> 
      filter(
        between(year, max(year) - 11, max(year) -1),
        julian < 310
      ) |> 
      group_by(year) |> 
      mutate(hjust = runif(1, 0.8, 1)) |> 
      ggplot(
        aes(
          as.Date(julian, origin = paste0(curr_year - 1, "-12-31")), 
          cum_count
        )
      ) +
      
      geom_point(
        data = legend_quartiles,
        aes(x = x, y = y, colour = ObsQuart),
        shape = 16, size = 4
      )+ 
      
      # Historical data as colored lines by quartile:
      geom_textline(
        aes(label = year, group = year, hjust = hjust, colour = factor(ObsQuart)),
        alpha = 0.9,
        show.legend = FALSE
      ) +
      
      # Highlight current year as red (unchanged)
      geom_labelline(
        data = filter(
          somass_cn_with_quartiles, 
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
      
      scale_color_manual(
        name = "Observed Quartile",
        values = quartile_colors
      ) +
      
      scale_x_date(
        breaks = "2 weeks", date_labels = "%d %b"
      ) +
      
      scale_y_continuous(position = "right") +
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
        y = "Cumulative Somass Coho escapement"
      ) +
      
      theme(
        axis.title.y.right = element_text(
          margin = margin(l = 0.5, unit = "lines")
        )
      ) +
      
      #Manually enter the colours:
      scale_color_manual(
        name = "Observed Quartile",
        values = quartile_colors,
        na.translate = FALSE  # removes NA from legend
      ) +
      
      #Add a legend:
      guides(
        colour = guide_legend(
          title = "Observed Quartile",
          override.aes = list(
            shape = 16,     # circle
            size = 4,
            linetype = 0    # no line
          )
        )
      )
  )
  
  
  
  
  