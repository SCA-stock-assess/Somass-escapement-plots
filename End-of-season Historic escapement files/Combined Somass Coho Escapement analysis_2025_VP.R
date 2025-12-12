
curr_year<- 2025

#STAMP FALLS:
# Load historical escapement data from August onward
stampCoho <- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/TERMINAL_AREAS/TERMRBT/Stampfalls.xlsx",
  sheet = "STAMP Escapement Data",
  skip = 28,
  na = ""
) |> 
  select(1:5) |> 
  pivot_longer(
    cols = CO:JCO,
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
  ungroup() |> 
  filter(year >= "2013") #to make it start the same year as Sproat coho data

min(stampCoho$date)
min(sproatCoho$date)

#SPROAT:

# Load historical escapement data from August onward
sproatCoho <- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/TERMINAL_AREAS/TERMRBT/Stampfalls.xlsx",
  sheet = "Sproat Escapement Data ",
  skip = 28,
  na = ""
) |> 
  select(1:5) |> 
  pivot_longer(
    cols = CO:JCO,
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
######################---------------------Combine Stamp and Sproat

SomassCohodf <-
  bind_rows(stampCoho, sproatCoho) |>
  group_by(date) |>
  summarise(
    SomassCoho = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(year=year(date)) |> 
 
group_by(year) |>
mutate(
  CohoCum = cumsum(SomassCoho)
  ) |>
ungroup()


#filter out 2025 as a separate df for plotting purposes
Somass2025Coho<- SomassCohodf |> 
filter(year=="2025") |> 
  mutate(MonthDay = as.Date(format(date, "2000-%m-%d")))

##########-------------
RCH_Quartiles <- read_xlsx(
  "RbtObsQuart.xlsx",
  sheet = "Sheet1",
  na = ""
) 

RCH_Quartiles <- RCH_Quartiles %>%
  rename(year = `Return Year`)



# Merge  with quartiles
SomassCohoWQuart <- SomassCohodf %>%
  left_join(RCH_Quartiles, by = "year") %>%
  mutate(
    MonthDay = as.Date(format(date, "2000-%m-%d")),
    ObsQuart = factor(ObsQuart)    # factor for coloring
  ) |> 
  filter(!is.na(ObsQuart))

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
##_---------------Plotting

SomassCohoPlot<- ggplot() +
  
  
  # Historic lines with year labels (except current year):
  geom_textline(
    data = SomassCohoWQuart %>% filter(year != curr_year),
    
    aes(x = MonthDay, y = CohoCum, label = year, 
        group = year, colour = ObsQuart, linetype =ObsQuart),
    linewidth = 1,
    alpha = 0.9,
    show.legend = TRUE,
    key_glyph = "path",   # <- critical for showing the legend properly
    hjust = runif(1, 0.8, 1)
  ) +
  
  # Current year (2025) bold black line with label:
  geom_labelline(
    data = Somass2025Coho,
    
    aes(x = MonthDay, y = CohoCum ),
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
        panel.grid.x = element_blank(),
        panel.grid.y = element_blank(),
        axis.line = element_line()) +
  scale_y_continuous(name = "Somass Coho Escapement", position = "right", breaks = seq(0, 70000, by = 10000) )  


# Display plot
plot(SomassCohoPlot)


ggsave(
  plot = SomassCohoPlot,
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Escapement plot/",
    "SomassCohoAll",
    format(Sys.Date(), "%Y-%m-%d"), "_",  # Adds current date here
    ".png"
  ),
  height = 5,
  width = 10,
  units = "in"
)

