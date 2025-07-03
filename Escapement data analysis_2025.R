# Packages ----------------------------------------------------------------

pkgs <- c("tidyverse", "readxl", "ggridges", "geomtextpath")
#install.packages(pkgs)
  
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(ggridges)
# library(ggplot2) #not needed
library(geomtextpath)

# Enter the current analysis year
curr_year <- 2025

# Load historical and current Sockeye escapement data -----------------------------


# Escday data
escday <- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca//PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/Data/ESCAPEMENT_PROGRAM/Escday.xlsx",
  sheet = "Data",
  skip = 3,
  na = ""
) |> 
  rename(
    "adj_adults" = "Adjusted Adults.  Includes bypass since 2004 but not Biosamples.",
    "adj_jacks" = "Adjusted Jacks.  Includes bypass since 2004 but not Biosamples."
  ) |> 
  mutate(
    year = if_else(year < 2000,
                   as.numeric(paste0(19, year)),
                   year),
    date = as.Date(paste(year, month, day, sep = "-")),
    julian = date |> format("%j") |> as.numeric(),
    adj_adults = case_when(
      year == max(year) & is.na(`Adjusted net Adult up count`) ~ NA_real_, # Preserve NAs for days not yet observed in current year
      is.na(adj_adults) & !is.na(`Stamp Falls Adjusted Adults`) ~ `Stamp Falls Adjusted Adults`,
      is.na(adj_adults) & year < max(year) ~ 0,
      TRUE ~ adj_adults),
    adj_jacks = case_when(
      is.na(adj_jacks) ~ `Adjusted net Jack up count`,
      is.na(adj_jacks) & year < max(year) ~ 0,
      TRUE ~ adj_jacks)
  ) |> 
  select(month, day, system, year, date, julian, contains("adj_")) |> 
  group_by(system, year) |> 
  arrange(julian) |> 
  mutate(
    across(adj_adults:adj_jacks, ~sum(.x, na.rm = TRUE), .names = "ttl_{.col}"),
    #across(adj_adults:adj_jacks, ~if_else(is.na(.x) & year < max(year), 0, .x)),
    across(adj_adults:adj_jacks, ~cumsum(.x), .names = "cum_{.col}"),
    prop_ttl_adult = adj_adults / ttl_adj_adults,
    prop_ttl_jack = adj_jacks / ttl_adj_jacks,
    cum_prop_adult = cum_adj_adults / ttl_adj_adults,
    cum_prop_jack = cum_adj_jacks / ttl_adj_jacks,
    system = if_else(system == "GCL",
                     "Great Central Lake",
                     "Sproat Lake"),
    stop.seq = case_when(
      between(cum_prop_adult,0.01,0.5) ~ ">1% date",
      between(cum_prop_adult,0.5,0.99) >= 0.5 ~ ">50% date",
      cum_prop_adult >=0.99 ~ ">99% date",
      TRUE ~ "<1% date"),
    d.m = paste(day,"-",month.name[month],sep = "")
  ) |> 
  ungroup()




#Remove any row where the "system" column is blank:
escday <- escday[!is.na(escday$system) & trimws(escday$system) != "", ]

### THIS GOES INTO SOXSUM ROWS 430-436 (APPROX):
# Recent 3-day average and SD for both systems
escday |> 
  filter(!is.na(cum_adj_adults)) |> 
  group_by(system) |> 
  slice_max(order_by = date, n = 3) |> 
  summarise(
    mean = mean(adj_adults),
    sd = sd(adj_adults)
  )



# Cumulative current versus historical Sockeye timing graphs -------------------------------------------

###NOTE:
#you need to have at least 2 data points entered into  Escday.xlsx for SPR and GCL (Sproat Lake and Great Central Lake)
#in order for this code to run, because it needs at least 2 points to be able to draw a line



# Update current Somass escapement target
som_esc <- 358333 ###FOR a 750,000 return
# som_esc <- 400000 ###FOR a 1,000,000 return
# som_esc <- 383333 ###FOR a 900,000 return
# som_esc <- 331250 ###FOR a 550,000 return
# som_esc <- 337500 ###for a 600,000 return
# som_esc <- 325000 for a 500,000 return: according to the management plan (500k return leads to 325k esc target)


# Forecasts for current year escapement
#split the escapement target into 2 different proportions (SPR and GCL):
esc_fcst <- data.frame(
  system = unique(escday$system),
  fcst = c(som_esc*0.17, som_esc*0.83) # Sproat, then GCL #(originally: THIS COMES FROM esc_fcst (the percentage of the total escapement)), but as the season goes on use Test fishery proportions
)

#print the forecasted escapement:
esc_fcst

#print a table that shows the total number of fish to date, the forecast, and the current
#proportion of the runsize so far based on the forecast.(Summarizes the cumulative escapement per system, and joins to compute as a proportion of forecast achieved)
current_est <- escday %>%
  filter(year == curr_year) %>%
  group_by(system) %>%
  summarise(total_cum_escapement = max(cum_adj_adults, na.rm = TRUE)) %>%
  arrange(desc(total_cum_escapement))
  
current_est <-  current_est %>%
  left_join(esc_fcst, by = "system") %>%
  mutate(proportion_of_forecast = total_cum_escapement / fcst)

current_est




# Biological reference points
# Note: these points were changed in 2025 (May 26 2025) by Mikayla Hamilton to reflect the
#values in Nicholas Brown's draft CSAS working paper that was reviewed on May 26&27 2025.
ref_pts <- data.frame(
  system = unique(escday$system),
  lwr = c(26103, 50729), # Sproat, Stamp --> old values: c(12060, 29290)
  upr = c(82073, 113575) # Sproat, Stamp --> old values: c(65570, 91640)
)




# Function to plot the curves
esc_p1 <- function(data, sys) {
  sys_fcst <- filter(esc_fcst, system == sys)$fcst
  curr_yr <- max(escday$year)
  
  ggplot(
    data, 
    aes(as.Date(julian, origin = "2020-12-31"), 
        mean)
  ) +
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$lwr/sys_fcst,
      lty = 2,
      linewidth = 0.8,
      colour = "red"
    ) +
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$upr/sys_fcst,
      lty = 2,
      linewidth = 0.8,
      colour = "gold2"
    ) +
    geom_ribbon(
      aes(ymin = l95, ymax = u95), 
      alpha = 0.25
    ) +
   
  geom_textline(
    label = paste0("Historic 2002 to ", curr_yr-1),
    linewidth = 1,
    hjust = 0.6,
    colour = "blue"
  ) +
  
  
    #Code below adds curves showing some of the most dramatic warm years
    # geom_textline(
    #   data = filter(escday, system == sys, year %in% c(2021, 2015)), 
    #   aes(y = cum_prop_adult,
    #       colour = as.factor(year),
    #       label = year),
    #   hjust = 0.6,
    #   linewidth = 1
    # ) +
    
    geom_textline(
      data = filter(escday, system == sys, year == curr_yr), 
      aes(
        y = cum_adj_adults/sys_fcst),
      label = as.character(curr_yr),
      text_smoothing = 30,
      colour = "black",
      hjust = 0.90,
      #gap = FALSE,
      #vjust = -0.25, # Adjust vertical text position relative to line
      linewidth = 1
    ) +
    
    scale_y_continuous(
      labels = scales::percent,
      name = "Proportion of total escapement",
      sec.axis = sec_axis(
        transform = ~.*sys_fcst, 
        labels = scales::comma,
        name = paste(curr_yr, "cumulative escapement")
      ),
      expand = c(0,0)
    ) +
    
    
    scale_x_date(breaks = "2 weeks", date_labels = "%d %b") +
    #scale_colour_manual(values = c("salmon", "purple")) +
    guides(colour = "none") +
    
    coord_cartesian(xlim = as.Date(c("2021-05-25", "2021-10-15")),
                    ylim = c(-0.05, 1.5)) + #show 5% below the 0, and 115% above the line ()
    labs(x = NULL) +
    theme(
      legend.position.inside = c(0.8, 0.3),
      plot.tag = element_text(colour = "grey35"),
      plot.tag.position = c(0.22,0.95),
      legend.background = element_rect(colour = "black")
    )
}



#### FIGURES 1 & 2 IN THE INSEASON BULLETIN FOR SOCKEYE:
# Curves with historical average proportions
(timing_plots <- purrr::set_names(unique(escday$system)) |> 
  map(~ escday |> 
        # Do the recent 20-year averages
        filter(
          year < max(year),
          year > 2002,
          system == .x
        ) |> 
        filter(!is.na(cum_prop_adult)) |> #remove NAs
        group_by(julian) |> 
        summarise(
          mean = mean(cum_prop_adult),
          l95 = quantile(cum_prop_adult, 0.05),
          u95 = quantile(cum_prop_adult, 0.95))
  ) %>% 
  imap(~esc_p1(.x, .y))
)




# Save cumulative current versus historical plots to network folder -------

#Need to make sure that in the current year SOCKEYE_MGMT folder, that you have an "Escapement plots" folder
#for these plots to go into
# 
# # Save to current year management folder
# timing_plots |> 
#   iwalk(
#     ~ggsave(
#       plot = .x, 
#       filename = paste0(
#         "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/",
#         curr_year,
#         "_MGT/Escapement plots/",
#         "R-PLOT_2025_Sk_cum-esc-timing_",
#         format(Sys.Date(), "%Y-%m-%d"), "_",  # Add current date here
#         .y,
#         ".png"
#       ),
#       height = 4.5,
#       width = 8,
#       units = "in"
#     )
#   )

#------------------------------ USING HISTORIC DATA ----------------------------

# Create a dataframe of historic returns: (got this from Area23_sockeye.xlsx file)
year <- 1988:2024
total_return <- c(
  755479, 368679, 325541, 1740640, 948437, 1346183, 452655, 118642,
  481489, 492408, 616831, 494499, 289475, 697130, 1062123, 912421,
  664629, 411700, 288391, 142145, 135921, 511547, 1501934, 1426640,
  742393, 281201, 866624, 2046096, 1104297, 384299, 276360, 193422,
  308908, 526950, 925083, 565254, 686831
)

Percent_GCL <- c("44%", "50%", "57%", "69%", "40%", "40%", "41%", "46%", "37%", "50%",
                 "45%", "54%", "28%", "64%", "55%", "57%", "65%", "56%", "68%", "51%",
                 "44%", "56%", "44%", "52%", "41%", "34%", "28%", "57%", "50%", "43%",
                 "19%", "26%", "45%", "65%", "34%", "47%", "80%")
Percent_GCL <- as.numeric(sub("%", "", Percent_GCL)) / 100

# bind the data together
historic <- data.frame(year, total_return, Percent_GCL)




#HISTORIC AVERAGES FOR CATCH AND STOCK COMP:
#What was the historic run size average?:
#Overall:
# mean(historic$total_return) #678196 --> 650,000 run

#Last 20 years: 
mean(historic$total_return[historic$year >= (curr_year-20)]) #666299.8 --> 650,000 run

#Last 10 years:
# mean(historic$total_return[historic$year >= (curr_year-10)]) #701750 --> 700,000 run


#What was the historic GCL split?
#Overall:
# mean(historic$Percent_GCL) #0.48

#Last 20 years:
mean(historic$Percent_GCL[historic$year >= (curr_year-20)]) #0.47

#Last 10 years:
# mean(historic$Percent_GCL[historic$year >= (curr_year-10)]) #0.47



#HISTORIC VALUES:

# Update current Somass escapement target
som_esc_hist <- 343750 ###FOR a 650,000 return
# som_esc_hist <- 350000 ###FOR a 700,000 return (according to the management plan)

# Update the GCL split:
# Forecasts for current year escapement
esc_fcst_hist <- data.frame(
  system = unique(escday$system),
  fcst = c(som_esc_hist*0.53, som_esc_hist*0.47) # Sproat being 53%, and GCL being 47% 
)

#print the forecasted escapement:
esc_fcst_hist



# Function to plot the curves
esc_p1 <- function(data, sys) {
  sys_fcst <- filter(esc_fcst_hist, system == sys)$fcst
  curr_yr <- max(escday$year)
  
  ggplot(
    data, 
    aes(as.Date(julian, origin = "2020-12-31"), 
        mean)
  ) +
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$lwr/sys_fcst,
      lty = 2,
      linewidth = 0.8,
      colour = "red"
    ) +
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$upr/sys_fcst,
      lty = 2,
      linewidth = 0.8,
      colour = "gold2"
    ) +
    geom_ribbon(
      aes(ymin = l95, ymax = u95), 
      alpha = 0.25
    ) +
    
    geom_textline(
      label = paste0("Historic 2002 to ", curr_yr-1),
      linewidth = 1,
      hjust = 0.6,
      colour = "blue"
    ) +
    
    
    #Code below adds curves showing some of the most dramatic warm years
    # geom_textline(
    #   data = filter(escday, system == sys, year %in% c(2021, 2015)), 
    #   aes(y = cum_prop_adult,
    #       colour = as.factor(year),
    #       label = year),
    #   hjust = 0.6,
    #   linewidth = 1
    # ) +
    
    geom_textline(
      data = filter(escday, system == sys, year == curr_yr), 
      aes(
        y = cum_adj_adults/sys_fcst),
      label = as.character(curr_yr),
      text_smoothing = 30,
      colour = "black",
      hjust = 0.90,
      #gap = FALSE,
      #vjust = -0.25, # Adjust vertical text position relative to line
      linewidth = 1
    ) +
    
    scale_y_continuous(
      labels = scales::percent,
      name = "Proportion of total escapement",
      sec.axis = sec_axis(
        transform = ~.*sys_fcst, 
        labels = scales::comma,
        name = paste(historic, "cumulative escapement")
      ),
      expand = c(0,0)
    ) +
    
    
    scale_x_date(breaks = "2 weeks", date_labels = "%d %b") +
    #scale_colour_manual(values = c("salmon", "purple")) +
    guides(colour = "none") +
    
    coord_cartesian(xlim = as.Date(c("2021-05-25", "2021-10-15")),
                    ylim = c(-0.05, 1.5)) + #show 5% below the 0, and 115% above the line ()
    labs(x = NULL) +
    theme(
      legend.position.inside = c(0.8, 0.3),
      plot.tag = element_text(colour = "grey35"),
      plot.tag.position = c(0.22,0.95),
      legend.background = element_rect(colour = "black")
    )
}



# Curves with historical average proportions
(timing_plots <- purrr::set_names(unique(escday$system)) |> 
    map(~ escday |> 
          # Do the recent 20-year averages
          filter(
            year < max(year),
            year > 2002,
            system == .x
          ) |> 
          filter(!is.na(cum_prop_adult)) |> #remove NAs
          group_by(julian) |> 
          summarise(
            mean = mean(cum_prop_adult),
            l95 = quantile(cum_prop_adult, 0.05),
            u95 = quantile(cum_prop_adult, 0.95))
    ) %>% 
    imap(~esc_p1(.x, .y))
)

# 
# #SAVE the plots:
# timing_plots |> 
#   iwalk(
#     ~ggsave(
#       plot = .x, 
#       filename = paste0(
#         "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/",
#         curr_year,
#         "_MGT/Escapement plots/",
#         "R-PLOT_2025_Sk_cum-esc-timing_HISTORIC_",
#         format(Sys.Date(), "%Y-%m-%d"), "_",  # Add current date here
#         .y,
#         ".png"
#       ),
#       height = 4.5,
#       width = 8,
#       units = "in"
#     )
#   )





################## SHOW BOTH CURRENT GCL:SPR AGAINST HISTORIC ##################


#Make a plot scaled to the proportion of fish returning (based on either historic or current year):
historic_plot <- function(system_name){
  sys_fcst_hist <- esc_fcst_hist %>% filter(system == system_name) %>% pull(fcst)
  sys_fcst_curr <- esc_fcst %>% filter(system == system_name) %>% pull(fcst)
  
  hist_data <- escday %>%
    filter(system == system_name , year < curr_year, year > 2002) %>%
    filter(!is.na(cum_prop_adult)) %>%
    group_by(julian) %>%
    summarise(
      mean = mean(cum_prop_adult),
      l95 = quantile(cum_prop_adult, 0.05),
      u95 = quantile(cum_prop_adult, 0.95),
      .groups = "drop"
    )
  
  curr_data <- escday %>% filter(system == system_name, year == curr_year)
  
  ggplot() +
    # 1a. Historic ribbon (scaled to current forecast) — GRAY
    geom_ribbon(
      data = hist_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        ymin = l95 * sys_fcst_hist / sys_fcst_curr,
        ymax = u95 * sys_fcst_hist / sys_fcst_curr
      ),
      fill = "gray40", alpha = 0.2
    ) +
    
    geom_hline(
      yintercept = filter(ref_pts, system == system_name)$lwr / sys_fcst_curr,
      linetype = "dashed",
      linewidth = 0.8,
      colour = "red"
    ) +
    
    # 1b. Upper reference line (yellow)
    geom_hline(
      yintercept = filter(ref_pts, system == system_name)$upr / sys_fcst_curr,
      linetype = "dashed",
      linewidth = 0.8,
      colour = "gold2"
    ) +
    
    # 2. Historic line (scaled to current forecast) — GRAY dashed
    geom_line(
      data = hist_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        y = mean * sys_fcst_hist / sys_fcst_curr
      ),
      colour = "gray40",
      linewidth = 1
    ) +
    
    # 3. Historic ribbon (scaled to historic forecast) — BLUE
    geom_ribbon(
      data = hist_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        ymin = l95,
        ymax = u95
      ),
      fill = "blue", alpha = 0.2
    ) +
    
    # 4. Historic line (scaled to historic forecast) — BLUE
    geom_line(
      data = hist_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        y = mean
      ),
      colour = "blue",
      linewidth = 1
    ) +
    
    # 5. Current year (black line) - adjust to current forecast percentages
    geom_line(
      data = curr_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        y = cum_adj_adults / sys_fcst_curr
      ),
      colour = "black",
      linewidth = 1
    ) +
    
    # 6. Add label to end of black line
    geom_textline(
      data = curr_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        y = cum_adj_adults / sys_fcst_curr
      ),
      label = as.character(curr_year),  # e.g., "2025"
      text_smoothing = 30,
      colour = "black",
      linewidth = 1.2,
      hjust = 0.90,
      vjust = 0,
      gap = TRUE,     
      size = 4
    ) +
    
    # Axes and labels: if right y axis is to be historic:
    scale_y_continuous(
      labels = scales::percent,
      name = "Proportion of 2025 escapement", #rename with curr_year
      sec.axis = sec_axis(
        trans = ~ .  * sys_fcst_hist,
        labels = scales::comma,
        name = "Historic cumulative escapement"
      )
    ) +
    
    
    # # Axes and labels: if right y axis is to be based on current escapement
    # scale_y_continuous(
    #   labels = scales::percent,
    #   name = "Proportion of 2025 escapement", #rename with curr_year
    #   sec.axis = sec_axis(
    #     trans = ~ .  * sys_fcst_curr,
    #     labels = scales::comma,
    #     name = paste(curr_year, "cumulative escapement")
    #   )
    # ) +
    
    scale_x_date(breaks = "2 weeks", date_labels = "%d %b") +
    coord_cartesian(xlim = as.Date(c("2021-05-25", "2021-10-15"))) +
    theme_minimal() +
    labs(title = system_name, x = "Date") +
    theme(
      legend.position = "none",
      panel.grid = element_blank(), #remove the grid lines in the background
      axis.title.y.left = element_text(color = "blue", size = 12, face = "bold"),
      axis.title.y.right = element_text(color = "gray40", size = 12, face = "bold"),
      axis.text.y.left = element_text(color = "blue"),
      axis.text.y.right = element_text(color = "gray40"),
      axis.title.x = element_text(size = 12, face = "bold")
    )
}

#Print the plots:
# historic_plot(system_name="Sproat Lake") 
# historic_plot(system_name="Great Central Lake") 








#Plot raw escapement (actual number of fish), rather than scaled to proportion:
historic_plot <- function(system_name){
  sys_fcst_hist <- esc_fcst_hist %>% filter(system == system_name) %>% pull(fcst)
  sys_fcst_curr <- esc_fcst %>% filter(system == system_name) %>% pull(fcst)
  
  # Historic proportions converted to raw escapement for both historic & scaled versions
  hist_data <- escday %>%
    filter(system == system_name , year < curr_year, year > 2002) %>%
    filter(!is.na(cum_prop_adult)) %>%
    group_by(julian) %>%
    summarise(
      mean_hist = mean(cum_prop_adult) * sys_fcst_hist,
      l95_hist = quantile(cum_prop_adult, 0.05) * sys_fcst_hist,
      u95_hist = quantile(cum_prop_adult, 0.95) * sys_fcst_hist,
      mean_scaled = mean(cum_prop_adult) * sys_fcst_curr,
      l95_scaled = quantile(cum_prop_adult, 0.05) * sys_fcst_curr,
      u95_scaled = quantile(cum_prop_adult, 0.95) * sys_fcst_curr,
      .groups = "drop"
    )
  
  curr_data <- escday %>% filter(system == system_name, year == curr_year)
  
  ggplot() +
    # 1. Historic ribbon scaled to current forecast — GRAY
    geom_ribbon(
      data = hist_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        ymin = l95_scaled,
        ymax = u95_scaled
      ),
      fill = "gray40", alpha = 0.2
    ) +
    
    # 2. Gray historic line scaled to current forecast
    geom_line(
      data = hist_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        y = mean_scaled
      ),
      colour = "gray40",
      linewidth = 1
    ) +
    
    # 3. Blue historic ribbon (raw historic forecast)
    geom_ribbon(
      data = hist_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        ymin = l95_hist,
        ymax = u95_hist
      ),
      fill = "blue", alpha = 0.2
    ) +
    
    # 4. Blue historic line (raw historic forecast)
    geom_line(
      data = hist_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        y = mean_hist
      ),
      colour = "blue",
      linewidth = 1
    ) +
    
    # 5. Black line = current year cumulative escapement (raw)
    geom_line(
      data = curr_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        y = cum_adj_adults
      ),
      colour = "black",
      linewidth = 1
    ) +
    
    # 6. Label at end of black line
    geom_textline(
      data = curr_data,
      aes(
        x = as.Date(julian, origin = "2020-12-31"),
        y = cum_adj_adults
      ),
      label = as.character(curr_year),
      text_smoothing = 30,
      colour = "black",
      linewidth = 1.2,
      hjust = 0.90,
      vjust = 0,
      gap = TRUE,
      size = 4
    ) +
    
    # 7. Reference lines (raw escapement thresholds)
    geom_hline(
      yintercept = filter(ref_pts, system == system_name)$lwr,
      linetype = "dashed",
      linewidth = 0.8,
      colour = "red"
    ) +
    geom_hline(
      yintercept = filter(ref_pts, system == system_name)$upr,
      linetype = "dashed",
      linewidth = 0.8,
      colour = "gold2"
    ) +
    
    #8. 100% proportion of curr_year forecast line:
    geom_hline(
      yintercept = sys_fcst_curr,   # or sys_fcst_hist, whichever is your 100% reference
      linetype = "dashed",
      color = "lightgreen",
      linewidth = 0.8) +

    # Y-axis (raw escapement only)
    scale_y_continuous(
      labels = scales::comma,
      name = "Cumulative Escapement (Number of Fish)"
    ) +
    
    # X-axis formatting
    scale_x_date(breaks = "2 weeks", date_labels = "%d %b") +
    coord_cartesian(xlim = as.Date(c("2021-05-25", "2021-10-15"))) +
    
    # Styling
    theme_minimal() +
    labs(title = system_name, x = "Date") +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.y.left = element_text(color = "black", size = 12, face = "bold"),
      axis.text.y.left = element_text(color = "black"),
      axis.title.x = element_text(size = 12, face = "bold")
    )
}


#Call the plots:
historic_plots <- set_names(unique(escday$system)) %>%
  map(~ historic_plot(system_name = .x))

historic_plots

#### FIGURES 1 & 2 IN THE INSEASON BULLETIN FOR SOCKEYE:
# Save the plots in the escapement folder on the STAD drive:
historic_plots %>%
  iwalk(~ ggsave(
    plot = .x,
    filename = paste0(
      "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/",
      curr_year,
      "_MGT/Escapement plots/",
      "R-PLOT_",
      curr_year,
      "_Sk_cum-esc-timing_HISTORIC_",
      format(Sys.Date(), "%Y-%m-%d"),
      "_",  
      gsub(" ", "_", .y),   # replace spaces with underscores in system name
      ".png"
    ),
    height = 4.5,
    width = 8,
    units = "in"
  ))


################################################################################


#### How do we reach the upper reference point by the end of the forecast (how many fish to escape in the next week to stay on track?)
#Get today's cumulative escapement
current_esc <- escday %>%
  filter(year == curr_year) %>%
  group_by(system) %>%
  summarise(total_cum_esc = max(cum_adj_adults, na.rm = TRUE))

#Join with reference points and forecasts
esc_progress <- current_esc %>%
  left_join(ref_pts, by = "system") %>%
  left_join(esc_fcst, by = "system") %>%
  mutate(
    remaining_to_target = pmax(upr - total_cum_esc, 0),  # how many more fish needed
    days_left = 7,
    needed_per_day = remaining_to_target / days_left
  )

#See the result
esc_progress %>%
  select(system, total_cum_esc, upr, remaining_to_target, needed_per_day)





# Annual curves overlaid on historical ------------------------------------


# This one can be drawn straight from escday
escday |> 
  filter(between(year, max(year)-11, max(year)-1)) |> 
  # Get the date corresponding to 95% of escapement having passed
  mutate(esc95 = if_else(cum_prop_adult > 0.95, 1, 0)) |> 
  group_by(system, year, esc95) %>% 
  mutate(tt95 = min(julian)) |> 
  group_by(system, year) |> 
  mutate(tt95 = max(tt95)) |> 
  ggplot(aes(as.Date(julian, origin = "2020-12-31"), cum_prop_adult)) +
  facet_grid(system ~ year) +
  # Lines showing all years in the dataset
  geom_line(
    data = escday |> 
      filter(year < max(year)) |> # Exclude current year data (incomplete)
      mutate(group = year,
             year = NULL),
    aes(group = group),
    colour = "grey75"
  ) +
  # Coloured lines for the individual years
  geom_line(aes(colour = tt95), size = 1) +
  coord_cartesian(xlim = as.Date(c("2021-05-25", "2021-10-15"))) +
  scale_y_continuous(
    labels = NULL,
    name = "Proportion of total escapement",
    expand = expansion(mult = 0.02)
  ) +
  # Customize such that darker lines show years with earlier escapement curves
  scale_colour_viridis_c(option = "F", end = 0.9) +
  guides(colour = "none") +
  labs(x = NULL) +
  theme(
    axis.ticks.y = element_blank(),
    panel.spacing.y = unit(0.2, "lines")
  )


# Ridgeline plots ---------------------------------------------------------


# Plot for current year
escday |> 
  filter(year == max(year)) |> 
  ggplot(
    aes(
      y = factor(system),
      as.Date(d.m, format = "%d-%b"), 
      height = prop_ttl_adult, 
      group = factor(system)
    )
  ) +
  geom_density_ridges(
    stat = "identity", 
    #alpha = 0.75, 
    scale = 0.95
  ) + 
  scale_y_discrete(
    limits = rev,
    expand = expansion(c(0, 0.05))
  ) +
  ggtitle("Sockeye escapement timing through Somass fishways") +
  labs(y = NULL, x = NULL) +
  scale_x_date(
    date_labels = "%b", 
    breaks = "1 month", 
    limits = as.Date(c("5-Apr","20-Nov"),format = "%d-%b"),
    expand = c(0,0)
  ) +
  theme_ridges() +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1,1),
    legend.box.background = element_rect(colour = "black",fill = "white")
  )


# Base uncoloured plot for both systems
(rp <- escday %>% 
   filter(year > 1982, year <= max(year)) %>% 
   ggplot(
     aes(
       y = factor(year),
       as.Date(d.m, format = "%d-%b"), 
       height = prop_ttl_adult, 
       group = factor(year)
     )
   ) +
   facet_wrap(~system, ncol = 2) +
   geom_density_ridges(
     stat = "identity", 
     alpha = 0.75, 
     scale = 3
   ) + 
   scale_y_discrete(limits = rev) +
   ggtitle("Sockeye escapement timing through Somass fishways") +
   labs(y = "Year", x = NULL, fill = "Criteria") +
   scale_x_date(
     date_labels = "%b", 
     breaks = "1 month", 
     limits = as.Date(c("5-Apr","20-Nov"),format = "%d-%b"),
     expand = c(0,0)
   ) +
   theme_ridges() +
   theme(
     legend.position = c(0.98, 0.98),
     legend.justification = c(1,1),
     legend.box.background = element_rect(colour = "black",fill = "white")
   )
)

# Coloured ridgelines
rp +
  geom_density_ridges_gradient(
    aes(fill = forcats::fct_rev(stop.seq)),
    scale = 3,
    stat = "identity",
    colour = FALSE
  ) +
  scale_fill_viridis_d(option = "magma", end = 0.8)

  
# Remove data prior to day 150
escday_trim <- escday %>% 
  filter(!julian < 150) %>% # Remove data from days prior to 150, per Howard's request
  group_by(system, year) %>% 
  arrange(date) %>% 
  mutate(
    ttl_adj_adults = sum(adj_adults, na.rm = TRUE),
    prop_ttl_adult = adj_adults/ttl_adj_adults,
    cum_adj_adults = cumsum(adj_adults),
    cum_ttl_adult = cum_adj_adults/ttl_adj_adults,
    stop.seq = case_when(
      between(cum_ttl_adult,0.01,0.5) ~ ">1% date",
      between(cum_ttl_adult,0.5,0.99) >= 0.5 ~ ">50% date",
      cum_ttl_adult >=0.99 ~ ">99% date",
      TRUE ~ "<1% date"),
    d.m = paste(day,"-",month.name[month],sep = "")
  ) %>% 
  ungroup()


# Update plot
rp %+% filter(escday_trim, year < max(year)) +
  geom_density_ridges_gradient(
    aes(fill = forcats::fct_rev(stop.seq)),
    scale = 3,
    stat = "identity",
    colour = FALSE
  ) +
  scale_x_date(limits = as.Date(c("15-May", "15-Nov"), format = "%d-%b")) +
  scale_fill_viridis_d(option = "magma", end = 0.8)


# Base plot for Somass total over previous 30 years
(rp2 <- escday %>% 
    filter(between(year, max(year) - 31, max(year) - 1)) %>% 
    group_by(year, d.m) |> 
    summarise(across(adj_adults:cum_adj_jacks, ~sum(.x, na.rm = TRUE))) |> 
    mutate(
      prop_ttl_adult = adj_adults / ttl_adj_adults,
      prop_ttl_jack = adj_jacks / ttl_adj_jacks,
      cum_prop_adult = cum_adj_adults / ttl_adj_adults,
      cum_prop_jack = cum_adj_jacks / ttl_adj_jacks,
      stop.seq = case_when(
        between(cum_prop_adult,0.01,0.5) ~ ">1% date",
        between(cum_prop_adult,0.5,0.99) >= 0.5 ~ ">50% date",
        cum_prop_adult >=0.99 ~ ">99% date",
        TRUE ~ "<1% date"
      )
    ) |> 
    ggplot(
      aes(
        y = factor(year), 
        as.Date(d.m, format = "%d-%b"), 
        height = prop_ttl_adult, 
        group = factor(year)
      )
    ) +
    geom_density_ridges(stat = "identity", alpha = 0.75, scale = 3) + # Disactivate for coloured ridgelines
    scale_fill_viridis_d(option = "magma", end = 0.8) +
    scale_y_discrete(limits = rev) +
    #ggtitle("Sockeye escapement timing through Somass fishways") +
    labs(y = "Year", x = NULL, fill = "Criteria") +
    scale_x_date(
      date_labels = "%b", 
      breaks = "1 month", 
      limits = as.Date(c("5-Apr","20-Nov"),format = "%d-%b"),
      expand = c(0,0)
    ) +
    theme_ridges() +
    theme(
      legend.position = c(0.02, 0.98),
      legend.justification = c(0,1),
      legend.box.background = element_rect(colour = "black",fill = alpha("white", 0.8))
    )
)


# Coloured ridgelines
rp2 +
  geom_density_ridges_gradient(
    aes(fill = forcats::fct_rev(stop.seq)),
    scale = 3,
    stat = "identity",
    colour = "black"
  )


# Compare current year to previous years in absolute numbers --------------

escday |> 
  filter(year >= 2020,
         !is.na(cum_adj_adults)
         ) %>%
  filter(julian < max(.[.$year == 2023,]$julian)) |> 
  mutate(label = if_else(julian == max(julian), year, NA_real_)) |> 
  ggplot(
    aes(
      julian, 
      cum_adj_adults, 
      colour = factor(year)
    )
  ) +
  facet_wrap(
    ~system, 
    ncol = 1, 
    scales = "free_y",
    strip.position = "right"
  ) +
  geom_textline(
    aes(label = year),
    hjust = .96,
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(130, NA)) +
  guides(colour = "none") +
  labs(
    x = "Day of the year", 
    y = "Cumulative adult escapement"
  ) +
  ggtitle("Recent years' escapement curves")



# Chinook & Coho escapement data -------------------------------------------


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


# Escapement target
esc_target <- 33000 #escapement target for 2025 was 33,000


# Summarise data and feed into plot
(cn_timing_plot <- stamp_cn |> 
  # Do the recent 20-year averages
  filter(between(year, max(year) - 21, max(year) -1),
         species == "CN") |> 
  group_by(julian) |> 
  summarise(
    mean = mean(cum_prop),
    l95 = quantile(cum_prop, 0.05),
    u95 = quantile(cum_prop, 0.95)
  ) |> 
  # Calculate smoothing curves for upper and lower 90th percentiles
  mutate(
    l95_smooth = stats::predict(glm(l95 ~ julian, family = binomial)),
    u95_smooth = stats::predict(glm(u95 ~ julian, family = binomial)),
    across(contains("smooth"), ~binomial()$linkinv(.x))
  ) |> 
  ggplot(
    aes(
      as.Date(julian, origin = paste0(curr_year - 1, "-12-31")), 
      mean
      )
    ) +
  geom_ribbon(
    aes(ymin = l95_smooth, ymax = u95_smooth), 
    alpha = 0.25
  ) +
  geom_textsmooth(
    label = paste("Historic ", max(stamp_cn$year) - 21, "to", max(stamp_cn$year) -1), 
    linewidth = 1,
    hjust = 0.6,
    colour = "blue",
    method = "glm",
    method.args = list(family = binomial())
  ) +
  geom_textline(
    data = filter(
      stamp_cn,                   
      species == "CN", 
      year == max(year)
    ), 
    aes(y = cum_count/esc_target),
    label = as.character(curr_year),
    colour = "red",
    hjust = 0.8,
    #vjust = -0.2,
    #gap = FALSE,
    linewidth = 1,
    text_smoothing = 60
  ) +
  scale_y_continuous(
    labels = scales::percent,
    name = "Proportion of total escapement",
    sec.axis = sec_axis(
      trans = ~.*esc_target, 
      labels = scales::comma,
      name = paste(curr_year, "cumulative escapement")
    ),
    expand = c(0,0)
  ) +
  scale_x_date(breaks = "2 weeks", date_labels = "%d %b") +
  guides(colour = "none") +
  coord_cartesian(
    xlim = as.Date(
      c(
        paste0(curr_year, "-08-01"), 
        paste0(curr_year, "-11-15")
        )
      )
    ) +
  labs(x = NULL) +
  theme(
    legend.position = c(0.8, 0.3),
    plot.tag.position = c(0.22,0.95),
    legend.background = element_rect(colour = "black")
  )
)


# Save latest plot version to network folder
ggsave(
  plot = cn_timing_plot, 
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Escapement plot/",
    "R-PLOT_2024_CN_cum-esc-timing.png"
  ),
  height = 4.5,
  width = 8,
  units = "in"
)



# Chinook data versus previous 15 years
(cn_spaghetti_p <- stamp_cn |> 
    # Do the recent 10-year averages
    filter(
      between(year, max(year) - 11, max(year) -1),
      species == "CN",
      julian < 310
    ) |> 
    group_by(year) |> 
    mutate(hjust = runif(1, 0.8, 1)) |> # Add random hjust values to reduce overlap between labels in geom_textline
    ggplot(
      aes(
        as.Date(julian, origin = paste0(curr_year -1, "-12-31")), 
        cum_count
      )
    ) +
    # Historical data as thin grey lines
    geom_textline(
      aes(label = year, group = year, hjust = hjust),
      colour = "grey50",
      alpha = 0.7
    ) +
    # Current year as thick red line with semi-transparent label
    geom_labelline(
      data = filter(
        stamp_cn, 
        species == "CN", 
        year == max(year)
      ), 
      aes(y = cum_count),
      label = as.character(curr_year),
      colour = "red",
      hjust = 0.85,
      linewidth = 1.25,
      boxcolour = "white",
      alpha = 0.75,
      label.padding = unit(0.1, "lines"),
      gap = TRUE
    ) +
    scale_x_date(
      breaks = "2 weeks", 
      date_labels = "%d %b", 
      limits = as.Date(
        c(
          paste0(curr_year, "-08-01"), 
          paste0(curr_year, "-11-05")
        )
      ),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      position = "right", # Put y axis on right to show count values at the end of the time series
      expand = expansion(mult = c(0, 0.05))
    ) + 
    guides(colour = "none") +
    labs(
      x = NULL, 
      y = "Cumulative Stamp Falls Chinook escapement"
    ) +
    theme(
      axis.title.y.right = element_text( # Increase y-axis title margin
        margin = margin(l = 0.5, unit = "lines")
      )
    ) 
)



# Save to the network folder
ggsave(
  plot = cn_spaghetti_p, 
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Escapement plot/",
    "R-PLOT_2024_CN_cum-esc-historic.png"
  ),
  height = 4.5,
  width = 8,
  units = "in"
)


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
    "R-PLOT_2024_CO_cum-esc-historic.png"
  ),
  height = 4.5,
  width = 8,
  units = "in"
)
