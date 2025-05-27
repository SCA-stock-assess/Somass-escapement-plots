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
som_esc <- 343750 ###UNSURE FOR 2025: according to Nick Brown this comes from the management plan



# Forecasts for current year escapement
esc_fcst <- data.frame(
  system = unique(escday$system),
  fcst = c(som_esc*0.27, som_esc*0.73) # Sproat, then GCL 
)

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
    coord_cartesian(xlim = as.Date(c("2021-05-25", "2021-10-15"))) +
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

# Save to current year management folder
timing_plots |> 
  iwalk(
    ~ggsave(
      plot = .x, 
      filename = paste0(
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/",
        curr_year,
        "_MGT/Escapement plots/",
        "R-PLOT_2025_Sk_cum-esc-timing_",
        .y,
        ".png"
      ),
      height = 4.5,
      width = 8,
      units = "in"
    )
  )


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
