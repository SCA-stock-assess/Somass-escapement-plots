
# Overhaul of the old escapement file and R code. Starting in 2026, Sockeye,
# Chinook, and Coho historical data are consolidated into SomassEsc.xlsx and
# read from there (replacing the old stampfall and escday sources). During the
# current season, the daily age-composition file provided by Graham is used as
# the current-year data source. Once post-season counts are finalized, that
# year's data will be appended to the respective species tab in SomassEsc.xlsx.
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(ggridges)
library(geomtextpath)

# Enter the current analysis year
curr_yr <- 2026  

# Load historical and current Sockeye escapement data -------------------
escday <- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca//PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/Data/ESCAPEMENT_PROGRAM/SomassEsc.xlsx",
  sheet = "Sockeye",
  na    = ""
) |>
  select(1:8) |>
  filter(!is.na(system), !is.na(year)) |>
  rename(
    adj_adults = "Adjusted net Adult up count",
    adj_jacks  = "Adjusted net Jack up count"
  ) |>
  mutate(
    year = if_else(year < 2000, as.numeric(paste0(19, year)), year),
    date = as.Date(paste(year, month, day, sep = "-")),
    julian = format(date, "%j") |> as.numeric(),
    
  ) |>
  select(month, day, system, year, date, julian, contains("adj_")) |>
  group_by(system, year) |>
  arrange(julian) |>
  mutate(
    # For each of adj_adults through adj_jacks, compute the column-wise total
    # (sum across all rows, ignoring NAs) and store as ttl_adj_adults, ttl_adj_jacks
    across(adj_adults:adj_jacks, \(x) sum(x, na.rm = TRUE), .names = "ttl_{.col}"),
    
    # Cumulative sum over time for each of those columns → cum_adj_adults, cum_adj_jacks
    across(adj_adults:adj_jacks, cumsum, .names = "cum_{.col}"),
    
    # Weekly (or daily) count as a proportion of the season total
    prop_ttl_adult = adj_adults / ttl_adj_adults,
    prop_ttl_jack  = adj_jacks  / ttl_adj_jacks,
    
    # Cumulative proportion of the season total reached by each time step
    cum_prop_adult = cum_adj_adults / ttl_adj_adults,
    cum_prop_jack  = cum_adj_jacks  / ttl_adj_jacks,
    
    system = if_else(system == "GCL", "Great Central Lake", "Sproat Lake"),
    
    # Bin each row into a run-timing phenology stage based on
    # where cumulative adult proportion falls in the season:
    #   <1%  → before run has meaningfully started
    #   1–50% → ascending limb
    #   50–99% → descending limb
    #   ≥99% → effectively complete
    stop.seq = case_when(
      between(cum_prop_adult, 0.01, 0.5)  ~ ">1% date",
      between(cum_prop_adult, 0.5,  0.99) ~ ">50% date",
      cum_prop_adult >= 0.99              ~ ">99% date",
      TRUE                                ~ "<1% date"
    ),
    
    # Formatted date label for axis/tooltip display, e.g. "3-September"
    d.m = paste(day, "-", month.name[month], sep = "")
  ) |>
  ungroup() 


CurrentYearSproatEsc<- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca//PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/2026_MGT/Daily Totals by Age 2026.xlsx",
  sheet = "Sproat",
  skip = 1,
  na    = ""
) %>% 
  select(`Review Date`:`64`) %>% 
  filter(!is.na(`Review Date`))

CurrentYearStampEsc<- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca//PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/2026_MGT/Daily Totals by Age 2026.xlsx",
  sheet = "Stamp",
  skip = 1,
  na    = ""
) %>% 
  select(`Review Date`:`64`) %>% 
  filter(!is.na(`Review Date`))

# Age columns present in the raw sheets
age_cols <- c("32", "42", "43", "52", "53", "62", "63", "64")

current_year_esc <- bind_rows(
  CurrentYearSproatEsc |> mutate(system = "Sproat Lake"),
  CurrentYearStampEsc  |> mutate(system = "Great Central Lake")
) |>
  rename(
    date       = `Review Date`,
    adj_adults = Adult,
    adj_jacks  = Jack
  ) |>
  mutate(
    date   = as.Date(date),
    month  = month(date),
    day    = day(date),
    year   = year(date),
    julian = yday(date)
  ) |>
  # drop age columns — not needed downstream
  select(-any_of(age_cols)) |>
  group_by(system) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    across(adj_adults:adj_jacks, \(x) sum(x, na.rm = TRUE), .names = "ttl_{.col}"),
    across(adj_adults:adj_jacks, cumsum,                     .names = "cum_{.col}"),
    prop_ttl_adult = adj_adults / ttl_adj_adults,
    prop_ttl_jack  = adj_jacks  / ttl_adj_jacks,
    cum_prop_adult = cum_adj_adults / ttl_adj_adults,
    cum_prop_jack  = cum_adj_jacks  / ttl_adj_jacks,
    stop.seq = case_when(
      between(cum_prop_adult, 0.01, 0.5)  ~ ">1% date",
      between(cum_prop_adult, 0.5,  0.99) ~ ">50% date",
      cum_prop_adult >= 0.99              ~ ">99% date",
      TRUE                                ~ "<1% date"
    ),
    d.m = paste(day, "-", month.name[month], sep = "")
  ) |>
  ungroup() |>
  select(month, day, system, year, date, julian,
         adj_adults, adj_jacks,
         ttl_adj_adults, ttl_adj_jacks,
         cum_adj_adults, cum_adj_jacks,
         prop_ttl_adult, prop_ttl_jack,
         cum_prop_adult, cum_prop_jack,
         stop.seq, d.m)

# Recent 3-day average and SD for both systems GOES TO SOXSUM (row 486)
current_year_esc |>
  filter(!is.na(cum_adj_adults)) |>
  group_by(system) |>
  slice_max(order_by = date, n = 3) |>
  summarise(
    mean = mean(adj_adults),
    sd   = sd(adj_adults)
  )

# Cumulative current versus historical Sockeye timing graphs ------------

# Current Somass escapement target
som_esc <- 382500  # Updated for 2026 (May 3, 2026)

# Current year escapement forecasts per system
esc_fcst <- data.frame(
  system = c("Sproat Lake", "Great Central Lake"),
  fcst   = c(som_esc * 0.20, som_esc * 0.80)
)

# Biological reference points — updated for 2026 (DFO, 2026)
ref_pts <- data.frame(
  system = c("Sproat Lake", "Great Central Lake"),
  lwr    = c(15220, 30887),
  upr    = c(68316, 92229)
)


# Historical average total escapement per system (last 20 years)
# Used to scale the grey historical line and ribbon to raw fish counts
hist_ref <- escday |>
  filter(
    year >= curr_yr - 20,
    year <= curr_yr - 1
  ) |>
  group_by(system, year) |>
  summarise(
    ttl = max(cum_adj_adults, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(system) |>
  summarise(
    hist_avg_esc = mean(ttl, na.rm = TRUE),
    .groups = "drop"
  )

# Plot function ----------------------------------------------------------
esc_p1 <- function(data, sys, curr_yr) {
  
  sys_fcst  <- filter(esc_fcst, system == sys)$fcst
  hist_fcst <- filter(hist_ref, system == sys)$hist_avg_esc
  stopifnot(length(sys_fcst) == 1, length(hist_fcst) == 1)
  
  orig_date <- as.Date(paste0(curr_yr - 1, "-12-31"))
  x_min     <- as.Date(paste0(curr_yr, "-05-25"))
  x_max     <- as.Date(paste0(curr_yr, "-10-15"))
  
  curr_esc  <- filter(current_year_esc, system == sys)
  data_clip <- filter(data, as.Date(julian, origin = orig_date) >= x_min,
                      as.Date(julian, origin = orig_date) <= x_max)
  
  ggplot(data_clip, aes(as.Date(julian, origin = orig_date), mean * hist_fcst)) +
    
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$lwr,
      lty = 2, linewidth = 0.8, colour = "#C0392B"
    ) +
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$upr,
      lty = 2, linewidth = 0.8, colour = "#E8A020"
    ) +
    geom_vline(
      xintercept = seq(x_min, x_max, by = "2 weeks"),
      colour = "grey80", linewidth = 0.4, alpha = 0.6
    ) +
    geom_ribbon(
      aes(ymin = l90 * hist_fcst, ymax = u90 * hist_fcst),
      fill = "#B0A090", alpha = 0.30
    ) +
    geom_ribbon(
      aes(ymin = l90 * sys_fcst, ymax = u90 * sys_fcst),
      fill = "#5B8DB8", alpha = 0.25
    ) +
    geom_line(linewidth = 1.4, colour = "grey40") +
    geom_line(aes(y = mean * sys_fcst), linewidth = 1.4, colour = "#4A7BA7") +
    geom_line(
      data      = filter(curr_esc,
                         as.Date(julian, origin = orig_date) >= x_min,
                         as.Date(julian, origin = orig_date) <= x_max),
      aes(x = as.Date(julian, origin = orig_date), y = cum_adj_adults),
      linewidth = 1.8, colour = "#1B7837"
    ) +
    annotate("text",
             x = x_max, y = tail(data_clip$mean * hist_fcst, 1),
             label = paste0(curr_yr - 20, " to ", curr_yr - 1, " mean"),
             hjust = -0.05, vjust = -0.5, size = 3.5, colour = "grey40") +
    annotate("text",
             x = x_max, y = tail(data_clip$mean * sys_fcst, 1),
             label = paste0(curr_yr, " forecast"),
             hjust = -0.05, vjust = -0.5, size = 3.5, colour = "#4A7BA7") +
    annotate("text",
             x     = max(curr_esc$julian) |> (\(j) as.Date(j, origin = orig_date))(),
             y     = max(curr_esc$cum_adj_adults, na.rm = TRUE),
             label = as.character(curr_yr),
             hjust = -0.05, vjust = 0.5, size = 3.5, colour = "#1B7837") +
    scale_y_continuous(
      labels = scales::comma,
      name   = paste0(sys, " Cumulative Escapement"),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_date(
      breaks      = seq(x_min, x_max, by = "2 weeks"),
      date_labels = "%d %b"
    ) +
    guides(colour = "none") +
    coord_cartesian(xlim = c(x_min, x_max), clip = "off") +
    labs(x = "") +
    theme_classic() +
    theme(
      panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.4),
      plot.margin        = margin(t = 5, r = 100, b = 5, l = 5, unit = "pt"),
      axis.line          = element_blank(),
      axis.ticks         = element_blank(),
      axis.text          = element_text(colour = "grey30", size = 11),
      axis.title.y       = element_text(colour = "grey30", size = 12)
    )
}
# Build timing plots -----------------------------------------------------
timing_plots <- purrr::set_names(unique(escday$system)) |>
  map(\(.x) escday |>
        filter(
          year   >= curr_yr - 20,
          year   <= curr_yr - 1,
          system == .x
        ) |>
        group_by(julian) |>
        summarise(
          mean = mean(cum_prop_adult, na.rm = TRUE),
          l90  = quantile(cum_prop_adult, 0.05, na.rm = TRUE),
          u90  = quantile(cum_prop_adult, 0.95, na.rm = TRUE)
        )
  ) |>
  imap(\(.x, .y) esc_p1(.x, .y, curr_yr))

# Save plots to network folder -------------------------------------------
timing_plots |>
  iwalk(
    \(.x, .y) ggsave(
      plot     = .x,
      filename = paste0(
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/",
        curr_yr, "_MGT/Escapement plots/",
        curr_yr, "_Sk_cum-esc-timing_", .y, ".png"
      ),
      height = 4.5,
      width  = 9,
      units  = "in"
    )
  )


#Less frequently used Plots 

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

## 
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



