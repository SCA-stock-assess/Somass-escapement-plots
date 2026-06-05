
#The old version that I edited so it can be used moving forward this year
#2026
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(ggridges)
library(geomtextpath)

# Enter the current analysis year
curr_year <- 2026

# Load historical and current Sockeye escapement data -----------------------------


# Escday data
escday <- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca//PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/Data/ESCAPEMENT_PROGRAM/Escday.xlsx",
  sheet = "Data",
  skip = 3,
  na = ""
) |> 
  filter(!is.na(system), !is.na(year)) |>  # drop blank trailing rows
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

# Update current Somass escapement target
som_esc <- 382500 #Updated for 2026 (May 3, 2026)

# Forecasts for current year escapement
esc_fcst <- data.frame(
  system = unique(escday$system),
  fcst = c(som_esc*0.20, som_esc*0.8) # Sproat, then GCL Forecast
)

# Biological reference points Updated for 2026 based on Nick's CSAS paper
ref_pts <- data.frame(
  system = unique(escday$system),
  lwr = c(15220, 30887), # Sproat, Stamp
  upr = c(68316, 92229) # Sproat, Stamp
)


# Function to plot the curves
esc_p1 <- function(data, sys, curr_yr) {
  
  sys_fcst  <- filter(esc_fcst, system == sys)$fcst
  stopifnot(length(sys_fcst) == 1)
  
  orig_date <- as.Date(paste0(curr_yr - 1, "-12-31"))
  
  ggplot(
    data,
    aes(as.Date(julian, origin = orig_date), mean)
  ) +
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$lwr / sys_fcst,
      lty        = 2,
      linewidth  = 0.8,
      colour     = "#C0392B"
    ) +
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$upr / sys_fcst,
      lty        = 2,
      linewidth  = 0.8,
      colour     = "#E8A020"
    ) +
    geom_vline(
      xintercept = seq(
        as.Date(paste0(curr_yr, "-05-25")),
        as.Date(paste0(curr_yr, "-10-15")),
        by = "2 weeks"
      ),
      colour    = "grey80",
      linewidth = 0.4,
      alpha     = 0.6
    ) + 
    geom_ribbon(
      aes(ymin = l90, ymax = u90),
      fill  = "#6B93B5",
      alpha = 0.25
    ) +
    geom_textline(
      label     = paste(curr_yr - 20, "to", curr_yr - 1),
      linewidth = 1,
      hjust     = 0.6,
      colour    = "#4A7BA7"
    ) +
    geom_textline(
      data = filter(escday, system == sys, year == curr_yr),
      aes(
        x = as.Date(julian, origin = orig_date),
        y = cum_adj_adults / sys_fcst
      ),
      label          = as.character(curr_yr),
      text_smoothing = 30,
      colour         = "black",
      hjust          = 0.90,
      vjust          = -0.25,
      linewidth      = 1
    ) +
    scale_y_continuous(
      labels   = scales::percent,
      name     = "Proportion of total escapement",
      sec.axis = sec_axis(
        trans  = ~ . * sys_fcst,
        labels = scales::comma,
        name   = paste(curr_yr, "cumulative escapement")
      ),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_date(
      breaks       = seq(
        as.Date(paste0(curr_yr, "-05-25")),
        as.Date(paste0(curr_yr, "-10-15")),
        by = "2 weeks"
      ),
      date_labels  = "%d %b"
    ) +
    guides(colour = "none") +
    coord_cartesian(
      xlim = as.Date(paste0(curr_yr, c("-05-25", "-10-15")))
    ) +
    labs(x = NULL) +
    theme_classic() +
    theme(
      legend.position    = c(0.8, 0.3),
      plot.tag           = element_text(colour = "grey35"),
      plot.tag.position  = c(0.22, 0.95),
      legend.background  = element_blank(),
      legend.key         = element_blank(),
      axis.line          = element_blank(),
      axis.ticks         = element_blank(),
      axis.text          = element_text(colour = "grey65", size = 11),
      axis.title.y       = element_text(colour = "grey65", size = 12),
      axis.title.y.right = element_text(colour = "grey65", size = 12)
    )
}

# Curves with historical average proportions
curr_yr <- max(escday$year)

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

# Save cumulative current versus historical plots to network folder -------


# Save to current year management folder
timing_plots |> 
  iwalk(
    ~ggsave(
      plot = .x, 
      filename = paste0(
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/",
        curr_year,
        "_MGT/Escapement plots/",
        "2026_Sk_cum-esc-timing_",
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


