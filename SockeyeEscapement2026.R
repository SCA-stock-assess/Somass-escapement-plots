# Overhaul of the old escapement file and R code. Starting in 2026, Sockeye,
# Chinook, and Coho historical data are consolidated into SomassEsc.xlsx and
# read from there (replacing the old stampfall and escday sources). During the
#  season, the daily age-composition file provided by Graham is used as
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
  select(1:7) |>
  filter(!is.na(system), !is.na(year)) |>
  rename(
    adj_adults = "Adjusted net Adult up count",
    adj_jacks  = "Adjusted net Jack up count"
  ) |>
  mutate(
    year = if_else(year < 2000, as.numeric(paste0(19, year)), year),
    date = as.Date(paste(year, month, day, sep = "-")),
    # FIXED: was `format(date, "%j") |> as.numeric()`. That returns
    # day-of-year using EACH RECORD'S OWN calendar year, so any date after
    # Feb 29 in a leap year is numbered one higher than the identical
    # calendar date in a non-leap year. Since timing_plots and get_blue_line_value
    # both group/match on raw julian across curr_yr-20:curr_yr-1 (which
    # includes ~5 leap years in any 20-year window), this misaligned the
    # historical mean/ribbon by up to a day for every date after Feb 29.
    # Anchoring to a fixed non-leap year (2001) makes julian a pure function
    # of month-day, so it's comparable across years regardless of leap status.

    julian = yday(as.Date(paste0("2001-", format(date, "%m-%d")))),
    
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
    # FIXED: was `yday(date)` — same raw-julian problem as escday above.
    # This didn't bite in 2026 specifically (2026 isn't leap, so yday()
    # happens to match the anchor method for this year's own dates), but it
    # matters the moment this pipeline is reused in a leap year, and it's
    # inconsistent with the anchor method now used in escday — you want both
    # tables computing julian the same way so they're comparable.
    julian = yday(as.Date(paste0("2001-", format(date, "%m-%d"))))
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
som_esc <- 383333  # Updated to a 900 K run
#som_esc <- 391667  # Updated to a 950 K run


#split changed on July 2
# Current year escapement forecasts per system
esc_fcst <- data.frame(
  system = c("Sproat Lake", "Great Central Lake"),
  fcst   = c(som_esc * 0.25, som_esc * 0.75)
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
  
  curr_esc_clip <- filter(curr_esc,
                          as.Date(julian, origin = orig_date) >= x_min,
                          as.Date(julian, origin = orig_date) <= x_max)
  
  # blue_max: reverted to fixed system forecast total per request —
  # NOT derived from the curve, so it stays pinned even if the mean
  # curve doesn't fully resolve to 1.0 by x_max.
  blue_max  <- sys_fcst
  
  grey_max  <- suppressWarnings(max(data_clip$mean * hist_fcst, na.rm = TRUE))
  if (!is.finite(grey_max)) grey_max <- NA_real_
  
  green_max <- suppressWarnings(max(curr_esc_clip$cum_adj_adults, na.rm = TRUE))
  if (!is.finite(green_max)) green_max <- NA_real_
  
  # --- TEMPORARY DEBUG: remove once grey_max is confirmed correct ---
  message(sprintf(
    "[esc_p1 debug] sys=%s | hist_fcst=%s | max(data_clip$mean)=%s | grey_max=%s | green_max=%s | n_data_clip_rows=%d",
    sys, hist_fcst, max(data_clip$mean, na.rm = TRUE), grey_max, green_max, nrow(data_clip)
  ))
  # -------------------------------------------------------------------
  
  break_vals <- c(blue_max = blue_max, grey_max = grey_max, green_max = green_max)
  break_cols <- c(blue_max = "#4A7BA7", grey_max = "grey50", green_max = "#1B7837")
  
  keep <- !is.na(break_vals)
  y_breaks        <- sort(break_vals[keep])
  y_break_colours <- break_cols[keep][order(break_vals[keep])]
  
  ggplot(data_clip, aes(as.Date(julian, origin = orig_date), mean * hist_fcst)) +
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$lwr,
      lty = 2, linewidth = 0.8, colour = "#C0392B"
    ) +
    geom_hline(
      yintercept = filter(ref_pts, system == sys)$upr,
      lty = 2, linewidth = 0.8, colour = "#E8A020"
    ) +
    geom_hline(
      yintercept = y_breaks,
      colour = y_break_colours,
      linewidth = 0.5, alpha = 0.5
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
      data      = curr_esc_clip,
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
             x     = max(curr_esc_clip$julian) |> (\(j) as.Date(j, origin = orig_date))(),
             y     = max(curr_esc_clip$cum_adj_adults, na.rm = TRUE),
             label = as.character(curr_yr),
             hjust = -0.05, vjust = 0.5, size = 3.5, colour = "#1B7837") +
    scale_y_continuous(
      labels = scales::comma,
      breaks = y_breaks,
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
      panel.grid.major.y = element_blank(),
      plot.margin        = margin(t = 5, r = 100, b = 5, l = 5, unit = "pt"),
      axis.line          = element_blank(),
      axis.ticks         = element_blank(),
      axis.text.y        = element_text(colour = y_break_colours, size = 11),
      axis.text.x        = element_text(colour = "grey30", size = 11),
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
######################
# Expected escapement by a specific date / How are we tracking?
######################


get_blue_line_value <- function(sys, target_date, curr_yr) {
  
  sys_fcst <- filter(esc_fcst, system == sys)$fcst
  # FIXED: was `yday(as.Date(target_date))` — must match the anchor method
  # now used to build julian in escday, or the lookup below silently mismatches.
  target_julian <- yday(as.Date(paste0("2001-", format(as.Date(target_date), "%m-%d"))))
  
  hist_curve <- escday |>
    filter(
      year >= curr_yr - 20,
      year <= curr_yr - 1,
      system == sys
    ) |>
    group_by(julian) |>
    summarise(mean = mean(cum_prop_adult, na.rm = TRUE), .groups = "drop")
  
  mean_prop <- hist_curve |>
    filter(julian == target_julian) |>
    pull(mean)
  
  if (length(mean_prop) == 0) {
    # no exact match on that julian day — interpolate
    mean_prop <- approx(hist_curve$julian, hist_curve$mean, xout = target_julian)$y
  }
  
  tibble(
    system     = sys,
    date       = as.Date(target_date),
    mean_prop  = mean_prop,
    sys_fcst   = sys_fcst,
    CurrentYearExpected = mean_prop * sys_fcst
  )
}

# Quick spot-checks
get_blue_line_value("Sproat Lake", "2026-07-05", curr_yr)
get_blue_line_value("Great Central Lake", "2026-07-05", curr_yr)

# Full season export, both systems, with actuals for tracking
blue_line_export <- expand_grid(
  system = c("Sproat Lake", "Great Central Lake"),
  date   = seq(as.Date(paste0(curr_yr, "-05-25")),
               as.Date(paste0(curr_yr, "-10-30")),
               by = "day")
) |>
  pmap(\(system, date) get_blue_line_value(system, date, curr_yr)) |>
  list_rbind() |>
  left_join(
    current_year_esc |> select(system, date, actual_cum_esc = cum_adj_adults),
    by = c("system", "date")
  ) |>
  mutate(
    diff           = actual_cum_esc - CurrentYearExpected,
    pct_of_expected = actual_cum_esc / CurrentYearExpected
  )

blue_line_export <- blue_line_export |>
  mutate(across(c(mean_prop, sys_fcst, CurrentYearExpected, actual_cum_esc, diff, pct_of_expected), \(x) round(x, 4)))
write_csv(blue_line_export, paste0(curr_yr, "_Sk_blue-line-tracking_", Sys.Date(), ".csv"))

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
rp %+% filter(escday_trim, year <= max(year)) +
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
    filter(between(year, max(year) - 31, max(year))) %>% 
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

 


# ---------------------------------------------------------------------------
# One base pale->dark colour ramp per system. Edit hex codes / add systems
# as needed -- pale end = early runs, dark end = late runs.
# ---------------------------------------------------------------------------
library(colorspace)
library(scales)

system_palettes <- list(
  "Great Central Lake" = c("#cde5f7", "#08306b"),  # pale -> dark blue
  "Sproat Lake"        = c("#fde0c5", "#8c2d04")   # pale -> dark orange
)

get_line_colour <- function(system, tt95_scaled) {
  if (is.na(tt95_scaled)) return("grey70")
  
  pal_ends <- system_palettes[[system]]
  if (is.null(pal_ends)) {
    stop("No palette defined for system: '", system, "' — check system_palettes names match escday$system exactly")
  }
  
  pal <- colorRampPalette(pal_ends)(101)
  pal[round(tt95_scaled * 100) + 1]
}

# ---------------------------------------------------------------------------
# Sockeye run-timing plot: cumulative escapement by system and year
#
# For each system/year, plots the cumulative proportion of adult escapement
# (cum_prop_adult) against day of year. By default restricted to the most
# recent 7 years (max(year) - 6 to max(year)) — adjust the range in the
# filter(between(year, ...)) call below to show more or fewer years.
#
# Each panel is annotated with:
#   - a horizontal reference line at 50% cumulative escapement
#   - a vertical line marking the julian day that system/year first crossed
#     50% cumulative escapement (tt50)
#   - the run-timing line itself, coloured using a system-specific palette:
#     each system has its own base hue, shaded pale-to-dark according to how
#     early or late that year's 95%-completion date (tt95) fell relative to
#     that system's own 7-year range
#
# Years/systems that have not yet reached 50% or 95% (e.g. an in-progress
# current season) are assigned NA for tt50/tt95 rather than a misleading
# early value, and are rendered in neutral grey with no reference line.
#
# Drawn directly from escday (no upstream joins needed).
# ---------------------------------------------------------------------------

tt50_df <- escday |> 
  filter(between(year, max(year) - 14, max(year))) |> 
  group_by(system, year) |> 
  summarise(
    tt50 = if (any(cum_prop_adult > 0.5, na.rm = TRUE)) {
      min(julian[cum_prop_adult > 0.5], na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) |> 
  mutate(tt50_date = as.Date(tt50, origin = "2020-12-31"))

plot_df <- escday |> 
  filter(between(year, max(year) - 14, max(year))) |> 
  group_by(system, year) |> 
  mutate(
    tt95 = if (any(cum_prop_adult > 0.95, na.rm = TRUE)) {
      min(julian[cum_prop_adult > 0.95], na.rm = TRUE)
    } else {
      NA_real_
    }
  ) |> 
  ungroup() |> 
  group_by(system) |> 
  mutate(tt95_scaled = scales::rescale(tt95, to = c(0, 1))) |> 
  ungroup() |> 
  mutate(line_colour = purrr::map2_chr(system, tt95_scaled, get_line_colour))

ggplot(plot_df, aes(as.Date(julian, origin = "2020-12-31"), cum_prop_adult)) +
  facet_grid(system ~ year) +
  geom_hline(yintercept = 0.5, colour = "grey60", linewidth = 0.4) +
  geom_vline(
    data = tt50_df,
    aes(xintercept = tt50_date),
    colour = "grey60",
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  geom_line(aes(colour = line_colour, group = year), linewidth = 1) +
  scale_colour_identity() +
  coord_cartesian(xlim = as.Date(c("2021-05-25", "2021-10-15"))) +
  scale_y_continuous(
    breaks = c(0.25, 0.5, 0.75),
    labels = c("25%", "50%", "75%"),
    name = "Proportion of total escapement",
    expand = expansion(mult = 0.02)
  ) +
  labs(x = NULL) +
  theme(
    axis.ticks.y = element_blank(),
    panel.spacing.y = unit(0.2, "lines"),
    panel.spacing.x = unit(0.05, "lines"),
    panel.grid = element_blank(),
   # aspect.ratio = 0.9
  )
#all years condensed to single plot for each system

year_levels <- sort(unique(plot_df$year))
year_shades <- colorRampPalette(c("#c6dbef", "#08306b"))(length(year_levels))
names(year_shades) <- year_levels

avg_df <- plot_df |>
  group_by(system, julian) |>
  summarise(mean_prop = mean(cum_prop_adult, na.rm = TRUE), .groups = "drop") |>
  mutate(date = as.Date(julian, origin = "2020-12-31"))

avg_tt50 <- avg_df |>
  group_by(system) |>
  summarise(
    tt50 = if (any(mean_prop > 0.5, na.rm = TRUE)) {
      min(julian[mean_prop > 0.5], na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) |>
  mutate(tt50_date = as.Date(tt50, origin = "2020-12-31"))

ggplot(plot_df, aes(as.Date(julian, origin = "2020-12-31"), cum_prop_adult,
                    group = year, colour = factor(year))) +
  facet_wrap(~system, ncol = 1, axes = "all_x") +
  geom_hline(yintercept = 0.5, colour = "grey50", linetype = 2, linewidth = 0.4) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geomtextpath::geom_textline(
    data = avg_df, aes(x = date, y = mean_prop, label = "avg"),
    inherit.aes = FALSE, colour = "firebrick", linewidth = 1.3,
    hjust = 0.95, size = 3.2, fontface = "bold"
  ) +
  geom_vline(
    data = avg_tt50, aes(xintercept = tt50_date),
    inherit.aes = FALSE, colour = "firebrick", linetype = 3, linewidth = 0.6, na.rm = TRUE
  ) +
  geom_point(
    data = avg_tt50, aes(x = tt50_date, y = 0.5),
    inherit.aes = FALSE, colour = "firebrick", size = 2.6, na.rm = TRUE
  ) +
  geom_label(
    data = avg_tt50,
    aes(x = tt50_date, y = 0.5, label = format(tt50_date, "%b %d")),
    inherit.aes = FALSE, colour = "firebrick", fill = "white", size = 3,
    nudge_x = 3, nudge_y = 0.07, label.size = 0.3, na.rm = TRUE
  ) +
  scale_colour_manual(values = year_shades, name = "Year") +
  coord_cartesian(xlim = as.Date(c("2021-05-25", "2021-10-15"))) +
  scale_x_date(date_breaks = "15 days", date_labels = "%b %d") +
  scale_y_continuous(
    breaks = c(0.25, 0.5, 0.75), labels = c("25%", "50%", "75%"),
    name = "Proportion of total escapement", expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL) +
  theme_classic() +
  theme(
    panel.grid       = element_blank(),
    panel.spacing.y  = unit(1.2, "lines"),
    strip.background = element_rect(fill = "grey15", colour = NA),
    strip.text       = element_text(colour = "white", face = "bold", size = 12,
                                    margin = margin(6, 6, 6, 6)),
    legend.title     = element_text(face = "bold"),
    legend.position  = "right"
  )
