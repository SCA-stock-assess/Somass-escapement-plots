library(readxl)
library(tidyverse)
library(ggrepel)   # direct year labels on the spaghetti plot (geomtextpath was loaded but never used)

# ---- Settings ---------------------------------------------------------------

curr_year    <- 2026   # current management year; drives file paths and labels
n_hist_years <- 10     # prior years used in every plot (was 10 / 10 / 9)
esc_target   <- 34000  # in-season escapement target

ANCHOR_YEAR <- 2001    # fixed non-leap year every date is mapped onto for plotting
PLOT_WINDOW <- as.Date(paste0(ANCHOR_YEAR, c("-08-01", "-11-05")))
DATE_BREAKS <- seq(PLOT_WINDOW[1], PLOT_WINDOW[2], by = "2 weeks")  # same ticks on every plot

# Shared palette
hist_colour   <- "#2C5F7C"  # muted steel blue: historic mean / band / recent years
hist_light    <- "#C3D0D9"  # oldest historic year on the spaghetti plot
curr_colour   <- "#EE6C29"  # current year, every plot
target_colour <- "#2E8B57"  # target line

# ---- Shared helpers ---------------------------------------------------------

# Map any date onto ANCHOR_YEAR so Aug-Nov dates line up across leap and
# non-leap years. Replaces safe_julian() + julian_to_date(): the plots only
# ever needed the anchored date, never the julian number itself.
anchor_date <- function(date) make_date(ANCHOR_YEAR, month(date), mday(date))

# Read one escapement sheet into long format with cumulative counts.
# Blank cells are treated as 0 fish up to the last day with any entry for
# that year/species, and as "not counted yet" (NA) after it. That stops the
# current-year line, and its callout, from running flat to the end of a
# sheet that already has future dates in it.
read_esc <- function(path, sheet, species_cols) {
  esc <- read_xlsx(path, sheet = sheet, na = "") |>
    select(1:9) |>
    pivot_longer({{ species_cols }}, names_to = "species", values_to = "count") |>
    rename_with(tolower) |>
    filter(!is.na(date)) |>              # drops subtotal rows, if present
    mutate(date = as.Date(date))
  if (!"year" %in% names(esc)) esc$year <- year(esc$date)  # live sheet has no Year column
  
  esc |>
    group_by(year, species) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      counted   = rev(cumany(rev(!is.na(count)))),
      cum_count = if_else(counted, cumsum(replace_na(count, 0)), NA_real_),
      ann_ttl   = sum(count, na.rm = TRUE),
      cum_prop  = cum_count / ann_ttl,
      plot_date = anchor_date(date)
    ) |>
    ungroup() |>
    select(-counted)
}

# Historic Chinook: one row per year per day up to the window end, so every
# day's mean/quantiles use the same set of years and every spaghetti line
# reaches the window edge. Cumulative values are 0 before a year's first
# record and hold at their last value after it.
hist_chinook <- function(hist_data, curr_year, n_years) {
  hist <- hist_data |>
    filter(species == "Chinook", between(year, curr_year - n_years, curr_year - 1))
  
  zero_years <- unique(hist$year[hist$ann_ttl == 0])
  if (length(zero_years) > 0) {
    warning("Excluding year(s) with zero total Chinook: ", paste(zero_years, collapse = ", "))
  }
  
  hist |>
    filter(ann_ttl > 0, plot_date <= PLOT_WINDOW[2]) |>
    select(year, plot_date, cum_count, cum_prop) |>
    complete(year, plot_date = seq(min(plot_date), PLOT_WINDOW[2], by = "day")) |>
    group_by(year) |>
    arrange(plot_date, .by_group = TRUE) |>
    fill(cum_count, cum_prop, .direction = "down") |>
    mutate(across(c(cum_count, cum_prop), \(x) replace_na(x, 0))) |>
    ungroup()
}

curr_chinook <- function(current_data) {
  out <- filter(current_data, species == "Chinook", !is.na(cum_count))
  if (nrow(out) == 0) {
    warning("No Chinook rows in current_data -- check unique(current_data$species).")
  }
  out
}

# Dot + filled label at a point; used for every callout
callout <- function(data, y, label, fill, hjust = 0, vjust = 0.5, nudge_x = 2) {
  list(
    geom_point(data = data, aes(plot_date, {{ y }}), colour = fill, size = 3),
    geom_label(
      data = data, aes(plot_date, {{ y }}, label = {{ label }}),
      colour = "white", fill = fill, fontface = "bold", size = 3,
      label.padding = unit(0.15, "lines"), label.r = unit(0.1, "lines"),
      hjust = hjust, vjust = vjust, nudge_x = nudge_x
    )
  )
}

# Callout for the current year's latest count. Label goes right of the dot,
# or left of it in the last 3 weeks of the window so it stays off the axis.
curr_callout <- function(latest, label, vjust = 0) {
  near_end <- nrow(latest) > 0 && latest$plot_date > PLOT_WINDOW[2] - 21
  callout(latest, cum_count, {{ label }}, curr_colour, vjust = vjust,
          hjust = if (near_end) 1 else 0, nudge_x = if (near_end) -2 else 2)
}

# Legend inside the top-left of the panel (numeric legend.position is
# deprecated from ggplot2 3.5.0)
legend_top_left <- function() {
  if (packageVersion("ggplot2") >= "3.5.0") {
    theme(legend.position = "inside", legend.position.inside = c(0.02, 0.98),
          legend.justification = c(0, 1))
  } else {
    theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1))
  }
}

cn_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
      axis.line          = element_line(colour = "black", linewidth = 0.4),
      axis.ticks.x       = element_line(colour = "black", linewidth = 0.4),
      axis.ticks.length.x = unit(0.15, "cm"),
      axis.title.y.right = element_text(margin = margin(l = 0.5, unit = "lines")),
      axis.title.y.left  = element_text(margin = margin(r = 0.5, unit = "lines")),
      legend.background  = element_rect(fill = alpha("white", 0.7), colour = NA),
      legend.key         = element_rect(fill = NA, colour = NA),
      legend.spacing.y   = unit(0, "lines"),
      legend.margin      = margin(0, 4, 0, 4)
    )
}

# ---- Data loading -----------------------------------------------------------

data_dir <- "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/Sockeye/SOMASS/"

SomassEsc <- read_esc(
  paste0(data_dir, "Data/ESCAPEMENT_PROGRAM/SomassEsc.xlsx"),
  sheet = "Stamp CN & CO", species_cols = Coho:ChinookJack
)

CurrentYearEsc <- read_esc(
  paste0(data_dir, "SOCKEYE_MGMT/", curr_year, "_MGT/Daily Totals by Age ", curr_year, ".xlsx"),
  sheet = "Stamp CN&CO", species_cols = 2:9
)

# ---- Timing / count plot ----------------------------------------------------

#' Stamp River Chinook cumulative escapement vs historic
#'
#' esc_target = NULL (post-season): historic mean and 5th-95th percentile of
#'   daily cumulative COUNTS, current year on the same scale.
#' esc_target = number (in-season): historic run TIMING (each year's
#'   cumulative proportion of its own total) multiplied by the target, i.e.
#'   where the count would be on each date with that year's timing and a run
#'   that finishes exactly on target. Adds the target line and a left axis in
#'   % of target. By construction the mean and band converge on 100% of target
#'   at the end of the window, so the band narrows to zero width there.
build_cn_count_plot <- function(hist_data, current_data, curr_year,
                                n_years = n_hist_years, esc_target = NULL) {
  use_target <- !is.null(esc_target)
  
  hist_summary <- hist_chinook(hist_data, curr_year, n_years) |>
    mutate(value = if (use_target) cum_prop * esc_target else cum_count) |>
    group_by(plot_date) |>
    summarise(mean = mean(value), q05 = quantile(value, 0.05),
              q95 = quantile(value, 0.95), .groups = "drop") |>
    filter(plot_date >= PLOT_WINDOW[1])
  
  current <- curr_chinook(current_data)
  latest  <- slice_max(current, plot_date, n = 1, with_ties = FALSE)
  # In-season: historic mean on the same date as the latest count (like-for-like).
  # Post-season: historic mean at the end of its line, i.e. the mean final total.
  hist_pt <- if (use_target) filter(hist_summary, plot_date %in% latest$plot_date)
  else slice_max(hist_summary, plot_date, n = 1)
  
  # Callout text: count, plus % of target in-season
  fmt <- function(x) {
    if (use_target) paste0(scales::comma(round(x)), " (", scales::percent(x / esc_target), ")")
    else scales::comma(round(x))
  }
  # In-season: higher value's label above its point, lower one below, so they don't collide
  curr_on_top <- nrow(hist_pt) == 0 || latest$cum_count >= hist_pt$mean
  hist_callout <- if (use_target) {
    # Same date as the latest count, so the label goes directly under (or over)
    # the dot rather than to the right, where it would sit on the rest of the line
    callout(hist_pt, mean, fmt(mean), hist_colour, hjust = 0.5, nudge_x = 0,
            vjust = if (curr_on_top) 1.8 else -0.8)
  } else {
    # End of the line: label sits just above the point, right-aligned to it
    callout(hist_pt, mean, fmt(mean), hist_colour, hjust = 1, vjust = -0.9, nudge_x = 0)
  }
  
  hist_span  <- paste0(curr_year - n_years, "\u2013", curr_year - 1)
  curr_lab   <- as.character(curr_year)
  mean_lab   <- paste0("Historic mean", if (use_target) " timing", " (", hist_span, ")",
                       if (use_target) ", scaled to target")
  band_lab   <- paste0("Historic 5th\u201395th percentile", if (use_target) " timing")
  target_lab <- if (use_target) paste0("Escapement target (", scales::comma(esc_target), ")")
  
  ggplot() +
    geom_ribbon(data = hist_summary, aes(plot_date, ymin = q05, ymax = q95, fill = band_lab),
                alpha = 0.15) +
    (if (use_target) geom_hline(aes(yintercept = esc_target, linetype = target_lab),
                                colour = target_colour, linewidth = 0.5)) +
    geom_line(data = hist_summary, aes(plot_date, mean, colour = mean_lab), linewidth = 1) +
    geom_line(data = current, aes(plot_date, cum_count, colour = curr_lab), linewidth = 1.6) +
    curr_callout(latest, fmt(cum_count), vjust = if (curr_on_top) -0.4 else 1.4) +
    hist_callout +
    scale_colour_manual(values = setNames(c(curr_colour, hist_colour), c(curr_lab, mean_lab)),
                        breaks = c(curr_lab, mean_lab), name = NULL) +
    scale_fill_manual(values = setNames(hist_colour, band_lab), name = NULL) +
    (if (use_target) scale_linetype_manual(values = setNames("dashed", target_lab), name = NULL)) +
    guides(
      colour   = guide_legend(order = 1, override.aes = list(linewidth = c(1.6, 1))),
      fill     = guide_legend(order = 2),
      linetype = guide_legend(order = 3)
    ) +
    scale_y_continuous(
      name = "Cumulative Stamp Falls Chinook escapement",
      position = "right",
      labels = scales::comma,
      # In-season: count breaks at 25% steps of target so both axes share the gridlines
      breaks = if (use_target) seq(0, 5, 0.25) * esc_target else waiver(),
      expand = expansion(mult = c(0, 0.05)),
      sec.axis = if (use_target) {
        sec_axis(~ . / esc_target, breaks = seq(0, 5, 0.25),
                 labels = scales::percent, name = "% of escapement target")
      } else waiver()
    ) +
    (if (use_target) expand_limits(y = esc_target * 1.3)) +  # headroom so legend clears target line
    scale_x_date(breaks = DATE_BREAKS, date_labels = "%d %b") +
    coord_cartesian(xlim = PLOT_WINDOW) +
    labs(x = NULL) +
    cn_theme() +
    legend_top_left()
}

# ---- Spaghetti plot ---------------------------------------------------------

#' Stamp River Chinook cumulative count, one line per year
#'
#' Historic years are thin lines shaded from light (oldest) to steel blue
#' (most recent) and labelled directly at the right edge, so no legend is
#' needed. The current year is the thick orange line with its latest count.
build_cn_spaghetti_plot <- function(hist_data, current_data, curr_year,
                                    n_years = n_hist_years) {
  hist    <- hist_chinook(hist_data, curr_year, n_years) |>
    filter(plot_date >= PLOT_WINDOW[1])
  ends    <- filter(hist, plot_date == PLOT_WINDOW[2])
  current <- curr_chinook(current_data)
  latest  <- slice_max(current, plot_date, n = 1, with_ties = FALSE)
  
  ggplot(hist, aes(plot_date, cum_count)) +
    geom_line(aes(group = year, colour = year), linewidth = 0.6) +
    geom_text_repel(
      data = ends, aes(label = year, colour = year),
      size = 3, hjust = 0, nudge_x = 1.5, direction = "y",
      xlim = c(PLOT_WINDOW[2] + 1, NA), segment.colour = "grey75",
      segment.size = 0.3, min.segment.length = 0, box.padding = 0.1, seed = 1
    ) +
    geom_line(data = current, colour = curr_colour, linewidth = 1.6) +
    curr_callout(latest, paste0(curr_year, ": ", scales::comma(cum_count)), vjust = -0.4) +
    scale_colour_gradient(low = hist_light, high = hist_colour, guide = "none") +
    scale_x_date(breaks = DATE_BREAKS, date_labels = "%d %b",
                 expand = expansion(add = c(2, 9))) +   # room for the year labels
    scale_y_continuous(
      name = "Cumulative Stamp Falls Chinook escapement",
      position = "right", labels = scales::comma,
      expand = expansion(mult = c(0, 0.05))
    ) +
    coord_cartesian(xlim = PLOT_WINDOW, clip = "off") +
    labs(x = NULL) +
    cn_theme()
}

# ---- Build and save ---------------------------------------------------------

ChinookTimingPlot <- build_cn_count_plot(SomassEsc, CurrentYearEsc, curr_year,
                                         esc_target = esc_target)
ChinookTimingPlot_noTarget <- build_cn_count_plot(SomassEsc, CurrentYearEsc, curr_year)
ChinookSpaghettiPlot <- build_cn_spaghetti_plot(SomassEsc, CurrentYearEsc, curr_year)

ggsave(
  plot = ChinookSpaghettiPlot,
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year, "/A23/Escapement plot/", curr_year, "_ChinookSpaghettiPlot.png"
  ),
  height = 5.5, width = 9, units = "in"
)
