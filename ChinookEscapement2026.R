library(readxl)
library(tidyverse)
library(geomtextpath)

# ---- Shared helpers -------------------------------------------------------

ANCHOR_YEAR <- 2001  # fixed non-leap anchor used across all timing plots
curr_year   <- 2026  # current management year; drives file paths and plot calls below

# Leap-year-safe day-of-year: anchors every date to a fixed non-leap year
# before extracting day-of-year, so Aug-Oct dates align consistently across
# leap and non-leap calendar years. Replaces format(date, "%j").
safe_julian <- function(date) {
  lubridate::yday(as.Date(paste0(ANCHOR_YEAR, "-", format(date, "%m-%d"))))
}

# Converts a leap-safe julian day back to a plottable Date, anchored to the
# same fixed year used by safe_julian(). Shared by every timing plot so the
# x-axis is always built the same way.
julian_to_date <- function(j) as.Date(j - 1, origin = paste0(ANCHOR_YEAR, "-01-01"))

# Bounded [0,1] smoother used for the l95/u95 ribbon and the historic mean
# label line. Not a real binomial model (l95/u95 are proportions, not
# Bernoulli outcomes) -- used purely as a logit-constrained smoothing curve.
# The "non-integer #successes" warning this throws is expected and harmless
# for that reason; it's a signal you're using glm() outside its intended
# purpose, not a computation error.
logit_smooth <- function(y, x) {
  binomial()$linkinv(predict(glm(y ~ x, family = binomial)))
}

# Shared palette across all Stamp Chinook timing plots
hist_colour_dark <- "#2C5F7C"  # muted steel blue (historic mean line / ribbon)
curr_colour      <- "#EE6c29"  # current-year highlight, used across all timing/spaghetti plots
target_colour    <- "#2E8B57"  # (target line)

# Distinct, non-green palette for historic spaghetti-plot years. Green is
# deliberately excluded here so curr_colour above always reads unambiguously
# as "this year" against any number of historic lines.
hist_year_palette <- c(
  "#2E8B57", "#566238", "#FFD95D", "#6D8EC5", "#9C755F",
  "#162660", "#CF8852", "#6F6134", "#C9C769", "#F0C845"
)

# Shared theme so every Stamp Chinook plot looks consistent
cn_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
      axis.line.y = element_line(colour = "black", linewidth = 0.4),
      axis.line.x = element_line(colour = "black", linewidth = 0.4),
      axis.ticks.x = element_line(colour = "black", linewidth = 0.4),
      axis.ticks.length.x = unit(0.15, "cm")
    )
}

# ---- Data loading -----------------------------------------------------

SomassEsc <- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/Sockeye/SOMASS/Data/ESCAPEMENT_PROGRAM/SomassEsc.xlsx",
  sheet = "Stamp CN & CO",
  na = ""
) |>
  select(1:9) |>
  pivot_longer(cols = Coho:ChinookJack, names_to = "species", values_to = "count") |>
  rename_with(tolower) |>
  mutate(
    date = as.Date(date),
    # Historic (complete) years: missing daily counts mean "no fish that
    # day", i.e. 0 -- not unknown. Leaves the current year's NAs alone
    # (handled separately via CurrentYearEsc / the live file).
    count = if_else(year < max(year) & is.na(count), 0, count)
  ) |>
  group_by(year, species) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    cum_count = cumsum(count),
    ann_ttl = sum(count, na.rm = TRUE),
    cum_prop = cum_count / ann_ttl,
    julian = safe_julian(date)
  ) |>
  ungroup()

CurrentYearEsc <- read_xlsx(
  "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/2026_MGT/Daily Totals by Age 2026.xlsx",
  sheet = "Stamp CN&CO",
  na = ""
) |>
  select(1:9) |>
  pivot_longer(cols = 2:9, names_to = "species", values_to = "count") |>
  rename_with(tolower) |>
  filter(!is.na(date)) |>  # drop subtotal rows at end of sheet, if present
  mutate(date = as.Date(date)) |>
  group_by(species) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    # Blank/not-yet-entered days in the live file must read as 0, not NA --
    # cumsum() propagates NA forward, which would otherwise wipe out every
    # later day's cumulative count (see the same pattern in the Coho scripts).
    cum_count = cumsum(replace_na(count, 0)),
    ann_ttl = sum(count, na.rm = TRUE),
    cum_prop = cum_count / ann_ttl,
    julian = safe_julian(date)
  ) |>
  ungroup()

# ---- Timing (proportion) plot ------------------------------------------

#' Build Stamp River Chinook escapement timing plot
#'
#' @param hist_data    Historic data frame (e.g. SomassEsc). Columns needed:
#'                     species, year, julian, cum_prop.
#' @param current_data Current-season data frame (e.g. CurrentYearEsc),
#'                      updated weekly from the live file. Columns needed:
#'                      species, julian, cum_count.
#' @param curr_year    Current management year (integer), used for labels.
#' @param hist_years   Number of prior years in the historic mean/ribbon
#'                      (default 20).
#' @param esc_target   Numeric escapement target for curr_year.
build_cn_timing_plot <- function(hist_data, current_data, curr_year,
                                 hist_years = 20, esc_target) {
  
  hist_input <- hist_data |>
    filter(species == "Chinook",
           between(year, curr_year - hist_years, curr_year - 1))
  
  zero_total_years <- hist_input |>
    filter(!is.finite(cum_prop)) |>
    distinct(year) |>
    pull(year)
  
  if (length(zero_total_years) > 0) {
    warning(
      "Excluding year(s) with zero total Chinook count from historic average: ",
      paste(zero_total_years, collapse = ", ")
    )
  }
  
  hist_summary <- hist_input |>
    filter(is.finite(cum_prop)) |>
    group_by(julian) |>
    summarise(
      mean = mean(cum_prop),
      l95  = quantile(cum_prop, 0.05),
      u95  = quantile(cum_prop, 0.95),
      .groups = "drop"
    ) |>
    mutate(
      date = julian_to_date(julian),
      l95_smooth = logit_smooth(l95, julian),
      u95_smooth = logit_smooth(u95, julian)
    )
  
  current_input <- current_data |>
    filter(species == "Chinook") |>
    mutate(date = julian_to_date(julian), prop_of_target = cum_count / esc_target)
  
  if (nrow(current_input) == 0) {
    warning(
      "current_data has 0 rows after filtering species == 'Chinook' -- ",
      "check the actual species values with unique(current_data$species)."
    )
  }

  # Most recent day with a real (non-NA) cumulative count, for the
  # current-count callout at the end of the orange line.
  latest_point <- current_input |>
    filter(!is.na(cum_count)) |>
    slice_max(date, n = 1, with_ties = FALSE)

  ggplot() +
    geom_ribbon(
      data = hist_summary,
      aes(date, ymin = l95_smooth, ymax = u95_smooth),
      fill = hist_colour_dark, alpha = 0.15
    ) +
    geom_textsmooth(
      data = hist_summary,
      aes(date, mean),
      label = paste("Historic", curr_year - hist_years, "\u2013", curr_year - 1),
      linewidth = 1, hjust = 0.6, colour = hist_colour_dark,
      method = "glm", method.args = list(family = binomial())
    ) +
    geom_hline(
      yintercept = 1, linetype = "dashed", colour = target_colour, linewidth = 0.5
    ) +
    geom_textline(
      data = current_input,
      aes(date, prop_of_target),
      label = as.character(curr_year),
      colour = curr_colour, hjust = 0.7, vjust = 0.8,
      linewidth = 1.6, text_smoothing = 60
    ) +
    geom_point(
      data = latest_point,
      aes(date, prop_of_target),
      colour = curr_colour, size = 3
    ) +
    geom_label(
      data = latest_point,
      aes(date, prop_of_target, label = scales::comma(cum_count)),
      colour = "white", fill = curr_colour, fontface = "bold",
      size = 3, label.padding = unit(0.15, "lines"), label.r = unit(0.1, "lines"),
      hjust = 0, nudge_x = 2
    ) +
    scale_y_continuous(
      labels = scales::percent,
      name = "Proportion of total escapement (historic) / target (current year)",
      breaks = seq(0, 1, 0.25),
      sec.axis = sec_axis(
        transform = ~ . * esc_target,
        breaks = seq(0, 1, 0.25) * esc_target,
        labels = scales::comma,
        name = paste(curr_year, "cumulative escapement")
      ),
      expand = expansion(mult = c(0, 0.05))
    ) + 
    scale_x_date(breaks = "2 weeks", date_labels = "%d %b") +
    coord_cartesian(
      xlim = as.Date(c(paste0(ANCHOR_YEAR, "-08-01"), paste0(ANCHOR_YEAR, "-10-27")))
    ) +
    labs(x = NULL) +
    cn_theme() +
    theme(legend.position = "none")
}

# ---- Spaghetti (cumulative count) plot ----------------------------------

#' Build Stamp River Chinook cumulative-count spaghetti plot
#'
#' Shows each historic year as an individual line (faded by recency, colour
#' legend) plus the current year highlighted, on raw cumulative count
#' rather than proportion -- complements build_cn_timing_plot().
#'
#' @param hist_data    Historic data frame (e.g. SomassEsc). Columns needed:
#'                     species, year, julian, cum_count.
#' @param current_data Current-season data frame (e.g. CurrentYearEsc).
#'                      Columns needed: species, julian, cum_count.
#' @param curr_year    Current management year (integer), used for label.
#' @param hist_years   Number of prior years shown as individual lines
#'                      (default 10).
build_cn_spaghetti_plot <- function(hist_data, current_data, curr_year,
                                    hist_years = 10) {
  
  hist_input <- hist_data |>
    filter(species == "Chinook",
           between(year, curr_year - hist_years, curr_year - 1)) |>
    mutate(date = julian_to_date(julian))
  
  current_input <- current_data |>
    filter(species == "Chinook") |>
    mutate(date = julian_to_date(julian))
  
  if (nrow(current_input) == 0) {
    warning(
      "current_data has 0 rows after filtering species == 'Chinook' -- ",
      "check the actual species values with unique(current_data$species)."
    )
  }
  
  hist_years_present <- sort(unique(hist_input$year))
  n_years <- length(hist_years_present)
  
  hist_colours <- colorRampPalette(hist_year_palette)(n_years)
  names(hist_colours) <- hist_years_present
  
  all_colours <- c(hist_colours, setNames(curr_colour, curr_year))
  legend_order <- as.character(c(hist_years_present, curr_year))
  
  plot_input <- bind_rows(
    hist_input |> mutate(year = as.character(year)),
    current_input |> mutate(year = as.character(curr_year))
  ) |>
    mutate(is_current = year == as.character(curr_year))
  
  ggplot(plot_input, aes(date, cum_count, colour = year, group = year)) +
    geom_line(aes(linewidth = is_current, alpha = is_current)) +
    scale_colour_manual(values = all_colours, breaks = legend_order, name = "Year") +
    scale_linewidth_manual(values = c(`TRUE` = 1.6, `FALSE` = 0.8), guide = "none") +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.75), guide = "none") +
    scale_x_date(
      breaks = "2 weeks", date_labels = "%d %b",
      expand = expansion(mult = 0)
    ) +
    coord_cartesian(
      xlim = as.Date(c(paste0(ANCHOR_YEAR, "-08-01"), paste0(ANCHOR_YEAR, "-10-20")))
    ) +
    scale_y_continuous(
      position = "right",
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = NULL, y = "Cumulative Stamp Falls Chinook escapement") +
    cn_theme() +
    theme(
      axis.title.y.right = element_text(margin = margin(l = 0.5, unit = "lines")),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
      legend.key = element_rect(fill = NA)
    ) +
    guides(colour = guide_legend(override.aes = list(linewidth = 2.5)))
}

# ---- Running the functions and generating plots -----------

ChinookTimingPlot <- build_cn_timing_plot(
  hist_data    = SomassEsc,
  current_data = CurrentYearEsc,
  curr_year    = curr_year,
  hist_years   = 10,
  esc_target   = 34000
)

ChinookSpagettiPlot <- build_cn_spaghetti_plot(
  hist_data    = SomassEsc,
  current_data = CurrentYearEsc,
  curr_year    = curr_year,
  hist_years   = 10
)

ggsave(
  plot = ChinookTimingPlot,
  filename = paste0(
    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
    curr_year,
    "/A23/Escapement plot/",
    "Fig4_", curr_year, "_ChinookTimingPlot.png"
  ),
  height = 4.5,
  width = 8,
  units = "in"
)
