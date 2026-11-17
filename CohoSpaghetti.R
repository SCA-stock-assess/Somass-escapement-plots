# =============================================================================
# Somass Coho Escapement Bulletin Plots
# Cumulative spaghetti plots: historic years (grey) vs current year (orange)
#   - Stamp unmarked, Stamp marked, Sproat unmarked
# Each line ends at the last day with an actual count; nothing is extended.
# =============================================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(ggrepel)

# ---- Settings ---------------------------------------------------------------

curr_year   <- 2026
ANCHOR_YEAR <- 2001                                     # non-leap year used to align all dates
X_START     <- as.Date(paste0(ANCHOR_YEAR, "-08-01"))   # left edge of the plots
curr_colour <- "#EE6C29"

current_year_file <- paste0(
  "Y:/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/", curr_year, "_MGT/",
  "Daily Totals by Age ", curr_year, ".xlsx"
)

# ---- Helpers ----------------------------------------------------------------

safe_julian <- function(date) {
  yday(as.Date(paste0(ANCHOR_YEAR, "-", format(date, "%m-%d"))))
}

julian_to_date <- function(j) as.Date(j - 1, origin = paste0(ANCHOR_YEAR - 1, "-12-31"))

save_bulletin_plot <- function(plot, name, height = 5, width = 9) {
  ggsave(
    plot = plot,
    filename = paste0(
      "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
      curr_year, "/A23/Escapement plot/",
      "R-PLOT_", curr_year, "_", name, "_", format(Sys.Date(), "%Y-%m-%d"), ".png"
    ),
    height = height, width = width, units = "in", dpi = 300
  )
}

# ---- Historic file config ---------------------------------------------------
# One row per historic year. Add/remove rows to change the historic range;
# everything downstream is derived from this table.
# date_format: "iso" = ISO text/date, "serial" = Excel serial number,
# otherwise a strptime format (2025 is "MM-DD-YYYY" text).

file_config <- tribble(
  ~year, ~filename,                                       ~stamp_sheet,           ~sproat_sheet,            ~date_format,
  2015,  "2015 Inseason Somass Counts.xlsx",               "Stamp Daily Expanded", "Sproat Daily Expanded",  "iso",
  2016,  "2016 Inseason Somass Counts.xlsx",               "Stamp Daily Expanded", "Sproat Daily Expanded",  "serial",
  2017,  "2017 Inseason Somass Counts.xlsx",               "Stamp Daily Expanded", "Sproat Daily Expanded",  "serial",
  2018,  "2018 Inseason Somass Counts Final.xlsx",         "Stamp Daily Expanded", "Sproat Daily Expanded",  "serial",
  2019,  "2019 Inseason Somass Counts Final update.xlsx",  "Stamp Daily Expanded", "Sproat Daily Expanded",  "serial",
  2020,  "2020 Inseason Somass Counts Final.xlsx",         "Stamp Daily Expanded", "Sproat Daily Expanded",  "serial",
  2021,  "2021 Somass Counts Final.xlsx",                  "Stamp Daily Expanded", "Sproat Daily Expanded",  "serial",
  2022,  "2022 Somass Counts Final.xlsx",                  "Stamp Daily Expanded", "Sproat Daily Expanded",  "serial",
  2023,  "2023 Somass Counts Final.xlsx",                  "Stamp Daily Expanded", "Sproat Daily Expanded",  "serial",
  2024,  "2024 Somass Counts Final.xlsx",                  "Stamp Daily Expanded", "Sproat Daily Expanded",  "serial",
  2025,  "2025 Somass Counts Final.xlsx",                  "Stamp Daily Expanded", "Sproat Daily Expanded",  "%m-%d-%Y"
)

# ---- Read historic data -----------------------------------------------------

read_one_site <- function(filename, sheet, year, date_format) {
  df <- read_xlsx(filename, sheet = sheet, na = "") |>
    select(Site, `Review Date`, `Co  Mark`, `Co  NoMark`) |>
    mutate(year = year, across(starts_with("Co"), as.numeric))
  
  df$`Review Date` <- if (date_format == "serial") {
    as.Date(as.numeric(df$`Review Date`), origin = "1899-12-30")
  } else if (date_format == "iso") {
    as.Date(df$`Review Date`)
  } else {
    as.Date(df$`Review Date`, format = date_format)
  }
  df
}

historic_data <- file_config |>
  pivot_longer(c(stamp_sheet, sproat_sheet), names_to = NULL, values_to = "sheet") |>
  pmap(\(year, filename, date_format, sheet) read_one_site(filename, sheet, year, date_format)) |>
  list_rbind()

# ---- Plot function ----------------------------------------------------------
# site:   "Stamp" or "Sproat"
# status: "NoMark" (unmarked) or "Mark" (marked)

coho_spaghetti <- function(site, status = c("NoMark", "Mark")) {
  status    <- match.arg(status)
  count_col <- paste0("Co  ", status)      # source column names contain two spaces
  y_title   <- paste0(site, " River ", if (status == "Mark") "marked" else "unmarked",
                      " Coho")
  
  # Historic years: cumulative count on the common anchor-year calendar.
  # Rows with no count are dropped, so each line ends at its last real observation.
  hist_df <- historic_data |>
    filter(
      str_detect(Site, regex(site, ignore_case = TRUE)),
      !is.na(`Review Date`), !is.na(.data[[count_col]])
    ) |>
    mutate(date = julian_to_date(safe_julian(`Review Date`))) |>
    arrange(year, date) |>
    mutate(cum = cumsum(.data[[count_col]]), .by = year) |>
    filter(date >= X_START)   # cumulative total already includes any earlier counts
  
  no_counts <- setdiff(file_config$year, hist_df$year)
  if (length(no_counts) > 0) {
    stop(site, " ", status, ": no counts found for ", toString(no_counts))
  }
  
  # Current year: cumulative count to the last day with data
  curr_df <- read_xlsx(current_year_file, sheet = paste0(site, " CN&CO"), na = "") |>
    filter(!is.na(Date)) |>
    transmute(date = julian_to_date(safe_julian(Date)),
              count = as.numeric(.data[[count_col]])) |>
    filter(!is.na(count)) |>
    arrange(date) |>
    mutate(cum = cumsum(count)) |>
    filter(date >= X_START)
  
  hist_end <- slice_max(hist_df, date, n = 1, by = year, with_ties = FALSE)
  latest   <- slice_max(curr_df, date, n = 1, with_ties = FALSE)
  x_max    <- max(hist_df$date, curr_df$date)   # latest date with data in any year
  x_right  <- x_max + 8                         # room for the last year label
  y_max    <- max(hist_df$cum, curr_df$cum)
  
  ggplot() +
    # historic years
    geom_line(
      data = hist_df,
      aes(date, cum, group = year),
      colour = "grey60", linewidth = 0.4
    ) +
    # year label at the end of each line; only nudged vertically if labels collide
    geom_text_repel(
      data = hist_end,
      aes(date, cum, label = year),
      colour = "grey30", size = 3.2, hjust = 0, direction = "y",
      nudge_x = 1, bg.color = "white", bg.r = 0.15,
      box.padding = 0.15, min.segment.length = 0.5,
      segment.colour = "grey60", segment.size = 0.2, seed = 1
    ) +
    # current year, with its value beside the end of the line
    geom_line(
      data = curr_df,
      aes(date, cum),
      colour = curr_colour, linewidth = 1.3
    ) +
    geom_point(
      data = latest,
      aes(date, cum),
      colour = curr_colour, size = 2.8
    ) +
    geom_label(
      data = latest,
      aes(date, cum, label = paste0(curr_year, ": ", comma(cum))),
      colour = curr_colour, fontface = "bold", size = 4,
      hjust = 0, nudge_x = 1.5, linewidth = 0, fill = alpha("white", 0.85)
    ) +
    scale_x_date(
      breaks = seq(X_START, x_right, by = "2 weeks"),
      date_labels = "%b %d"
    ) +
    scale_y_continuous(
      name = y_title, position = "right",
      breaks = pretty_breaks(n = 6), labels = comma
    ) +
    coord_cartesian(
      xlim = c(X_START, x_right), ylim = c(0, y_max * 1.05),
      expand = FALSE, clip = "off"
    ) +
    labs(
      x = NULL) +
    theme_classic(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      axis.ticks = element_line(colour = "black", linewidth = 0.4),
      axis.title.y.right = element_text(margin = margin(l = 8)),
      plot.caption = element_text(colour = "grey40", size = 9, hjust = 0),
      plot.margin = margin(10, 12, 8, 30)   # left margin leaves room for the "Aug 01" label
    )
}

# ---- Build, preview & save --------------------------------------------------

plots <- list(
  CO_Stamp_unmarked_spaghetti  = coho_spaghetti("Stamp",  "NoMark"),
  CO_Stamp_marked_spaghetti    = coho_spaghetti("Stamp",  "Mark"),
  CO_Sproat_unmarked_spaghetti = coho_spaghetti("Sproat", "NoMark")
)

walk(plots, print)
iwalk(plots, \(p, name) save_bulletin_plot(p, name))
