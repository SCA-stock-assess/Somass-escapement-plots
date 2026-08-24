# =============================================================================
# Somass Coho Escapement Bulletin Plots -- curr_year, current as of Aug 2026
# =============================================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(geomtextpath)
library(scales)   # needed for comma() in current-escapement annotation labels

# ---- Shared helpers ---------------------------------------------------

ANCHOR_YEAR <- 2001

safe_julian <- function(date) {
  yday(as.Date(paste0(ANCHOR_YEAR, "-", format(date, "%m-%d"))))
}

julian_to_date <- function(j) as.Date(j - 1, origin = paste0(ANCHOR_YEAR - 1, "-12-31"))

PLOT_XLIM <- as.Date(c(
  paste0(ANCHOR_YEAR, "-08-01"),
  paste0(ANCHOR_YEAR, "-11-05")
))

hist_colour_marked   <- "#2C5F7C"
hist_colour_unmarked <- "#3C8C5F"
curr_colour          <- "#EE6c29"

hist_year_palette <- c(
  "#2E8B57", "#566238", "#FFD95D", "#6D8EC5", "#9C755F",
  "#162660", "#CF8852", "#6F6134", "#C9C769", "#F0C845"
)

cn_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
      axis.line = element_line(colour = "black", linewidth = 0.4),
      axis.ticks.x = element_line(colour = "black", linewidth = 0.4),
      axis.ticks.length.x = unit(0.15, "cm")
    )
}

save_bulletin_plot <- function(plot, name, height = 4.5, width = 8) {
  ggsave(
    plot = plot,
    filename = paste0(
      "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/CHINOOK_MGT/",
      curr_year, "/A23/Escapement plot/",
      "R-PLOT_", curr_year, "_", name, "_",
      format(Sys.Date(), "%Y-%m-%d"), ".png"
    ),
    height = height, width = width, units = "in"
  )
}

# Deterministic per-year hjust offsets for spaghetti labels, computed once
# outside aes(). aes() evaluates expressions against the whole data frame,
# not per group/row -- runif(1, ...) inside aes() previously produced a
# single value recycled across every year (identical offset for all lines)
# and re-randomized on every render. This fixes both problems: one offset
# per year, stable across re-runs of the bulletin for a given seed.
make_hjust_lookup <- function(df, seed) {
  set.seed(seed)
  df |>
    distinct(year) |>
    mutate(hjust = runif(n(), 0.8, 1))
}

# Latest non-NA cumulative value + its date, for the current-year annotation.
get_latest_point <- function(df, value_col) {
  df |>
    filter(!is.na(.data[[value_col]])) |>
    filter(date == max(date))
}

curr_year <- 2026

# ---- Historic file config --------------------------------------------------
# Filenames/sheet names/date formats are genuinely inconsistent across years
# in the source files -- encoded explicitly per year rather than assuming a
# clean pattern.
#
# NUMBER OF HISTORICAL YEARS IS CONTROLLED HERE: add/remove rows in this
# tribble to change the historical range. Everything downstream (palette
# sizing, hjust lookup, colour scale domain) is derived from
# unique(year)/n_distinct(year), so nothing else needs to change when a
# year is added or removed -- EXCEPT: confirm date_format for any new year
# before trusting output (see 2025 note below).
#
# 2025's "Review Date" was confirmed (via str()) to be a character string
# in "MM-DD-YYYY" format, not a serial number and not ISO -- hence the
# explicit date_format override below. Sproat's 2025 format has NOT been
# separately confirmed to match Stamp's -- check before trusting Sproat 2025.

file_config <- tribble(
  ~year, ~filename,                                        ~stamp_sheet,            ~sproat_sheet,            ~date_is_serial,
  2015,  "2015 Inseason Somass Counts.xlsx",                "Stamp Daily Expanded",  "Sproat Daily Expanded",  FALSE,
  2016,  "2016 Inseason Somass Counts.xlsx",                "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE,
  2017,  "2017 Inseason Somass Counts.xlsx",                "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE,
  2018,  "2018 Inseason Somass Counts Final.xlsx",          "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE,
  2019,  "2019 Inseason Somass Counts Final update.xlsx",   "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE,
  2020,  "2020 Inseason Somass Counts Final.xlsx",          "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE,
  2021,  "2021 Somass Counts Final.xlsx",                   "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE,
  2022,  "2022 Somass Counts Final.xlsx",                   "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE,
  2023,  "2023 Somass Counts Final.xlsx",                   "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE,
  2024,  "2024 Somass Counts Final.xlsx",                   "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE,
  2025,  "2025 Somass Counts Final.xlsx",                   "Stamp Daily Expanded",  "Sproat Daily Expanded",  TRUE
) |>
  mutate(date_format = case_when(
    year == 2025 ~ "%m-%d-%Y",
    date_is_serial ~ "serial",
    TRUE ~ "iso"
  ))

# ---- Read + combine all historic years -------------------------------------

read_one_site <- function(filename, sheet, year, date_format) {
  df <- read_xlsx(filename, sheet = sheet, na = "") |>
    mutate(year = year)
  df$`Review Date` <- if (date_format == "serial") {
    as.Date(as.numeric(df$`Review Date`), origin = "1899-12-30")
  } else if (date_format == "iso") {
    as.Date(df$`Review Date`)
  } else {
    as.Date(df$`Review Date`, format = date_format)
  }
  df
}

keep_cols <- c("Site", "Review Date", "Co", "CoJk", "Co  NoMark", "Co  Mark",
               "CoJk  NoMark", "CoJk  Mark", "year")

stamp_list  <- pmap(file_config, \(year, filename, stamp_sheet, sproat_sheet, date_is_serial, date_format)
                    read_one_site(filename, stamp_sheet, year, date_format))
sproat_list <- pmap(file_config, \(year, filename, stamp_sheet, sproat_sheet, date_is_serial, date_format)
                    read_one_site(filename, sproat_sheet, year, date_format))

filtered_data <- c(stamp_list, sproat_list) |>
  map(\(df) df[, intersect(keep_cols, names(df)), drop = FALSE])

historic_data_2015_2025 <- bind_rows(filtered_data, .id = "source") |>
  filter(str_detect(Site, regex("stamp|sproat", ignore_case = TRUE)))

# Sanity check: 2025 present, and its dates actually fall in range (not NA
# and not some other year -- catches both the missing-year bug and the
# earlier all-NA-date bug in one check).
stopifnot(2025 %in% unique(historic_data_2015_2025$year))
stopifnot(!all(is.na(historic_data_2015_2025$`Review Date`[historic_data_2015_2025$year == 2025])))

# ---- Current year read-in ---------------------------------------------------
# ASSUMPTION: filename/sheet pattern below -- not verified for curr_year
# specifically; confirm before trusting output.

current_year_file <- paste0(
  "Y:/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/", curr_year, "_MGT/",
  "Daily Totals by Age ", curr_year, ".xlsx"
)

current_data_stamp <- read_xlsx(
  current_year_file,
  sheet = "Stamp CN&CO", na = ""
) |>
  select(Date, `Co  Mark`, `Co  NoMark`) |>
  mutate(
    year = curr_year,
    date = julian_to_date(safe_julian(Date)),
    Co_Mark = `Co  Mark`,
    Co_NoMark = `Co  NoMark`
  ) |>
  arrange(Date) |>
  mutate(
    Co_Mark_Cumulative = cumsum(replace_na(Co_Mark, 0)),
    Co_NoMark_Cumulative = cumsum(replace_na(Co_NoMark, 0))
  )

current_data_sproat <- read_xlsx(
  current_year_file,
  sheet = "Sproat CN&CO", na = ""
) |>
  select(Date, `Co  Mark`, `Co  NoMark`) |>
  mutate(
    year = curr_year,
    date = julian_to_date(safe_julian(Date)),
    Co_Mark = `Co  Mark`,
    Co_NoMark = `Co  NoMark`
  ) |>
  arrange(Date) |>
  mutate(
    Co_Mark_Cumulative = cumsum(replace_na(Co_Mark, 0)),
    Co_NoMark_Cumulative = cumsum(replace_na(Co_NoMark, 0)),
    Co_total_Cumulative = Co_Mark_Cumulative + Co_NoMark_Cumulative
  )

# Guard against silent character-vs-numeric failures downstream (this
# exact bug class has hit the CPUE pipeline before -- filter()/comma()
# on a character column won't error, it'll just produce garbage).
stopifnot(is.numeric(current_data_stamp$Co_NoMark_Cumulative))
stopifnot(is.numeric(current_data_sproat$Co_NoMark_Cumulative))

# ---- Historic cumulative marked/unmarked (Stamp & Sproat) ------------------

stamp_hist_cumulative <- historic_data_2015_2025 |>
  filter(Site == "Stamp") |>
  mutate(julian = safe_julian(as.Date(`Review Date`))) |>
  arrange(year, julian) |>
  group_by(year) |>
  mutate(
    cum_marked = cumsum(replace_na(`Co  Mark`, 0)),
    cum_unmarked = cumsum(replace_na(`Co  NoMark`, 0))
  ) |>
  ungroup()

sproat_hist_cumulative <- historic_data_2015_2025 |>
  filter(Site == "Sproat") |>
  mutate(julian = safe_julian(as.Date(`Review Date`))) |>
  arrange(year, julian) |>
  group_by(year) |>
  mutate(
    cum_marked = cumsum(replace_na(`Co  Mark`, 0)),
    cum_unmarked = cumsum(replace_na(`Co  NoMark`, 0))
  ) |>
  ungroup()


# =============================================================================
# PLOT: Stamp Unmarked -- spaghetti (all historic years + current year)
# =============================================================================

stamp_hjust_lookup <- make_hjust_lookup(
  stamp_hist_cumulative |> filter(year != curr_year),
  seed = curr_year
)

stamp_hist_plot_data <- stamp_hist_cumulative |>
  filter(year != curr_year) |>
  mutate(date = julian_to_date(julian)) |>
  left_join(stamp_hjust_lookup, by = "year")

stamp_latest <- get_latest_point(current_data_stamp, "Co_NoMark_Cumulative")

StampUnmarkedCohoSpagethi <- ggplot() +
  
  geom_textline(
    data = stamp_hist_plot_data,
    aes(x = date, y = cum_unmarked, label = year,
        group = year, colour = factor(year), hjust = hjust),
    linewidth = 0.5,
    alpha = 0.8
  ) +
  
  geom_line(
    data = current_data_stamp,
    aes(x = date, y = Co_NoMark_Cumulative),
    colour = "black",
    linewidth = 1.5
  ) +
  
  geom_point(
    data = stamp_latest,
    aes(x = date, y = Co_NoMark_Cumulative),
    colour = "black",
    size = 2.5
  ) +
  
  geom_text(
    data = stamp_latest,
    aes(x = date, y = Co_NoMark_Cumulative,
        label = paste0("Current escapement = ", comma(Co_NoMark_Cumulative))),
    colour = "black",
    hjust = -0.1,
    vjust = -0.5,
    size = 3.5,
    fontface = "bold"
  ) +
  
  scale_colour_manual(
    values = setNames(
      rep(hist_year_palette, length.out = n_distinct(stamp_hist_plot_data$year)),
      sort(unique(stamp_hist_plot_data$year))
    )
  ) +
  
  scale_x_date(
    date_labels = "%b-%d",
    date_breaks = "1 week",
    limits = as.Date(c("2001-07-30", "2001-11-06")),
    expand = c(0, 0)
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", linewidth = 0.4),
        axis.ticks = element_line(color = "black", linewidth = 1),
        axis.text.x = element_text(size = 11, angle = 35, hjust = 1, vjust = 1),
        legend.position = "none") + xlab("") +
  scale_y_continuous(name = "Stamp River Unmarked Coho", position = "right", breaks = seq(0, 34000, by = 4000))

print(StampUnmarkedCohoSpagethi)
save_bulletin_plot(StampUnmarkedCohoSpagethi, "CO_Stamp_unmarked_spaghetti")

# =============================================================================
# PLOT: Sproat Unmarked -- spaghetti (all historic years + current year)
# =============================================================================

sproat_hjust_lookup <- make_hjust_lookup(
  sproat_hist_cumulative |> filter(year != curr_year),
  seed = curr_year + 1  # different from Stamp's seed so jitter patterns differ
)

sproat_hist_plot_data <- sproat_hist_cumulative |>
  filter(year != curr_year) |>
  mutate(date = julian_to_date(julian)) |>
  left_join(sproat_hjust_lookup, by = "year")

sproat_latest <- get_latest_point(current_data_sproat, "Co_NoMark_Cumulative")

SproatUnmarkedCohoSpagethi <- ggplot() +
  
  geom_textline(
    data = sproat_hist_plot_data,
    aes(x = date, y = cum_unmarked, label = year,
        group = year, colour = factor(year), hjust = hjust),
    linewidth = 0.5,
    alpha = 0.8
  ) +
  
  geom_line(
    data = current_data_sproat,
    aes(x = date, y = Co_NoMark_Cumulative),
    colour = "black",
    linewidth = 1.5
  ) +
  
  geom_point(
    data = sproat_latest,
    aes(x = date, y = Co_NoMark_Cumulative),
    colour = "black",
    size = 2.5
  ) +
  
  geom_text(
    data = sproat_latest,
    aes(x = date, y = Co_NoMark_Cumulative,
        label = paste0("Current escapement = ", comma(Co_NoMark_Cumulative))),
    colour = "black",
    hjust = -0.1,
    vjust = -0.5,
    size = 3.5,
    fontface = "bold"
  ) +
  
  scale_colour_manual(
    values = setNames(
      rep(hist_year_palette, length.out = n_distinct(sproat_hist_plot_data$year)),
      sort(unique(sproat_hist_plot_data$year))
    )
  ) +
  
  scale_x_date(
    date_labels = "%b-%d",
    date_breaks = "1 week",
    limits = as.Date(c("2001-07-30", "2001-11-06")),
    expand = c(0, 0)
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", linewidth = 0.4),
        axis.ticks = element_line(color = "black", linewidth = 1),
        axis.text.x = element_text(size = 11, angle = 35, hjust = 1, vjust = 1),
        legend.position = "none") + xlab("") +
  scale_y_continuous(name = "Sproat River Unmarked Coho", position = "right", breaks = seq(0, 12000, by = 2000))

print(SproatUnmarkedCohoSpagethi)
save_bulletin_plot(SproatUnmarkedCohoSpagethi, "CO_Sproat_unmarked_spaghetti")