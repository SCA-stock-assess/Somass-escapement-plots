# =============================================================================
# Stamp Coho Escapement — Load, Wrangle & Plot
# Historical: Stamp Daily Expanded (2015–2025)
# Current:    Daily Totals by Age (in-season, updated weekly)
# =============================================================================


# -----------------------------------------------------------------------------
# 1. LIBRARIES
# -----------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggridges)
library(patchwork)


# -----------------------------------------------------------------------------
# 2. SEASON CONSTANTS — only section that needs updating each year
# -----------------------------------------------------------------------------

CURRENT_YEAR      <- 2026
JULIAN_END        <- 315        # Nov 11 — all historical years padded to here

forecast_category <- "Quartile 2"   # update to current preseason forecast

forecast_colours <- c(
  "Quartile 1" = "#ddeef5",     # pale ice blue    — lowest survival
  "Quartile 2" = "#a8c4d4",    # light slate      — below average
  "Quartile 3" = "#6b8fa8",    # medium slate     — above average
  "Quartile 4" = "#3d5166"    # dark slate blue  — highest survival
)
forecast_colour <- forecast_colours[[forecast_category]]


# -----------------------------------------------------------------------------
# 3. ROBUST DATE PARSER
#    Handles Excel serial numbers (e.g. "42479") and "mm-dd-yyyy" strings.
#    col_types = "text" in the loader prevents readxl from auto-converting
#    dates before this function sees them.
# -----------------------------------------------------------------------------

parse_review_date <- function(x) {
  case_when(
    grepl("^[0-9]{5}$", x) ~
      as.character(as.Date(as.numeric(x), origin = "1899-12-30")),
    grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$", x) ~
      as.character(as.Date(x, format = "%m-%d-%Y")),
    TRUE ~ NA_character_
  ) |>
    as.Date()
}


# -----------------------------------------------------------------------------
# 4. HISTORICAL YEAR LOADER
#    Reads one Stamp Daily Expanded sheet, strips totals/blank rows,
#    parses dates and converts count columns to numeric.
# -----------------------------------------------------------------------------

load_stamp_year <- function(file, year) {
  read_xlsx(file,
            sheet     = "Stamp Daily Expanded",
            na        = "",
            col_types = "text") |>
    select(
      Site, `Review Date`,
      Sk, SkJk, Co, CoJk, Pk, Cm, Cn, CnJk, Stlh, Rain,
      `Sk  Unk`, `Sk  NoMark`, `Sk  Mark`,
      `SkJk  Unk`, `SkJk  NoMark`, `SkJk  Mark`,
      `Co  Unk`, `Co  NoMark`, `Co  Mark`,
      `CoJk  Unk`, `CoJk  NoMark`, `CoJk  Mark`
    ) |>
    filter(
      !is.na(`Review Date`),
      grepl("^[0-9]", `Review Date`)    # exclude "Totals" rows and blank rows
    ) |>
    mutate(
      year          = as.integer(year),
      `Review Date` = parse_review_date(`Review Date`),
      across(c(Sk, SkJk, Co, CoJk, Pk, Cm, Cn, CnJk, Stlh, Rain,
               `Sk  Unk`, `Sk  NoMark`, `Sk  Mark`,
               `SkJk  Unk`, `SkJk  NoMark`, `SkJk  Mark`,
               `Co  Unk`, `Co  NoMark`, `Co  Mark`,
               `CoJk  Unk`, `CoJk  NoMark`, `CoJk  Mark`),
             as.numeric)
    )
}


# -----------------------------------------------------------------------------
# 5. FILE MANIFEST — add one row per new historical year
# -----------------------------------------------------------------------------

stamp_files <- tribble(
  ~year, ~file,
  2015,  "2015 Inseason Somass Counts.xlsx",
  2016,  "2016 Inseason Somass Counts.xlsx",
  2017,  "2017 Inseason Somass Counts.xlsx",
  2018,  "2018 Inseason Somass Counts Final.xlsx",
  2019,  "2019 Inseason Somass Counts Final update.xlsx",
  2020,  "2020 Inseason Somass Counts Final.xlsx",
  2021,  "2021 Somass Counts Final.xlsx",
  2022,  "2022 Somass Counts Final.xlsx",
  2023,  "2023 Somass Counts Final.xlsx",
  2024,  "2024 Somass Counts Final.xlsx",
  2025,  "2025 Somass Counts Final.xlsx"
)


# -----------------------------------------------------------------------------
# 6. COLUMN CONSISTENCY CHECK
#    Run before loading — all rows should show "none" in missing_cols.
# -----------------------------------------------------------------------------

needed_cols <- c(
  "Site", "Review Date",
  "Sk", "SkJk", "Co", "CoJk", "Pk", "Cm", "Cn", "CnJk", "Stlh", "Rain",
  "Sk  Unk", "Sk  NoMark", "Sk  Mark",
  "SkJk  Unk", "SkJk  NoMark", "SkJk  Mark",
  "Co  Unk", "Co  NoMark", "Co  Mark",
  "CoJk  Unk", "CoJk  NoMark", "CoJk  Mark"
)

map2(stamp_files$file, stamp_files$year, ~ {
  nms     <- read_xlsx(.x, sheet = "Stamp Daily Expanded",
                       col_types = "text", n_max = 0) |> names()
  missing <- needed_cols[!needed_cols %in% nms]
  tibble(year         = .y,
         missing_cols = if (length(missing) == 0) "none" else
           paste(missing, collapse = ", "))
}) |>
  bind_rows() |>
  print(n = Inf)


# -----------------------------------------------------------------------------
# 7. LOAD & WRANGLE HISTORICAL DATA
#    - Adult Co only (jacks excluded)
#    - NA counts set to 0 for complete historical years
#    - Cumulative columns computed per year: total, marked, unmarked, proportion
#    - 2020 removed (incomplete season)
#    - Each year padded to JULIAN_END so percentile bands plateau cleanly
# -----------------------------------------------------------------------------

stampHistPadded <- map2(stamp_files$file, stamp_files$year, load_stamp_year) |>
  bind_rows() |>
  rename(date = `Review Date`) |>
  filter(!is.na(date)) |>
  mutate(
    julian       = as.numeric(format(date, "%j")),
    Co           = if_else(is.na(Co),           0, Co),
    `Co  NoMark` = if_else(is.na(`Co  NoMark`), 0, `Co  NoMark`),
    `Co  Mark`   = if_else(is.na(`Co  Mark`),   0, `Co  Mark`)
  ) |>
  filter(year != 2020) |>                          # remove incomplete year
  group_by(year) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    cum_count  = cumsum(Co),
    cum_nomark = cumsum(`Co  NoMark`),
    cum_mark   = cumsum(`Co  Mark`),
    ann_ttl    = sum(Co, na.rm = TRUE),
    cum_prop   = cum_count / ann_ttl
  ) |>
  complete(julian = full_seq(c(julian, JULIAN_END), 1)) |>
  fill(cum_prop, cum_count, cum_nomark, cum_mark, .direction = "down") |>
  replace_na(list(cum_prop = 0, cum_count = 0, cum_nomark = 0, cum_mark = 0)) |>
  mutate(date = as.Date(julian - 1, origin = paste0(year, "-01-01"))) |>
  ungroup()


# -----------------------------------------------------------------------------
# 8. SANITY CHECK — all n_na_date should be 0
# -----------------------------------------------------------------------------

stampHistPadded |>
  group_by(year) |>
  summarise(
    n_rows    = n(),
    n_na_date = sum(is.na(date)),
    min_date  = min(date, na.rm = TRUE),
    max_date  = max(date, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  print(n = Inf)


# -----------------------------------------------------------------------------
# 9. LOAD & WRANGLE CURRENT YEAR
#    Loaded independently — different file format, no marked/unmarked breakdown.
#    cum_prop scaled to observed total so far: reaches 1 at last data point.
#    Line stops at the most recent weekly update — never padded forward.
# -----------------------------------------------------------------------------

stampCurrent <- read_xlsx(
  "Daily Totals by Age 2026 Example.xlsx",
  sheet = "Stamp CN&CO",
  na    = ""
) |>
  mutate(
    date   = as.Date(Date),
    julian = as.numeric(format(date, "%j")),
    Co     = as.numeric(Coho),
    year   = as.integer(CURRENT_YEAR)
  ) |>
  filter(!is.na(date)) |>
  arrange(date) |>
  mutate(
    cum_count      = cumsum(replace_na(Co, 0)),
    ann_ttl_so_far = sum(Co, na.rm = TRUE),
    cum_prop       = cum_count / ann_ttl_so_far
  ) |>
  select(year, date, julian, Co, cum_count, cum_prop)


# -----------------------------------------------------------------------------
# 10. HISTORICAL PERCENTILE SUMMARIES (25th / 60th percentiles)
#     Reference: DFO Pacific Salmon Outlook 2026
#     waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41316605.pdf
# -----------------------------------------------------------------------------

summarise_percentiles <- function(data, col) {
  data |>
    group_by(julian) |>
    summarise(
      median_val = median(.data[[col]], na.rm = TRUE),
      p25        = quantile(.data[[col]], 0.25, na.rm = TRUE),
      p60        = quantile(.data[[col]], 0.60, na.rm = TRUE),
      pMax       = max(.data[[col]], na.rm = TRUE),
      .groups    = "drop"
    )
}

stampHistTiming  <- summarise_percentiles(stampHistPadded, "cum_prop")
stampHistCount   <- summarise_percentiles(stampHistPadded, "cum_count")
stampHistMark    <- summarise_percentiles(stampHistPadded, "cum_mark")
stampHistNoMark  <- summarise_percentiles(stampHistPadded, "cum_nomark")


# -----------------------------------------------------------------------------
# 11. SHARED PLOT ELEMENTS
# -----------------------------------------------------------------------------



ribbon_light <- "#c9756e"    # below 25th percentile — behind historical norm
ribbon_mid   <- "#d4a843"    # 25th to 60th percentile — typical range
ribbon_dark  <- "#6a9e6f"    # above 60th percentile — ahead of historical norm

x_scale <- scale_x_continuous(limits = c(200, JULIAN_END))


# -----------------------------------------------------------------------------
# 12. FORECAST PANEL — combined with all ribbon plots via patchwork
# -----------------------------------------------------------------------------

forecast_df <- data.frame(
  category = factor(names(forecast_colours), levels = names(forecast_colours)),
  value    = 1
)

p_bottom <- ggplot(forecast_df, aes(x = category, y = value, fill = category)) +
  geom_col(width = 0.7, alpha = 0.7) +
  geom_col(data = filter(forecast_df, category == forecast_category),
           aes(x = category, y = value),
           fill = forecast_colour, width = 0.7) +
  annotate("text", x = forecast_category, y = 0.5,
           label = "► Current", hjust = 0.5, vjust = 0.5,
           fontface = "bold") +
  scale_fill_manual(values = forecast_colours) +
  scale_x_discrete(limits = names(forecast_colours)) +   
  labs(x = NULL, y = NULL, title = "Pre-season Survival Forecast") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y    = element_blank(),
        axis.ticks.y   = element_blank())


# -----------------------------------------------------------------------------
# 13. RIBBON PLOT BUILDER
#     hist_median_col: median column name in the historical summary data
#     current_col:     column in stampCurrent to overlay (always cum_count
#                      or cum_prop — never marked/unmarked since unavailable)
# -----------------------------------------------------------------------------

build_ribbon_plot <- function(hist_data, hist_median_col, current_col, y_label) {
  ggplot() +
    geom_ribbon(data = hist_data,
                aes(x = julian, ymin = 0, ymax = p25),
                fill = ribbon_light, alpha = 0.30) +
    geom_ribbon(data = hist_data,
                aes(x = julian, ymin = p25, ymax = p60),
                fill = ribbon_mid, alpha = 0.25) +
    geom_ribbon(data = hist_data,
                aes(x = julian, ymin = p60, ymax = pMax),
                fill = ribbon_dark, alpha = 0.30) +
    geom_line(data = stampCurrent,
              aes(x = julian, y = .data[[current_col]]),
              colour = "black", linewidth = 1.5) +
    annotate("text",
             x      = max(stampCurrent$julian),
             y      = max(stampCurrent[[current_col]], na.rm = TRUE),
             label  = paste0(CURRENT_YEAR),
             colour = "black", hjust = -0.1, size = 3) +
    # ── Ribbon legend ──────────────────────────────────────────────────────
    annotate("rect", xmin = 220, xmax = 230, ymin = 23400, ymax = 24400,
             fill = ribbon_dark,  alpha = 0.6) +
    annotate("rect", xmin = 220, xmax = 230, ymin = 22200, ymax = 23200,
             fill = ribbon_mid,   alpha = 0.6) +
    annotate("rect", xmin = 220, xmax = 230, ymin = 21000, ymax = 22000,
             fill = ribbon_light, alpha = 0.6) +
    annotate("text", x = 230, y = 23900, label = "Above 60th percentile",
             hjust = 0, size = 3, colour = "grey20") +
    annotate("text", x = 230, y = 22700, label = "25th–60th percentile",
             hjust = 0, size = 3, colour = "grey20") +
    annotate("text", x = 230, y = 21500, label = "Below 25th percentile",
             hjust = 0, size = 3, colour = "grey20") +
    # ───────────────────────────────────────────────────────────────────────
    scale_y_continuous(name = paste("Stamp River", y_label)) +
    x_scale +
    theme_classic()
}


# -----------------------------------------------------------------------------
# 14. RIBBON PLOTS
#     Marked and unmarked plots overlay total Co for current year since
#     the in-season file does not have a marked/unmarked breakdown.
# -----------------------------------------------------------------------------

p_count  <- build_ribbon_plot(stampHistCount,  "median_val",
                              "cum_count", "Cumulative Adult Coho Count")
p_nomark <- build_ribbon_plot(stampHistNoMark, "median_val",
                              "cum_count", "Cumulative Unmarked Coho Count")
p_timing <- build_ribbon_plot(stampHistTiming, "median_val",
                              "cum_prop",  "Cumulative Proportion of Run")

# Print and save
p_count   / p_bottom + plot_layout(heights = c(4, 1)) #✅ p_count — always valid
ggsave("stamp_abundance.png", width = 8, height = 6, dpi = 300)

p_nomark  / p_bottom + plot_layout(heights = c(4, 1)) #❌ p_mark — not valid in-season
ggsave("stamp_unmarked.png",  width = 8, height = 6, dpi = 300)

p_timing  / p_bottom + plot_layout(heights = c(4, 1)) #⚠️ p_timing — valid only with a forecast denominator
ggsave("stamp_timing.png",    width = 8, height = 6, dpi = 300)


# -----------------------------------------------------------------------------
# 15. RIDGELINE PLOT — total adult coho (marked + unmarked), one ridge per year
# -----------------------------------------------------------------------------

half_timing <- stampHistPadded |>
  group_by(year) |>
  filter(cum_prop >= 0.5) |>
  slice(1) |>
  ungroup() |>
  summarise(median_50pct_julian = median(julian))

median_50pct <- half_timing$median_50pct_julian

p_ridge_total <- stampHistPadded |>
  filter(!is.na(julian), julian >= 225) |>
  mutate(year = factor(year)) |>
  ggplot(aes(x = julian, y = cum_prop)) +
  geom_area(fill = "#7ba3a8", alpha = 0.4, colour = "black", linewidth = 0.6) +
  scale_x_continuous(limits = c(225, JULIAN_END), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0.5, 1), labels = c("0.5", "1.0"),
                     limits = c(0, 1), expand = c(0, 0),
                     position = "right") +
  facet_grid(year ~ ., switch = "y") +
  
  labs(x = "", y = NULL,
       title = "Stamp River Total Adult Coho Escapement Timing") +
  
  theme_classic() +
  theme(
    strip.placement   = "outside",
    strip.background  = element_blank(),
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10,
                                     colour = "#333"),
    panel.spacing     = unit(0.1, "lines"),
    axis.text.y.right  = element_text(size = 6, colour = "#333333"),
    axis.ticks.y.right = element_line(linewidth = 0.3),
    axis.ticks.length  = unit(0.15, "cm"),
    axis.line.y.right  = element_line(colour = "#444444", linewidth = 0.4),
    plot.title         = element_text(size = 12, colour = "#333333"),
    plot.background    = element_rect(fill = "white", colour = NA)
  )

print(p_ridge_total)
ggsave("stamp_ridge_total.png", plot = p_ridge_total,
       width = 8, height = 10, dpi = 300)
