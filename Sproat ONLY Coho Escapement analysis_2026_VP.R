# =============================================================================
# Sproat Coho Escapement — Load, Wrangle & Plot
# Historical:  2015–2025
# Current:     Daily Totals by Age (in-season, updated weekly)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. CONFIG
# -----------------------------------------------------------------------------
JULIAN_END   <- 340   # <- confirm
CURRENT_YEAR <- 2026  # <- confirm

# Current-year in-season file 
CURRENT_FOLDER <- "Y:/WCVI/SOCKEYE/SOMASS/SOCKEYE_MGMT/2026_MGT"
CURRENT_FILE   <- file.path(CURRENT_FOLDER, "Daily Totals by Age 2026.xlsx")

# Historical files — local folder (downloaded once from SharePoint since
# these 10 seasons are closed/static and never change).
HIST_FOLDER <- "C:/Users/POURFARAJV/Documents/GitHub/Somass-escapement-plots/End-of-season Historic escapement files/CohoEscapementData"

# -----------------------------------------------------------------------------
# 1. LIBRARIES
# -----------------------------------------------------------------------------
library(scales)
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)


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
# 3b. ANCHOR-YEAR JULIAN DAY HELPER
#     Maps a real calendar date onto a fixed non-leap anchor year (2001) so
#     julian day is comparable across leap and non-leap years. Feb 29 has no
#     equivalent in 2001, so it is explicitly mapped to NA rather than being
#     silently produced as an invalid date string by as.Date().
# -----------------------------------------------------------------------------

safe_julian <- function(date) {
  md <- format(date, "%m-%d")
  if_else(
    md == "02-29",
    NA_real_,
    as.numeric(format(as.Date(paste0("2001-", md)), "%j"))
  )
}

# -----------------------------------------------------------------------------
# 3c. SHEET NAME RESOLVER
#     Some workbooks have stray whitespace on sheet names too (e.g. " Sproat
#     Daily Expanded"). read_xlsx(sheet = ...) requires an exact match, so
#     resolve the real sheet name via trimmed comparison first rather than
#     trusting the literal target string.
# -----------------------------------------------------------------------------

resolve_sheet <- function(path, target) {
  sheets <- excel_sheets(path)
  match  <- sheets[trimws(sheets) == trimws(target)]
  if (length(match) == 0) {
    stop(sprintf("Sheet '%s' not found in %s (available: %s)",
                 target, path, paste(sheets, collapse = ", ")))
  }
  match[1]
}

# -----------------------------------------------------------------------------
# 4. HISTORICAL DATA LOADER
#    Reads one Sproat Daily Expanded sheet, strips totals/blank rows,
#    parses dates and converts count columns to numeric.
# -----------------------------------------------------------------------------

load_sproat_year <- function(filename, year) {
  local_path <- file.path(HIST_FOLDER, filename)
  read_xlsx(local_path,
            sheet     = resolve_sheet(local_path, "Sproat Daily Expanded"),
            na        = "",
            col_types = "text") |>
    # Some years' headers have a stray leading space (e.g. " SkJk", " CnJk")
    # -- trim before selecting so this works regardless of which year's file
    # happens to have it, instead of relying on every source file's headers
    # being hand-edited to match.
    rename_with(trimws) |>
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

sproat_files <- tribble(
  ~year, ~file,
 # 2014,  "2014 stamp counts.xlsx",
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

map2(sproat_files$file, sproat_files$year, ~ {
  # resolve_sheet()/trimws() here match load_sproat_year() exactly -- keeps
  # this check honest about what will actually load, not just what the raw
  # sheet name and header look like before trimming.
  path    <- file.path(HIST_FOLDER, .x)
  nms     <- read_xlsx(path, sheet = resolve_sheet(path, "Sproat Daily Expanded"),
                       col_types = "text", n_max = 0) |> names() |> trimws()
  missing <- needed_cols[!needed_cols %in% nms]
  tibble(year         = .y,
         missing_cols = if (length(missing) == 0) "none" else
           paste(missing, collapse = ", "))
}) |>
  bind_rows() |>
  print(n = Inf)

# -----------------------------------------------------------------------------
# 7. LOAD & WRANGLE HISTORICAL DATA
#    Builds THREE parallel cumulative series per year: total Co, Co Mark,
#    and Co NoMark. All three get cum_count/ann_ttl/cum_prop AND all three
#    get filled/replace_na'd in the padding step below — this is the exact
#    fix for the earlier bug where cum_prop_mark/cum_prop_nomark were
#    missing from the padding step for a different Coho pipeline.
# -----------------------------------------------------------------------------
sproatHistPadded <- map2(sproat_files$file, sproat_files$year, load_sproat_year) |>
  bind_rows() |>
  rename(
    date      = `Review Date`,
    co_mark   = `Co  Mark`,
    co_nomark = `Co  NoMark`
  ) |>
  filter(!is.na(date)) |>
  mutate(
    julian    = safe_julian(date),
    Co        = if_else(is.na(Co), 0, Co),
    co_mark   = if_else(is.na(co_mark), 0, co_mark),
    co_nomark = if_else(is.na(co_nomark), 0, co_nomark)
  ) |>
  group_by(year) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    cum_count        = cumsum(Co),
    ann_ttl          = sum(Co, na.rm = TRUE),
    cum_prop         = cum_count / ann_ttl,
    cum_count_mark   = cumsum(co_mark),
    ann_ttl_mark     = sum(co_mark, na.rm = TRUE),
    cum_prop_mark    = cum_count_mark / ann_ttl_mark,
    cum_count_nomark = cumsum(co_nomark),
    ann_ttl_nomark   = sum(co_nomark, na.rm = TRUE),
    cum_prop_nomark  = cum_count_nomark / ann_ttl_nomark
  ) |>
  complete(julian = full_seq(c(julian, JULIAN_END), 1)) |>
  fill(cum_count, cum_prop,
       cum_count_mark, cum_prop_mark,
       cum_count_nomark, cum_prop_nomark,
       .direction = "down") |>
  replace_na(list(
    cum_count = 0, cum_prop = 0,
    cum_count_mark = 0, cum_prop_mark = 0,
    cum_count_nomark = 0, cum_prop_nomark = 0
  )) |>
  mutate(plot_date = as.Date(julian - 1, origin = "2001-01-01")) |>  # anchor-year proxy date, NOT a real calendar date
  ungroup()

# -----------------------------------------------------------------------------
# 8. SANITY CHECK
# -----------------------------------------------------------------------------
sproatHistPadded |>
  group_by(year) |>
  summarise(
    n_na_date       = sum(is.na(date)),
    max_julian_real = max(julian[!is.na(date)]),
    expected_pad    = JULIAN_END - max_julian_real,
    match           = n_na_date == expected_pad,
    .groups = "drop"
  ) |>
  print(n = Inf)

# -----------------------------------------------------------------------------
# 9. LOAD & WRANGLE CURRENT YEAR, from Graham's weekly update file
#    File source: plain path read (CURRENT_FILE, see CONFIG) — the
#    SharePoint folder is mapped/synced locally, so this is a normal
#    local/network file read, no API or extra package involved.
# -----------------------------------------------------------------------------
sproatCurrent <- read_xlsx(
  CURRENT_FILE,
  sheet = "Sproat CN&CO",
  na    = ""
) |>
  # trim stray header whitespace, same as load_sproat_year(), before
  # referencing the double-space "Co  NoMark" column by name
  rename_with(trimws) |>
  mutate(
    date      = as.Date(Date),
    julian    = safe_julian(date),
    co_nomark = as.numeric(`Co  NoMark`),
    year      = as.integer(CURRENT_YEAR)
  ) |>
  filter(!is.na(date)) |>
  arrange(date) |>
  mutate(cum_count_nomark = cumsum(replace_na(co_nomark, 0))) |>
  select(year, date, julian, co_nomark, cum_count_nomark)

# =============================================================================
# 10. SHARED PLOT ELEMENTS
# =============================================================================

ribbon_light   <- "#c9756e"    # below Sgen        — muted terracotta red
ribbon_mid     <- "#d4a843"    # Sgen–Smsy         — muted amber
ribbon_dark    <- "#74C69D"    # Smsy–Smax         — light green
ribbon_darkest <- "#1B4332"    # above Smax        — darkest green
hist_avg_colour <- "#4a6fa5"   # historic-average comparison line/ribbon

# Smooths the historic mean/ribbon boundaries across julian day. Coho's
# 5-95% range here is raw counts (unbounded), not proportions -- Chinook's
# logit_smooth() (ChinookEscapement2026.R) assumes a [0,1]-bounded value via
# a binomial GLM, which would be statistically invalid on unbounded counts.
# loess is the generic analogue: no boundedness assumption, same "smooth the
# noisy day-to-day historic estimate" purpose.
count_smooth <- function(y, x, span = 0.3) {
  predict(loess(y ~ x, span = span))
}

# =============================================================================
# 11. BENCHMARK RIDGE PLOT BUILDER — escapement vs. Sgen / Smsy by year
#
# =============================================================================

S_gen <- 1889   # from SR analysis by Pieter using RCH ERs
#The spawner abundance that, at current productivity, would be expected to produce enough recruits
#to reach Smsy in a single generation if all those recruits returned as spawners.
S_msy <- 3691   # The spawner abundance that maximizes long-term sustainable catch
S_max <- 9636 #The spawner abundance that produces the largest
#possible number of recruits

col_gen <- ribbon_light      # below Sgen
col_mid <- ribbon_mid        # Sgen–Smsy
col_msy <- ribbon_dark       # Smsy–Smax
col_max <- ribbon_darkest    # above Smax

# cum_count is monotonic non-decreasing within a season, so each threshold
# is crossed at most once; if never reached, returns NA rather than -Inf.
get_crossing <- function(julian, count, threshold) {
  idx <- which(count >= threshold)
  if (length(idx) == 0) return(NA_real_)          # benchmark never reached
  i <- idx[1]
  if (i == 1) return(julian[1])                    # already above at first obs
  j0 <- julian[i - 1]; j1 <- julian[i]
  c0 <- count[i - 1]; c1 <- count[i]
  if (c1 == c0) return(j1)
  j0 + (threshold - c0) / (c1 - c0) * (j1 - j0)
}

zone_data <- tibble(
  ymin = c(-Inf, S_gen, S_msy, S_max),
  ymax = c(S_gen, S_msy, S_max, Inf),
  zone = factor(
    c("Critical (< Sgen)", "Cautious (Sgen–Smsy)", "Healthy (Smsy–Smax)", "Above Smax"),
    levels = c("Critical (< Sgen)", "Cautious (Sgen–Smsy)", "Healthy (Smsy–Smax)", "Above Smax")
  )
)

# count_col:   name of the cumulative-count column to plot
#              (e.g. "cum_count_mark" or "cum_count_nomark")
# plot_title:  main title text
# file_out:    PNG filename to save to
build_ridge_plot <- function(hist_data, count_col, plot_title, file_out) {
  
  pd <- hist_data |>
    filter(!is.na(julian), julian >= 225) |>
    mutate(
      year      = factor(year),
      count_val = .data[[count_col]],
      zone = case_when(
        count_val < S_gen ~ "Critical (< Sgen)",
        count_val < S_msy ~ "Cautious (Sgen–Smsy)",
        count_val < S_max ~ "Healthy (Smsy–Smax)",
        TRUE              ~ "Above Smax"
      ),
      zone = factor(zone, levels = levels(zone_data$zone))
    )

  bench_dates <- pd |>
    group_by(year) |>
    arrange(julian, .by_group = TRUE) |>
    summarise(
      gen_julian = get_crossing(julian, count_val, S_gen),
      msy_julian = get_crossing(julian, count_val, S_msy),
      .groups = "drop"
    ) |>
    mutate(
      gen_count = S_gen,
      msy_count = S_msy,
      gen_label = if_else(is.na(gen_julian), "Not reached",
                          format(as.Date(gen_julian - 1, origin = "2001-01-01"), "%b %d")),
      msy_label = if_else(is.na(msy_julian), "Not reached",
                          format(as.Date(msy_julian - 1, origin = "2001-01-01"), "%b %d"))
    )
  
  p <- pd |>
    ggplot(aes(x = julian, y = count_val)) +
    geom_rect(
      data = zone_data,
      aes(ymin = ymin, ymax = ymax, fill = zone),
      xmin = -Inf, xmax = Inf,
      inherit.aes = FALSE, alpha = 0.15
    ) +
    scale_fill_manual(
      values = c(
        "Critical (< Sgen)"    = col_gen,
        "Cautious (Sgen–Smsy)" = col_mid,
        "Healthy (Smsy–Smax)"  = col_msy,
        "Above Smax"           = col_max
      ),
      name = NULL
    ) +
    geom_area(fill = "grey40", alpha = 0.12, colour = "#4B5563", linewidth = 0.5) +
    geom_hline(yintercept = S_gen, colour = col_gen, linewidth = 0.45, linetype = "dashed") +
    geom_hline(yintercept = S_msy, colour = col_msy, linewidth = 0.45, linetype = "dashed") +
    geom_hline(yintercept = S_max, colour = col_max, linewidth = 0.45, linetype = "dashed") +
    geom_point(
      data = filter(bench_dates, !is.na(gen_julian)),
      aes(x = gen_julian, y = gen_count), inherit.aes = FALSE,
      colour = col_gen, size = 2.2
    ) +
    geom_text(
      data = filter(bench_dates, !is.na(gen_julian)),
      aes(x = gen_julian, y = gen_count, label = gen_label), inherit.aes = FALSE,
      hjust = -0.12, vjust = 1.4, size = 2.9, colour = col_gen, fontface = "bold"
    ) +
    geom_point(
      data = filter(bench_dates, !is.na(msy_julian)),
      aes(x = msy_julian, y = msy_count), inherit.aes = FALSE,
      colour = col_msy, size = 2.2
    ) +
    geom_text(
      data = filter(bench_dates, !is.na(msy_julian)),
      aes(x = msy_julian, y = msy_count, label = msy_label), inherit.aes = FALSE,
      hjust = -0.12, vjust = -0.6, size = 2.9, colour = col_msy, fontface = "bold"
    ) +
    scale_x_continuous(
      limits = c(225, JULIAN_END), expand = c(0, 0),
      breaks = c(225, 250, 275, 300, 325),
      labels = format(as.Date(c(225, 250, 275, 300, 325) - 1, origin = "2001-01-01"), "%b %d")
    ) +
    scale_y_continuous(
      name = "Cumulative escapement",
      labels = scales::comma
    ) +
    facet_wrap(~ year, ncol = 2, strip.position = "top") +
    labs(
      x = "",
      title = plot_title,
      subtitle = "Dashed lines mark Sgen, Smsy and Smax benchmarks; labels show the date Sgen/Smsy were reached"
    ) +
    theme_classic() +
    theme(
      strip.placement       = "outside",
      strip.background      = element_blank(),
      strip.text            = element_text(size = 11, colour = "#c0703a", face = "bold", hjust = 0),
      panel.spacing         = unit(1, "lines"),
      axis.text.x           = element_text(size = 8, colour = "#333333"),
      axis.text.y           = element_text(size = 7.5, colour = "#333333"),
      axis.title.y          = element_text(size = 9.5, colour = "#333333"),
      axis.ticks.length     = unit(0.15, "cm"),
      plot.title             = element_text(size = 15, colour = "#333333", face = "bold"),
      plot.subtitle          = element_text(size = 9, colour = "#666666"),
      plot.background        = element_rect(fill = "white", colour = NA),
      legend.position         = "top",
      legend.justification    = "left",
      legend.margin           = margin(b = 5)
    )
  
  print(p)
  ggsave(file_out, p, width = 10, height = 12, dpi = 300)
  p
}

# -----------------------------------------------------------------------------
# 12. RIDGE PLOTS —  for Unmarked Coho
# -----------------------------------------------------------------------------


p_ridge_nomark <- build_ridge_plot(
  sproatHistPadded, "cum_count_nomark",
  "Sproat River Adult Coho (Unmarked) — Escapement by Year",
  "SproatCohoRidge_NoMark.png"
)

# =============================================================================
# ADD-ON: Sproat Coho Escapement — Current Year Only
#   Uses the same S_gen / S_msy benchmarks, zone shading, and crossing-date
#   logic as build_ridge_plot(), but for a single in-season year with no
#   facet_wrap (only one year) and no padding to JULIAN_END, since the
#   current season isn't complete yet — the line simply stops at the
#   latest available observation.
#
#   Depends on objects already defined earlier in the script:
#     S_gen, S_msy, zone_data, JULIAN_END, sproatCurrent, ribbon_light,
#     ribbon_mid, ribbon_dark (all set in section 10)
# =============================================================================

#   hist_data:   optional historic data frame (e.g. sproatHistPadded) used
#                to draw a historic-average comparison line + 5-95% shaded
#                range, in the same style as the Stamp Chinook timing plot's
#                historic-mean overlay. NULL (default) omits it entirely.
#   hist_years:  number of most recent historic years to average over;
#                NULL (default) uses every year available in hist_data.
build_current_plot <- function(current_data, count_col, plot_title, file_out,
                                hist_data = NULL, hist_years = NULL) {

  pd <- current_data |>
    filter(!is.na(julian)) |>
    arrange(julian) |>
    mutate(
      count_val = .data[[count_col]],
      zone = case_when(
        count_val < S_gen ~ "Critical (< Sgen)",
        count_val < S_msy ~ "Cautious (Sgen–Smsy)",
        count_val < S_max ~ "Healthy (Smsy–Smax)",
        TRUE              ~ "Above Smax"
      ),
      zone = factor(zone, levels = levels(zone_data$zone))
    )

  # start the x-axis at the first day count is actually > 0, not a fixed day
  start_julian <- min(pd$julian[pd$count_val > 0], na.rm = TRUE)
  x_breaks <- scales::breaks_pretty(n = 10)(c(start_julian, JULIAN_END))
  x_breaks <- x_breaks[x_breaks >= start_julian & x_breaks <= JULIAN_END]
  
  #label for the current count
  tip <- pd %>%  slice_max(julian, n=1, with_ties = FALSE)

  # historic-average comparison line + 5-95% range, computed on the same
  # count_col being plotted so current year and history are directly
  # comparable (e.g. cum_count vs cum_count, not cum_count vs cum_count_mark)
  hist_summary <- NULL
  hist_caption <- NULL
  if (!is.null(hist_data)) {
    curr_yr <- max(current_data$year, na.rm = TRUE)
    yr_lo   <- if (is.null(hist_years)) -Inf else curr_yr - hist_years

    hist_years_used <- hist_data |>
      filter(year >= yr_lo, year < curr_yr) |>
      distinct(year) |>
      pull(year) |>
      sort()

    hist_caption <- paste0(
      "Bands mark Sgen/Smsy/Smax zones; dashed blue line and shaded band show the ",
      min(hist_years_used), "–", max(hist_years_used),
      " historic mean and 5–95% range (loess-smoothed)"
    )

    hist_summary <- hist_data |>
      filter(year >= yr_lo, year < curr_yr,
             julian >= start_julian, julian <= JULIAN_END) |>
      group_by(julian) |>
      summarise(
        hist_mean = mean(.data[[count_col]], na.rm = TRUE),
        l95       = quantile(.data[[count_col]], 0.05, na.rm = TRUE),
        u95       = quantile(.data[[count_col]], 0.95, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(julian) |>
      mutate(
        hist_mean_smooth = count_smooth(hist_mean, julian),
        l95_smooth       = count_smooth(l95, julian),
        u95_smooth       = count_smooth(u95, julian)
      )
  }

  p <- pd |>
    ggplot(aes(x = julian, y = count_val)) +
    geom_rect(
      data = zone_data,
      aes(ymin = ymin, ymax = ymax, fill = zone),
      xmin = -Inf, xmax = Inf,
      inherit.aes = FALSE, alpha = 0.15
    ) +
    scale_fill_manual(
      values = c(
        "Critical (< Sgen)"    = ribbon_light,
        "Cautious (Sgen–Smsy)" = ribbon_mid,
        "Healthy (Smsy–Smax)"  = ribbon_dark,
        "Above Smax"           = ribbon_darkest
      ),
      name = NULL
    ) +
    geom_line(colour = "#333333", linewidth = 0.9) +
    { if (!is.null(hist_summary))
        geom_ribbon(
          data = hist_summary, aes(x = julian, ymin = l95_smooth, ymax = u95_smooth),
          inherit.aes = FALSE, fill = hist_avg_colour, alpha = 0.12
        )
    } +
    { if (!is.null(hist_summary))
        geom_line(
          data = hist_summary, aes(x = julian, y = hist_mean_smooth),
          inherit.aes = FALSE, colour = hist_avg_colour, linewidth = 0.9, linetype = "dashed"
        )
    } +
    geom_hline(yintercept = S_gen, colour = ribbon_light, linewidth = 0.45, linetype = "dashed") +
    geom_hline(yintercept = S_msy, colour = ribbon_dark, linewidth = 0.45, linetype = "dashed") +
    geom_hline(yintercept = S_max, colour = col_max, linewidth = 0.45, linetype = "dashed") +
    geom_point(data = tip, size=2, color="#333333") +
    geom_text(data = tip, aes(label=scales::comma(count_val)),
                              hjust=-0.15,vjust=0.5, size=3.3, color="#333333") +
    scale_x_continuous(
      limits = c(start_julian, JULIAN_END), expand = c(0, 0),
      breaks = x_breaks,
      labels = format(as.Date(x_breaks - 1, origin = "2001-01-01"), "%b %d")
    ) +
    scale_y_continuous(
      name = "Escapement",
      labels = scales::comma,
      limits = c(0, 12000),
      breaks = scales::breaks_pretty(n = 6)
    ) +
    labs(
      x = "",
      title = plot_title,
      subtitle = hist_caption
    ) +
    theme_classic() +
    theme(
      axis.text.x           = element_text(size = 9, colour = "#333333"),
      axis.text.y           = element_text(size = 8.5, colour = "#333333"),
      axis.title.y          = element_text(size = 10.5, colour = "#333333"),
      axis.ticks.length     = unit(0.15, "cm"),
      plot.title             = element_text(size = 15, colour = "#333333", face = "bold"),
      plot.subtitle          = element_text(size = 9.5, colour = "#666666"),
      plot.background        = element_rect(fill = "white", colour = NA),
      legend.position         = "top",
      legend.justification    = "left",
      legend.margin           = margin(b = 5)
    )
  
  print(p)
  ggsave(file_out, p, width = 9, height = 5.5, dpi = 300)
  p
}

# -----------------------------------------------------------------------------
# Call it
# -----------------------------------------------------------------------------
p_current_coho <- build_current_plot(
  sproatCurrent, "cum_count_nomark",
  "Sproat River Adult Unmarked Coho",
  "SproatCoho_Current2026.png",
  hist_data = sproatHistPadded
)

# -----------------------------------------------------------------------------
# Probability of reaching smsy by Mid-September
# -----------------------------------------------------------------------------
smsy_crossings <- sproatHistPadded |>
  group_by(year) |>
  arrange(julian, .by_group = TRUE) |>
  summarise(smsy_julian = get_crossing(julian, cum_count_nomark, S_msy),
            .groups = "drop")

sep15_julian <- as.numeric(format(as.Date("2001-09-15"), "%j"))  # = 258

plot_dat <- smsy_crossings |>
  mutate(status = ifelse(is.na(smsy_julian), "Never reached", "Reached"),
         plot_julian = ifelse(is.na(smsy_julian), JULIAN_END, smsy_julian))

ggplot(plot_dat, aes(x = plot_julian, y = factor(year), color = status)) +
  geom_point(size = 3) +
  geom_vline(xintercept = sep15_julian, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = seq(260, 340, by = 20)) +   # only these get labels + major gridlines
  labs(x = "Day of year", y = "", title = "Smsy crossing date by year",
       subtitle = "Vertical dashed line is September 15") +
  theme(plot.subtitle = element_text(size = 9),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "grey40"))
        