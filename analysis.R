# ============================================================
# Opening Day Ticket Sales Changes — Analysis Script
# ============================================================
#
# This script generates synthetic ticket sales data for a
# professional baseball team and produces all analysis and
# visualizations for the Ticket Sales Changes Report.
#
# The analysis examines how strategic pricing changes for
# Opening Day affected revenue, volume, sales pacing, and
# site traffic compared to prior seasons.
#
# All data is synthetically generated for portfolio purposes.
# The analytical patterns mirror a real project conducted
# for an MLB franchise.
#
# Usage:
#   source("analysis.R")
#   # Then knit TicketSalesChangesReport.Rmd for the PDF
# ============================================================

# --- Libraries ---
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(forcats)

# --- Set working directory to this script's location ---
# Ensures data/ and images/ are created alongside the script
# regardless of where source() is called from.
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else if (!is.null(sys.frames())) {
  # Fallback: detect path when run via source()
  script_dir <- tryCatch(
    dirname(sys.frame(1)$ofile),
    error = function(e) NULL)
  if (!is.null(script_dir) && nchar(script_dir) > 0)
    setwd(script_dir)
}

set.seed(2026)

# --- Output directories ---
dir.create("data", showWarnings = FALSE)
dir.create("images", showWarnings = FALSE)


# ============================================================
# HELPER FUNCTIONS
# ============================================================

#' Generate daily hit counts following a bell-shaped distribution.
#' Uses the derivative of a logistic function for realistic
#' cumulative traffic curves when passed through cumsum().
#'
#' @param n         Number of days
#' @param total     Target total hits across all days
#' @param midpoint  Inflection point on 0-1 scale (earlier = front-loaded)
#' @param steepness Controls how concentrated the peak is
#' @param noise_sd  Standard deviation of multiplicative noise
#' @return Integer vector of daily hit counts summing to ~total
generate_daily_hits <- function(n, total, midpoint = 0.5,
                                steepness = 8, noise_sd = 0.2) {
  x <- seq(0, 1, length.out = n)
  rate <- steepness * exp(-steepness * (x - midpoint)) /
    (1 + exp(-steepness * (x - midpoint)))^2
  daily <- rate * (1 + rnorm(n, 0, noise_sd))
  daily <- pmax(daily, 0)
  round(daily / sum(daily) * total)
}


# ============================================================
# SECTION 1: GENERATE TABLE DATA (exported as CSVs for report)
# ============================================================
cat("Generating synthetic data...\n")

# ---- 1.1 Ticket Class Distribution ----
# Volume and revenue by ticket class: 2025 vs 2026
ticket_group_table <- tibble(
  ticket_group = c("Season Tickets", "Hospitality", "Group",
                    "Single Secondary", "Mini", "Single Primary",
                    "Comps", "Total"),
  seats_2025 = c(20500, 1750, 2050, 7500, 240, 9300, 1000, 42340),
  seats_2026 = c(16200, 1650, 1580, 10100, 470, 9100, 980, 40080),
  revenue_2025 = c(1820000, 198000, 125000, 440000, 16500,
                    530000, 0, 3129500),
  revenue_2026 = c(2100000, 220000, 117000, 780000, 27000,
                    815000, 0, 4059000)
) %>% mutate(
  seats_chg_pct = round(
    (seats_2026 - seats_2025) / seats_2025 * 100, 2),
  revenue_chg_pct = ifelse(
    revenue_2025 == 0, NA,
    round((revenue_2026 - revenue_2025) / revenue_2025 * 100, 2))
)

# ---- 1.2 Overall Average Pricing by Season (Non-Premium, Single Game) ----
avg_price_overall <- tibble(
  season         = c("2023",  "2024",  "2025",  "2026"),
  avg_price_per_seat = c(45.50,  62.75,  60.20,  97.80),
  total_seats    = c(9800,   5200,   7500,   6400)
)

# ---- 1.3 Average Price by Sale Month ----
avg_price_by_month <- tibble(
  season = c(rep("2023", 5), rep("2024", 4),
             rep("2025", 5), rep("2026", 3)),
  sale_month = c("Nov","Dec","Jan","Feb","Mar",
                 "Dec","Jan","Feb","Mar",
                 "Nov","Dec","Jan","Feb","Mar",
                 "Jan","Feb","Mar"),
  avg_price_per_seat = c(
    44.20, 62.50, 58.30, 70.00, 38.50,
    61.40, 89.00, 44.80, 72.30,
    63.10, 55.80, 41.20, 82.50, 56.40,
    115.60, 110.20, 83.70),
  total_seats = c(
    6200, 110, 72, 18, 1600,
    2900, 5, 550, 1500,
    1900, 900, 1600, 350, 1750,
    2050, 1000, 3350)
)

# ---- 1.4 Single Game Price Changes by Price Location ----
price_change <- tibble(
  DA.Price.Location.Name = c(
    "Grandstand Corner", "Grandstand Reserved", "Lexus Infield 1B",
    "Vista Reserved 1B", "Grandstand Infield", "Vista Corner 3B",
    "Vista Reserved 3B", "Vista Corner 1B", "Diamond Infield 1B",
    "Coca Cola Corner", "Home Run Porch Low", "Diamond Corner 1B",
    "Home Run Porch High", "Diamond Reserved 1B", "Diamond Reserved 3B",
    "Dugout Infield 1B", "Diamond Infield 3B", "Vista Infield 3B",
    "Vista Infield 1B", "Dugout Reserved 3B", "Dugout Corner 1B",
    "Diamond Corner 3B", "Lexus Reserved 1B", "Lexus Infield 3B",
    "Vista Home Plate", "Dugout Reserved 1B", "Lexus Reserved 3B",
    "Terrace Corner", "Standing Room Only", "Dugout Corner 3B",
    "Dugout Infield 3B"),
  avg_price_per_seat_2025 = c(
    25, 32, 128, 58, 42, 43, 53, 45, 160, 47,
    94, 124, 88, 155, 152, 272, 178, 70, 75, 228,
    182, 133, 146, 168, 78, 257, 142, 118, 42, 232, 342),
  avg_price_per_seat_2026 = c(
    48.75, 56.00, 204.80, 89.90, 63.84, 64.50,
    78.44, 65.70, 220.80, 63.45, 124.08, 155.00,
    107.36, 189.10, 182.40, 320.96, 210.04, 81.20,
    85.50, 246.24, 192.92, 135.66, 143.08, 162.96,
    74.10, 246.72, 127.80, 103.84, 35.70, 190.24, 273.60)
) %>% mutate(
  price_chg_pct = round(
    (avg_price_per_seat_2026 - avg_price_per_seat_2025) /
      avg_price_per_seat_2025 * 100, 2)
) %>% arrange(-price_chg_pct)

# ---- 1.5 Season Ticket Average Prices by Price Location ----
# Season ticket value attributed to Opening Day, by price location
set.seed(42)
season_ticket_avg_price <- tibble(
  DA.Price.Location.Name = price_change$DA.Price.Location.Name,
  `2025` = round(price_change$avg_price_per_seat_2025 *
                   runif(nrow(price_change), 0.45, 0.70), 2)
)
season_ticket_avg_price$`2026` <- round(
  season_ticket_avg_price$`2025` *
    runif(nrow(season_ticket_avg_price), 1.30, 1.85), 2)
season_ticket_avg_price <- season_ticket_avg_price %>%
  mutate(price_chg_pct = round(
    (`2026` - `2025`) / `2025` * 100, 2)) %>%
  arrange(-price_chg_pct)
set.seed(2026)

# ---- 1.6 Season Ticket Overall ----
season_ticket_overall <- tibble(
  year               = c("2025",  "2026"),
  avg_price_per_seat  = c(72.50,  112.30),
  total_seats         = c(15600,  11500)
)


# ============================================================
# SECTION 2: EXPORT CSVs
# ============================================================
cat("Exporting CSVs to data/\n")

write.csv(ticket_group_table,     "data/ticket_group_table.csv",
          row.names = FALSE)
write.csv(avg_price_overall,      "data/avg_price_overall.csv",
          row.names = FALSE)
write.csv(avg_price_by_month,     "data/avg_price_by_month.csv",
          row.names = FALSE)
write.csv(price_change,           "data/price_change.csv",
          row.names = FALSE)
write.csv(season_ticket_avg_price,"data/season_ticket_avg_price.csv",
          row.names = FALSE)
write.csv(season_ticket_overall,  "data/season_ticket_overall.csv",
          row.names = FALSE)


# ============================================================
# SECTION 3: SITE TRAFFIC VISUALIZATIONS
# ============================================================
cat("Creating site traffic visualizations...\n")

# Dates are normalized to a common dummy-year frame for overlay:
#   Nov-Dec -> year 2000, Jan-Mar -> year 2001
all_dates <- seq(as.Date("2000-11-01"),
                 as.Date("2001-03-27"), by = "day")
jan12 <- as.Date("2001-01-12")

# ---- 3.1 Full Season Cumulative Hits (All Games) ----
# 2026 generally trails prior seasons in overall site traffic
full_daily <- bind_rows(
  tibble(dummy_date = all_dates, season = "2023",
         daily_hits = generate_daily_hits(
           length(all_dates), 210000, 0.45, 7)),
  tibble(dummy_date = all_dates, season = "2024",
         daily_hits = generate_daily_hits(
           length(all_dates), 185000, 0.50, 6)),
  tibble(dummy_date = all_dates, season = "2025",
         daily_hits = generate_daily_hits(
           length(all_dates), 230000, 0.40, 8)),
  tibble(dummy_date = all_dates, season = "2026",
         daily_hits = generate_daily_hits(
           length(all_dates), 160000, 0.55, 5))
) %>%
  arrange(season, dummy_date) %>%
  group_by(season) %>%
  mutate(cumulative_hits = cumsum(daily_hits)) %>%
  ungroup()

p_full_season <- ggplot(full_daily,
    aes(x = dummy_date, y = cumulative_hits, color = season)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Cumulative Hit Count by Season (Nov - Mar) - All Event Dates",
    x = "Date", y = "Cumulative Hit Count", color = "Season") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        title = element_text(size = 10))

ggsave("images/full_season_hits.png", p_full_season,
       width = 10, height = 6, dpi = 300)


# ---- 3.2 Opening Day Cumulative Hits ----
# 2026 Opening Day didn't go on sale until Jan 12 (zero hits before)
od_2026_pre <- tibble(
  dummy_date = all_dates[all_dates < jan12],
  season = "2026", daily_hits = 0)

od_2026_post_dates <- all_dates[all_dates >= jan12]
od_2026_post <- tibble(
  dummy_date = od_2026_post_dates, season = "2026",
  daily_hits = generate_daily_hits(
    length(od_2026_post_dates), 32000, 0.35, 6))

od_daily <- bind_rows(
  tibble(dummy_date = all_dates, season = "2023",
         daily_hits = generate_daily_hits(
           length(all_dates), 36000, 0.45, 6)),
  tibble(dummy_date = all_dates, season = "2024",
         daily_hits = generate_daily_hits(
           length(all_dates), 27000, 0.50, 7)),
  tibble(dummy_date = all_dates, season = "2025",
         daily_hits = generate_daily_hits(
           length(all_dates), 40000, 0.40, 8)),
  bind_rows(od_2026_pre, od_2026_post)
) %>%
  arrange(season, dummy_date) %>%
  group_by(season) %>%
  mutate(cumulative_hits = cumsum(daily_hits)) %>%
  ungroup()

p_od_hits <- ggplot(od_daily,
    aes(x = dummy_date, y = cumulative_hits, color = season)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Opening Day - Primary Market Cumulative Hit Count",
    x = "Date", y = "Cumulative Hit Count", color = "Season") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("images/opening_day_hits.png", p_od_hits,
       width = 10, height = 6, dpi = 300)


# ---- 3.3 Opening Day Hits from January 12th Forward ----
# Shows traffic growth from the 2026 on-sale date onward
od_jan12 <- od_daily %>%
  filter(dummy_date >= jan12) %>%
  arrange(season, dummy_date) %>%
  group_by(season) %>%
  mutate(cumulative_hits = cumsum(daily_hits)) %>%
  ungroup()

p_od_jan12 <- ggplot(od_jan12,
    aes(x = dummy_date, y = cumulative_hits, color = season)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Opening Day - Cumulative Hit Count January 12th on",
    x = "Date", y = "Cumulative Hit Count", color = "Season") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("images/opening_day_hits_jan12.png", p_od_jan12,
       width = 10, height = 6, dpi = 300)


# ============================================================
# SECTION 4: CONVERSION RATE VISUALIZATION
# ============================================================
cat("Creating conversion rate visualization...\n")

# Monthly conversion rate = unique buyers / unique visitors
# for the Opening Day ticket page
dummy_months <- as.Date(c("2000-12-01", "2001-01-01",
                          "2001-02-01", "2001-03-01"))

conversion_data <- bind_rows(
  tibble(dummy_month = dummy_months, season = "2023",
         conversion_rate = c(0.020, 0.025, 0.030, 0.032)),
  tibble(dummy_month = dummy_months, season = "2024",
         conversion_rate = c(0.015, 0.022, 0.028, 0.045)),
  tibble(dummy_month = dummy_months, season = "2025",
         conversion_rate = c(0.028, 0.055, 0.035, 0.042)),
  # 2026: No Dec data (on-sale Jan 12); high Jan, drops as inventory shrinks
  tibble(dummy_month = dummy_months[-1], season = "2026",
         conversion_rate = c(0.062, 0.038, 0.028))
)

p_conv <- ggplot(conversion_data,
    aes(x = dummy_month, y = conversion_rate,
        color = season, group = season)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Opening Day - Monthly Conversion Rate by Season (Dec - Mar)",
    x = "Month", y = "Conversion Rate", color = "Season") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("images/conversion_rate_plot.png", p_conv,
       width = 10, height = 6, dpi = 300)


# ============================================================
# SECTION 5: SALES PACING VISUALIZATIONS
# ============================================================
cat("Creating sales pacing visualizations...\n")

# Sales pacing shows % of section capacity sold at snapshots
# taken at 120, 90, 60, 30, and 0 days before Opening Day.
# In prior seasons, tickets went on sale in November.
# In 2026, Opening Day single-game tickets went on sale Jan 12.
days_before <- c(120, 90, 60, 30, 0)
pacing_seasons <- c("2022", "2023", "2024", "2025", "2026")

# Helper to build a pacing dataset for one price location
build_pacing <- function(location, pct_matrix) {
  tibble(
    DA.Price.Location.Name = location,
    season = rep(pacing_seasons, each = 5),
    days_before_opening = rep(days_before, 5),
    pct_capacity_sold = as.vector(t(pct_matrix))
  )
}

# ---- 5.1 Grandstand Corner ----
# Historically sells out fast; 2026 much slower but catches up
gc_pacing <- build_pacing("Grandstand Corner", matrix(c(
  0.65, 0.78, 0.88, 0.94, 0.97,
  0.72, 0.85, 0.93, 0.97, 0.99,
  0.68, 0.82, 0.90, 0.95, 0.98,
  0.60, 0.76, 0.88, 0.94, 0.97,
  0.05, 0.24, 0.55, 0.80, 0.96
), nrow = 5, byrow = TRUE))

gc_info <- price_change %>%
  filter(DA.Price.Location.Name == "Grandstand Corner")

p_gc <- ggplot(gc_pacing,
    aes(x = days_before_opening, y = pct_capacity_sold,
        color = season, group = season)) +
  geom_line() + geom_point() +
  scale_x_reverse(breaks = c(120, 90, 60, 30, 0)) +
  scale_y_continuous(labels = percent) +
  labs(
    title = paste0("Grandstand Corner | Avg Price Change 2025 to 2026: ",
                   gc_info$price_chg_pct, "%"),
    x = "Days Before Opening Day",
    y = "% Capacity Sold", color = "Season") +
  theme_minimal()

ggsave("images/grandstand_corner.png", p_gc,
       width = 10, height = 6, dpi = 300)


# ---- 5.2 Vista Corner 3B ----
# Sold later in 2026 but nearly sold out
vc3b_pacing <- build_pacing("Vista Corner 3B", matrix(c(
  0.55, 0.70, 0.82, 0.90, 0.95,
  0.60, 0.75, 0.85, 0.92, 0.97,
  0.58, 0.72, 0.83, 0.91, 0.96,
  0.52, 0.68, 0.80, 0.90, 0.95,
  0.03, 0.18, 0.45, 0.72, 0.93
), nrow = 5, byrow = TRUE))

vc3b_info <- price_change %>%
  filter(DA.Price.Location.Name == "Vista Corner 3B")

p_vc3b <- ggplot(vc3b_pacing,
    aes(x = days_before_opening, y = pct_capacity_sold,
        color = season, group = season)) +
  geom_line() + geom_point() +
  scale_x_reverse(breaks = c(120, 90, 60, 30, 0)) +
  scale_y_continuous(labels = percent) +
  labs(
    title = paste0("Vista Corner 3B | Avg Price Change 2025 to 2026: ",
                   vc3b_info$price_chg_pct, "%"),
    x = "Days Before Opening Day",
    y = "% Capacity Sold", color = "Season") +
  theme_minimal()

ggsave("images/vista_corner_3b.png", p_vc3b,
       width = 10, height = 6, dpi = 300)


# ---- 5.3 Dugout Infield 3B ----
# Significant selling in the final 30 days
di3b_pacing <- build_pacing("Dugout Infield 3B", matrix(c(
  0.40, 0.52, 0.65, 0.82, 0.94,
  0.45, 0.58, 0.70, 0.85, 0.96,
  0.42, 0.55, 0.68, 0.83, 0.95,
  0.38, 0.50, 0.62, 0.80, 0.93,
  0.02, 0.12, 0.35, 0.65, 0.92
), nrow = 5, byrow = TRUE))

di3b_info <- price_change %>%
  filter(DA.Price.Location.Name == "Dugout Infield 3B")

p_di3b <- ggplot(di3b_pacing,
    aes(x = days_before_opening, y = pct_capacity_sold,
        color = season, group = season)) +
  geom_line() + geom_point() +
  scale_x_reverse(breaks = c(120, 90, 60, 30, 0)) +
  scale_y_continuous(labels = percent) +
  labs(
    title = paste0("Dugout Infield 3B | Avg Price Change 2025 to 2026: ",
                   di3b_info$price_chg_pct, "%"),
    x = "Days Before Opening Day",
    y = "% Capacity Sold", color = "Season") +
  theme_minimal()

ggsave("images/dugout_infield_3b.png", p_di3b,
       width = 10, height = 6, dpi = 300)


# ---- 5.4 Lexus Reserved 1B ----
# Later sales in 2026 but achieved similar final sell-through
lr1b_pacing <- build_pacing("Lexus Reserved 1B", matrix(c(
  0.50, 0.62, 0.75, 0.88, 0.94,
  0.55, 0.68, 0.80, 0.90, 0.96,
  0.52, 0.65, 0.78, 0.89, 0.95,
  0.48, 0.60, 0.74, 0.87, 0.94,
  0.03, 0.15, 0.42, 0.70, 0.92
), nrow = 5, byrow = TRUE))

lr1b_info <- price_change %>%
  filter(DA.Price.Location.Name == "Lexus Reserved 1B")

p_lr1b <- ggplot(lr1b_pacing,
    aes(x = days_before_opening, y = pct_capacity_sold,
        color = season, group = season)) +
  geom_line() + geom_point() +
  scale_x_reverse(breaks = c(120, 90, 60, 30, 0)) +
  scale_y_continuous(labels = percent) +
  labs(
    title = paste0("Lexus Reserved 1B | Avg Price Change 2025 to 2026: ",
                   lr1b_info$price_chg_pct, "%"),
    x = "Days Before Opening Day",
    y = "% Capacity Sold", color = "Season") +
  theme_minimal()

ggsave("images/lexus_reserved_1b.png", p_lr1b,
       width = 10, height = 6, dpi = 300)


# ============================================================
# SECTION 6: SEASON TICKET PROPORTION VISUALIZATION
# ============================================================
cat("Creating season ticket proportion visualization...\n")

# Shows what fraction of each price location's tickets are
# season tickets. Most locations decreased in 2026; a couple
# increased slightly (Dugout Infield 3B, Diamond Infield 1B).
st_prop_locations <- c(
  "Diamond Corner 1B", "Diamond Corner 3B",
  "Diamond Infield 1B", "Diamond Infield 3B",
  "Diamond Reserved 1B", "Diamond Reserved 3B",
  "Dugout Corner 1B", "Dugout Corner 3B",
  "Dugout Infield 1B", "Dugout Infield 3B",
  "Dugout Reserved 1B", "Dugout Reserved 3B",
  "Home Run Porch High", "Home Run Porch Low",
  "Lexus Infield 1B", "Lexus Infield 3B",
  "Lexus Reserved 1B", "Lexus Reserved 3B",
  "Terrace Corner",
  "Vista Corner 1B", "Vista Corner 3B",
  "Vista Home Plate",
  "Vista Infield 1B", "Vista Infield 3B",
  "Vista Reserved 1B", "Vista Reserved 3B"
)

set.seed(99)
n_locs <- length(st_prop_locations)
st_2025 <- runif(n_locs, 0.25, 0.75)

# Most locations decrease; force a couple to increase
st_diffs <- rnorm(n_locs, -0.10, 0.08)
st_diffs[which(st_prop_locations == "Dugout Infield 3B")]  <-  0.06
st_diffs[which(st_prop_locations == "Diamond Infield 1B")] <-  0.05
st_2026 <- pmax(st_2025 + st_diffs, 0.05)

st_prop_wide <- tibble(
  DA.Price.Location.Name = st_prop_locations,
  `2025` = st_2025,
  `2026` = st_2026
) %>% mutate(diff = `2026` - `2025`)

st_prop_plot_data <- st_prop_wide %>%
  pivot_longer(cols = c(`2025`, `2026`),
               names_to  = "season",
               values_to = "pct_season_tickets") %>%
  mutate(
    is_2026 = season == "2026",
    DA.Price.Location.Name = fct_reorder(
      DA.Price.Location.Name, diff)
  )

p_st_prop <- ggplot(st_prop_plot_data,
    aes(x = pct_season_tickets, y = DA.Price.Location.Name,
        color = season, size = is_2026)) +
  geom_point() +
  scale_size_manual(values = c("TRUE" = 5, "FALSE" = 2),
                    guide = "none") +
  scale_x_continuous(labels = percent) +
  labs(
    title = paste0("Season Ticket Proportion on Opening Day ",
                   "by Price Location (2025 vs 2026)"),
    x = "% Season Tickets", y = NULL, color = "Season") +
  theme_minimal()

ggsave("images/opening_day_season_pct_2526.png", p_st_prop,
       width = 10, height = 6, dpi = 300)

set.seed(2026)


# ============================================================
# SECTION 7: PRICE CHANGE vs SALES CHANGE SCATTER PLOT
# ============================================================
cat("Creating price vs. sales scatter plot...\n")

# Despite large price increases, most price locations achieved
# similar or only slightly lower sell-through rates by Opening Day.
set.seed(77)
price_vs_sales <- price_change %>%
  filter(DA.Price.Location.Name != "Standing Room Only") %>%
  mutate(
    pct_sold_2025 = runif(n(), 0.82, 0.99),
    pct_sold_2026 = pmin(
      pct_sold_2025 + rnorm(n(), -0.01, 0.04), 1.0),
    sales_pace_chg_pct = (pct_sold_2026 - pct_sold_2025) * 100
  )

p_pvs <- ggplot(price_vs_sales,
    aes(x = price_chg_pct, y = sales_pace_chg_pct,
        label = DA.Price.Location.Name)) +
  geom_point() +
  geom_text(vjust = -0.5, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Price Change vs Sales Change: Opening Day 2025 to 2026",
    x = "% Change in Avg Ticket Price",
    y = "% Change in Capacity Sold") +
  theme_minimal()

ggsave("images/price_vs_sales.png", p_pvs,
       width = 10, height = 6, dpi = 300)

set.seed(2026)


# ============================================================
cat("\nAll visualizations saved to images/\n")
cat("All CSVs saved to data/\n")
cat("Run: rmarkdown::render('TicketSalesChangesReport.Rmd')\n")
cat("Analysis complete.\n")
