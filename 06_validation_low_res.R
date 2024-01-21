# Title: Validate the Final Algorithm with the Low-Resolution Data
#
# Purpose: Loading low-resolution validation data (refer to [data](#data)) and
# conducting algorithm validation, this script produces figure 13 and provides
# data for table 3.
#
# Data files:
#   - data/06_low_res_validation.csv
#
# Function files:
#   - functions/fn_custom_cramer_statistic.R
#   - functions/cvrmse_measure.R
#   - functions/fn_validation.R
#
# Author: M. Schaffer Contact details: msch@build.aau.dk

# packages ----------------------------------------------------------------
library(mlr3)
require(mlr3tuning)
library(mlr3learners)
library(fst)
library(purrr)
library(lubridate)
library(data.table)
library(ggplot2)
library(patchwork)
library(cramer.statistic)

# Models
library(ranger)

source("functions/fn_validation.R")


# Load data ---------------------------------------------------------------

data_dt <- fread("data/06_low_res_validation.csv")
data_dt[, reading_time := with_tz(reading_time, "Europe/Copenhagen")]

# Add features ------------------------------------------------------------
# needed to get weekdays in English
Sys.setlocale("LC_TIME", "English")
# Time dependent features
data_dt[, `:=`(
  time_hour_cos = cos(hour(reading_time) * (2 * pi / 24)),
  time_hour_sin = sin(hour(reading_time) * (2 * pi / 24)),
  time_month_cos = cos((month(reading_time) - 1) * (2 * pi / 12)),
  time_month_sin = sin((month(reading_time) - 1) * (2 * pi / 12)),
  time_yday_cos = cos((yday(reading_time) - 1) * (2 * pi / 365)),
  time_yday_sin = sin((yday(reading_time) - 1) * (2 * pi / 365)),
  time_is_weekend = as.numeric(grepl("S(at|un)", weekdays(reading_time))),
  time_week_day_cos = cos((lubridate::wday(reading_time, label = FALSE, week_start = 1) - 1) * (2 * pi / 7)),
  time_week_day_sin = sin((lubridate::wday(reading_time, label = FALSE, week_start = 1) - 1) * (2 * pi / 7))
)]


# Customer specific features
walk(1:2, ~ data_dt[, paste0("demand_spms_lag_", .x) := shift(demand_spms, type = "lag", n = .x), by = .(house_nr, group)])
walk(1:2, ~ data_dt[, paste0("demand_spms_lead_", .x) := shift(demand_spms, type = "lead", n = .x), by = .(house_nr, group)])

# data_dt[, reading_time := NULL]
data_dt <- na.omit(data_dt)
data_lst <- split(data_dt, by = "house_nr")

# Similarity testing ------------------------------------------------------

similarity_result <- map(data_lst, ~ fn_similarity(data = .x)) |>
  rbindlist()

# Disaggrgation --------------------------------------------------------------

regression_result <- map(data_lst, ~ lgr::without_logging(fn_disaggrgate(data = .x, pg = pg))) |>
  rbindlist()

# Result evaluation -------------------------------------------------------

# Merge results
result <- merge(regression_result, similarity_result, by = c("house_nr"))

# Consider only full weeks
result[, week := paste0(year(reading_time), "_", isoweek(reading_time))]
full_weeks <- result[, .N, by = .(house_nr, group, week)][N == 168]
result <- result[paste0(house_nr, group, week) %in% full_weeks[, paste0(house_nr, group, week)]]

# Aggregate to weekly results to reduce uncertainty in ground truth data
weekly_results <- result[, .(
  sh_calculated = sum(sh_calculated),
  dhw_calculated = sum(dhw_calculated),
  total_energy_kwh = sum(total_energy_kwh_demand),
  dhw_energy_kwh = sum(dhw_energy_kwh_demand)
), by = .(house_nr, group, week)]
weekly_results[, sh_energy_kwh := total_energy_kwh - dhw_energy_kwh]

# Evaluate
cvrmse <- function(truth, response) {
  sqrt(mean((truth - response)^2)) / mean(truth)
}
bias <- function(truth, response) {
  mean(truth - response)
}
pbias <- function(truth, response) {
  mean((truth - response) / abs(truth))
}


## Plot results -----------------------------------------------------------
plots <- list()

# Scatter plot of actual vs calculated for SH and DHW
plots[["SH"]] <- ggplot(weekly_results, aes(x = sh_energy_kwh, y = sh_calculated)) +
  geom_point(size = 0.4) +
  geom_abline(intercept = 0, linewidth = 0.1) +
  coord_cartesian(xlim = c(0, 700), ylim = c(0, 700)) +
  facet_grid(cols = vars(house_nr)) +
  labs(x = "true SH", y = "calculated SH") +
  theme_nice() +
  theme(
    strip.background = element_rect(colour = "black", fill = "white"),
    aspect.ratio = 1
  )

plots[["DHW"]] <- ggplot(weekly_results, aes(x = dhw_energy_kwh, y = dhw_calculated)) +
  geom_point(size = 0.4) +
  geom_abline(intercept = 0, linewidth = 0.1) +
  facet_grid(cols = vars(house_nr)) +
  coord_cartesian(xlim = c(0, 175), ylim = c(0, 175)) +
  labs(x = "true DHW", y = "calculated DHW") +
  theme_nice() +
  theme(
    strip.background = element_rect(colour = "black", fill = "white"),
    aspect.ratio = 1
  )

plots[["final"]] <- plots[["SH"]] / plots[["DHW"]] +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 8, face = "bold"))

ggsave(
  "plots/06_Figure_13",
  plots[["final"]],
  device = cairo_pdf,
  width = 88,
  height = 100,
  unit = "mm"
)
