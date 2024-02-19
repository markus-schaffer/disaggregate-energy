# Title: Validate the Final Algorithm with the High-Resolution Data
#
# Purpose: Loading high-resolution validation data (refer to [data](#data)) and
# conducting algorithm validation, this script produces figures 14 and 15, along
# with data for table 4.
#
# Data files:
#   - data/07_high_res_validation.csv
#
# Function files:
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
library(data.table)
library(ggplot2)
library(patchwork)
library(cramer)
library(mgsub)

# Models
library(ranger)

source("functions/fn_validation.R")


# Load data ---------------------------------------------------------------

data_dt <- fread("data/07_high_res_validation.csv")
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
data_dt <- na.omit(data_dt)
data_lst <- split(data_dt, by = "house_nr")

# Similarity testing ------------------------------------------------------

similarity_result <- map(data_lst, ~ fn_similarity(data = .x)) |> rbindlist()

# Disaggrgation --------------------------------------------------------------

regression_result <- map(data_lst, ~ lgr::without_logging(fn_disaggrgate(data = .x))) |> rbindlist()

# Result evaluation -------------------------------------------------------

# Merge results
result <- merge(regression_result, similarity_result, by = c("house_nr"))

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

### Overview scatter plots ------------------------------------------------
dummy_sh <- data.table(
  house_nr = c(1, 1, 2, 2, 3, 3),
  heating_energy_kwh_demand = c(0, 7.5, 0, 7.5, 0, 7.5),
  sh_calculated = c(0, 7.5, 0, 7.5, 0, 7.5)
)

# Scatter plot of actual vs calculated for SH and DHW
plots[["SH"]] <- ggplot(result, aes(x = heating_energy_kwh_demand, y = sh_calculated)) +
  geom_point(size = 0.2, alpha = 0.25) +
  geom_abline(intercept = 0, linewidth = 0.1) +
  # geom_blank(data = dummy_sh) +
  facet_grid(cols = vars(house_nr)) +
  coord_cartesian(xlim = c(0, 8), ylim = c(0, 8)) +
  scale_x_continuous(n.breaks = 5) +
  scale_y_continuous(n.breaks = 5) +
  labs(x = "true SH", y = "calculated SH") +
  theme_nice() +
  theme(
    strip.background = element_rect(colour = "black", fill = "white"),
    aspect.ratio = 1,
    panel.spacing.y = unit(5, "mm")
  )

plots[["DHW"]] <- ggplot(result, aes(x = dhw_energy_kwh_demand, y = dhw_calculated)) +
  geom_point(size = 0.2, alpha = 0.25) +
  geom_abline(intercept = 0, linewidth = 0.1) +
  facet_grid(cols = vars(house_nr)) +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 12)) +
  scale_x_continuous(n.breaks = 7) +
  scale_y_continuous(n.breaks = 7) +
  labs(x = "true DHW", y = "calculated DHW") +
  theme_nice() +
  theme(
    strip.background = element_rect(colour = "black", fill = "white"),
    aspect.ratio = 1,
    panel.spacing.y = unit(5, "mm")
  )


plots[["final_overview"]] <- plots[["SH"]] / plots[["DHW"]] +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 8, face = "bold"))

ggsave(
  "plots/07_Figure_14.pdf",
  plots[["final"]],
  device = final_overview,
  width = 120,
  height = 100,
  unit = "mm"
)


### Detailed plots --------------------------------------------------------

# Select summer and winter days
summer_dates <- c(as.Date("2011-05-22", tz = "Europe/Copenhagen"), as.Date("2011-05-23", tz = "Europe/Copenhagen"))
winter_dates <- c(as.Date("2011-01-16", tz = "Europe/Copenhagen"), as.Date("2011-01-17", tz = "Europe/Copenhagen"))
plot_dt <- result[
  (house_nr == 3 & as.Date(reading_time, tz = "Europe/Copenhagen") %in% summer_dates) |
    (house_nr == 1 & as.Date(reading_time, tz = "Europe/Copenhagen") %in% winter_dates)
]

# Calculate and reshape the data for plotting
plot_dt[, real_total_demand := dhw_energy_kwh_demand + heating_energy_kwh_demand]
plot_dt2 <- melt(
  plot_dt[, .(
    house_nr, reading_time, demand_spms, real_total_demand
  )],
  id.vars = c("house_nr", "reading_time"),
  variable.factor = F
)
plot_dt <- melt(
  plot_dt[, .(
    house_nr, reading_time, sh_calculated, heating_energy_kwh_demand,
    dhw_energy_kwh_demand, dhw_calculated
  )],
  id.vars = c("house_nr", "reading_time"),
  variable.factor = F
)

# Prettify names
plot_dt[, variable := mgsub(
  variable,
  c("dhw_energy_kwh_demand", "dhw_calculated", "sh_calculated", "heating_energy_kwh_demand"),
  c("true DHW", "calulcated DHW", "calculated SH", "true SH")
)]
plot_dt2[, variable := mgsub(
  variable,
  c("demand_spms", "real_total_demand"),
  c("total energy input", "real total energy")
)]


plots[["summer"]] <- ggplot(plot_dt[house_nr == 3], aes(x = reading_time, y = value, color = variable, group = variable, shape = variable)) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.9) +
  scale_color_manual(
    values = c(
      "true DHW" = "#364B9A",
      "calulcated DHW" = "#6EA6CD",
      "calculated SH" = "#FDB366",
      "true SH" = "#A50026"
    ), name = NULL
  ) +
  scale_shape_manual(values = 15:18, name = NULL) +
  labs(x = NULL, y = "energy kWh") +
  scale_x_datetime(date_breaks = "1 day", date_labels = " ") +
  theme_nice()



plots[["summer_total"]] <- ggplot(plot_dt2[house_nr == 3], aes(x = reading_time, y = value, color = variable, group = variable, shape = variable)) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.9) +
  scale_color_manual(
    values = c(
      "total energy input" = "#762A83",
      "real total energy" = "#1B7837"
    ), name = NULL
  ) +
  scale_shape_manual(values = 15:16, name = NULL) +
  labs(x = NULL, y = "energy kWh") +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
  theme_nice()



plots[["winter"]] <- ggplot(plot_dt[house_nr == 1], aes(x = reading_time, y = value, color = variable, group = variable, shape = variable)) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.9) +
  scale_color_manual(
    values = c(
      "true DHW" = "#364B9A",
      "calulcated DHW" = "#6EA6CD",
      "calculated SH" = "#FDB366",
      "true SH" = "#A50026"
    ), name = NULL
  ) +
  scale_shape_manual(values = 15:18, name = NULL) +
  labs(x = NULL, y = "energy kWh") +
  scale_x_datetime(date_breaks = "1 day", date_labels = " ") +
  theme_nice()


plots[["winter_total"]] <- ggplot(plot_dt2[house_nr == 1], aes(x = reading_time, y = value, color = variable, group = variable, shape = variable)) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.9) +
  scale_color_manual(
    values = c(
      "total energy input" = "#762A83",
      "real total energy" = "#1B7837"
    ), name = NULL
  ) +
  scale_shape_manual(values = 15:16, name = NULL) +
  labs(x = NULL, y = "energy kWh") +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
  theme_nice()


# Assemble plots
layout <- c(
  area(t = 1, l = 1, b = 3, r = 1),
  area(t = 1, l = 2, b = 3, r = 2),
  area(t = 4, l = 1, b = 4, r = 1),
  area(t = 4, l = 2, b = 4, r = 2)
)

plots[["final_detailed"]] <- plots[["winter"]] + plots[["summer"]] + plots[["winter_total"]] + plots[["summer_total"]] +
  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect", design = layout) &
  theme(
    plot.tag = element_text(size = 8, face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(-2, -2, -2, -2, "mm")
  )

plots[["final_detailed"]][[3]] <- plots[["final_detailed"]][[3]] + plot_layout(tag_level = "new")
plots[["final_detailed"]][[4]] <- plots[["final_detailed"]][[4]] + plot_layout(tag_level = "new")

ggsave(
  "plots/07_Figure_15",
  plots[["final_detailed"]],
  device = cairo_pdf,
  width = 160,
  height = 160 * 4 / 6,
  unit = "mm"
)
