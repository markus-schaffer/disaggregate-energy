# Title: Data Selection and Analysis of Smart Heater and Water Meter Data
# 
# Purpose: This script combines smart meter data with weather data and conducts
# analysis, producing figures 3 to 5 in the publication.
# 
# Data files:
#   - data/00_smart_meter_data.fst
#   - data/00_weather_processed.csv
# 
# Function files:
#   - functions/000_styles.R
# 
# Author: M. Schaffer
# Contact details: msch@build.aau.dk

# Load packages -----------------------------------------------------------

library(data.table)
library(fst)
library(purrr)
library(ggplot2)
library(scales)
library(patchwork)
library(lubridate)
library(fitdistrplus)


source("functions/000_styles.R")

if (!dir.exists("plots")) {
  dir.create("plots")
}


# Load data ---------------------------------------------------------------

all_data <- read_fst("data/00_smart_meter_data.fst", as.data.table = TRUE)

# Add weather data to the smart meter data
weather_dt <- fread("data/00_weather_processed.csv")
weather_dt <- weather_dt[, .(observed, temp_mean_past1h, DHI_calc_BRL, DNI_calc_BRL, radia_glob_past1h)]
setnames(weather_dt, c("observed", "temp_mean_past1h", "DHI_calc_BRL", "DNI_calc_BRL", "radia_glob_past1h"), c("time_rounded", "ext_temp", "dhi", "dni", "ghi"))
all_data <- merge(all_data, weather_dt, by.x = "time_rounded", by.y = "time_rounded", all.x = TRUE, sort = FALSE)

# Process data -------------------------------------------------------------

# Identify unused buildings in January or February, excluding newly constructed
# buildings with installed meters that are not yet in use.
not_used <- map(c(1, 2), ~ all_data[month(time_rounded) == .x, .(energy = sum(demand_spms), water = sum(water_volume_demand_m3h)), by = customer_id])
walk2(not_used, paste0(c("jan", "feb"), "_energy"), ~ setnames(.x, "energy", .y))
walk2(not_used, paste0(c("jan", "feb"), "_water"), ~ setnames(.x, "water", .y))
not_used <- Reduce(function(...) merge(..., by = "customer_id"), not_used)
not_used[, exclude := any(map_lgl(.SD, ~ .x == 0)),
  .SDcols =
    c(
      paste0(c("jan", "feb"), "_energy"),
      paste0(c("jan", "feb"), "_water")
    ),
  by = customer_id
]
all_data <- all_data[!customer_id %in% not_used[exclude == TRUE, customer_id]]

# Recalculate the water volume to overcome the effect that the liner
# interpolation used to get equidistant data can introduce very small water
# usage
all_data[, water_volume_m3 := round(water_volume_m3, digits = 3)]
all_data[, water_volume_demand_m3h := water_volume_m3 - shift(water_volume_m3), by = customer_id]
setnafill(all_data, type = "nocb", cols = "water_volume_demand_m3h")
all_data[, zero_water := round(water_volume_demand_m3h, 3) < (1 / 1000)]

# Exclude meters with less than 3 hours or more than 21 hours of daily water use on average.
fraction_no_water <- all_data[, .(no_water = mean(zero_water)), by = customer_id]
fraction_no_water[, suspicious := no_water <= ((365 * 3) / 8760) | no_water > ((8760 - (365 * 3)) / 8760)]
all_data <- all_data[customer_id %in% fraction_no_water[suspicious == FALSE, customer_id]]

# Save processed data
write_fst(all_data, "data/01_selected_data.fst", compress = 100)




# Plot data ---------------------------------------------------------------
plots <- list()

# Function to calculate binwidth for histograms
# https://stats.stackexchange.com/a/862/385713
fn_bw <- function(vec) {
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
  if (is.wholenumber(vec) |> all()) {
    (2 * IQR(vec) / length(vec)^(1 / 3)) |> round()
  } else {
    (2 * IQR(vec) / length(vec)^(1 / 3)) |>
      formatC(format = "fg", digits = 2) |>
      as.numeric()
  }
}

## Distribution of fraction of hours without water use --------------------
plots[["distribution"]] <- ggplot(fraction_no_water[suspicious == FALSE], aes(x = no_water)) +
  geom_histogram(
    binwidth = fn_bw(fraction_no_water[suspicious == FALSE, no_water]),
    boundary = 0,
    fill = gray,
    aes(y = after_stat(density))
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = fraction_no_water[suspicious == FALSE, mean(no_water)],
      sd = fraction_no_water[suspicious == FALSE, sd(no_water)]
    ),
    linewidth = 0.2,
    aes(color = paste0(
      "Normal distribution\nmean = ",
      round(fraction_no_water[suspicious == FALSE, mean(no_water)], 2),
      ", sd = ", round(fraction_no_water[suspicious == FALSE, sd(no_water)], 2)
    )),
    xlim = c(0, 1)
  ) +
  scale_x_continuous(
    name = "fraction of hours without water use",
    expand = c(0.01, 0.01),
    limits = c(0, 1)
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_colour_manual(name = NULL, values = orange) +
  scale_y_continuous(name = "density") +
  theme_nice() +
  theme(legend.key.size = unit(5, "mm"))


plots[["distribution_ecdf"]] <-
  ggplot(fraction_no_water[suspicious == FALSE], aes(x = no_water)) +
  stat_ecdf(linewidth = 0.3) +
  stat_function(
    fun = pnorm,
    args = list(
      mean = fraction_no_water[suspicious == FALSE, mean(no_water)],
      sd = fraction_no_water[suspicious == FALSE, sd(no_water)]
    ),
    linewidth = 0.2,
    aes(color = "none"), xlim = c(-0.1, 1.1)
  ) +
  scale_y_continuous(name = "Cumulative distr. of fraction\n of hours without water use", expand = c(0.01, 0.01)) +
  scale_x_continuous(name = "fraction of hours without water use") +
  scale_colour_manual(name = NULL, values = orange) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_nice() +
  theme(legend.key.size = unit(5, "mm"))+
  guides(color = "none")


plots[["distribution_final"]] <- plots[["distribution"]] + plots[["distribution_ecdf"]] +
  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") &
  theme(
    plot.tag = element_text(size = 8, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.margin = margin(-2, -2, -2, -2, "mm")
  )

ggsave(
  "plots/01_Figure_03.pdf",
  plots[["distribution_final"]],
  device = cairo_pdf,
  width = 140,
  height = 88 * 4 / 6,
  unit = "mm"
)


## Plot ranked and daily normalized energy use split for water use  --------

# String for nicer plotting
all_data[, water_usage_plot := fcase(
  zero_water == TRUE, "no water use",
  zero_water == FALSE, "water use"
)]

all_data[, energy_rank := frank(demand_spms, ties.method = "random"), by = .(customer_id, as.Date(time_rounded, tz = "Europe/Copenhagen"))]
all_data[, energy_nomalised := demand_spms / max(demand_spms), by = .(customer_id, as.Date(time_rounded, tz = "Europe/Copenhagen"))]

# Exclude days which have no energy use as this would add noise to the result.
# Exclude day where due to the daylight saving time, the day has 23/25 hours
all_data[, exclude := sum(demand_spms) == 0 | max(energy_rank) != 24, by = .(customer_id, as.Date(time_rounded, tz = "Europe/Copenhagen"))]

plots[["tmp_dist_rank"]] <- ggplot(all_data[exclude == FALSE], aes(x = energy_rank, fill = water_usage_plot)) +
  geom_bar(aes(y = (after_stat(count)) / sum(after_stat(count)))) +
  facet_grid(rows = vars(water_usage_plot)) +
  scale_fill_manual(values = c(gray, blue), name = NULL) +
  scale_x_continuous(breaks = c(1, 6, 12, 18, 24), minor_breaks = c(3, 9, 15, 21)) +
  labs(x = "ranked energy use", y = "density") +
  theme_nice()+
    theme(
    legend.position = "none",
    strip.background = element_rect(colour = "black", fill = "white")
    )


plots[["tmp_dist_norm"]] <- ggplot(all_data[exclude == FALSE], aes(x = energy_nomalised, fill = water_usage_plot)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.025, boundary = 0) +
  facet_grid(rows = vars(water_usage_plot)) +
  scale_fill_manual(values = c(gray, blue), name = NULL) +
  theme_bw() +
  labs(x = "daily normalised energy use", y = NULL) +
  theme_nice()+
  theme(
    legend.position = "none",
    strip.background = element_rect(colour = "black", fill = "white")
    )

plots[["tmp_dist_energy"]] <- plots[["tmp_dist_rank"]] + plots[["tmp_dist_norm"]] +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 8, face = "bold"))

ggsave(
  "plots/01_Figure_04.pdf",
  plots[["tmp_dist_energy"]],
  device = cairo_pdf,
  width = 88,
  height = 88 * 4 / 6,
  unit = "mm"
)


## Three profiles with low medium and high fraction ------------------------


fn_plot_detail <- function(data) {
  bw <- min(c(fn_bw(data[zero_water == F, demand_spms]), fn_bw(data[zero_water == F, demand_spms])))

  hour_plot <- ggplot(data, aes(fill = water_usage_plot, x = plot_day, y = hour(time_rounded))) +
    geom_tile(color = NA) +
    scale_fill_manual(values = c(gray, blue), name = NULL) +
    scale_x_date(expand = c(0, 0), name = NULL, date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(expand = c(0, 0), name = "hour", breaks = seq(0, 24, 4)) +
    theme_nice()

  histo_plot <- ggplot(data, aes(x = demand_spms, fill = water_usage_plot)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = bw, boundary = 0) +
    facet_grid(cols = vars(water_usage_plot)) +
    scale_x_continuous(n.breaks = 4) +
    scale_y_continuous(n.breaks = 4) +
    scale_fill_manual(values = c(gray, blue), name = NULL) +
    labs(x = "total energy use - kWh", y = "density") +
    theme_nice() +
    theme(strip.text.x = element_blank()) +
    guides(fill = "none")


  return(list("hour" = hour_plot, "hist" = histo_plot))
}


water_fraction <- all_data[, .(percent = round(mean(zero_water) * 100, 3)), by = customer_id]

# Low fraction of hours without water use
low_plot_dt <- all_data[customer_id == water_fraction[percent == 20.927, customer_id]]
low_plot_dt[, plot_day := as.Date(time_rounded, tz = "Europe/Copenhagen")]

# Medium fraction of hours without water use
medium_plot_dt <- all_data[customer_id == water_fraction[percent == 43.726, customer_id]]
medium_plot_dt[, plot_day := as.Date(time_rounded, tz = "Europe/Copenhagen")]

# High fraction of hours without water use
high_plot_dt <- all_data[customer_id == water_fraction[percent == 69.323, customer_id]]
high_plot_dt[, plot_day := as.Date(time_rounded, tz = "Europe/Copenhagen")]

# Generate plots
low_plot <- fn_plot_detail(low_plot_dt)
medium_plot <- fn_plot_detail(medium_plot_dt)
high_plot <- fn_plot_detail(high_plot_dt)


plots[["detailed_plot"]] <- wrap_plots(low_plot) / wrap_plots(medium_plot) / wrap_plots(high_plot) +
  plot_layout(guides = "collect", nrow = 3, widths = c(3, 2)) +
  plot_annotation(tag_levels = c("a", NULL)) &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 8, face = "bold"),
    plot.margin = margin(rep(2, 4), unit = "mm")
  )

# Change annotation to be "row wise"
plots[["detailed_plot"]][[1]][[2]] <- plots[["detailed_plot"]][[1]][[2]] + plot_layout(tag_level = "new")
plots[["detailed_plot"]][[2]][[2]] <- plots[["detailed_plot"]][[2]][[2]] + plot_layout(tag_level = "new")
plots[["detailed_plot"]][[3]][[2]] <- plots[["detailed_plot"]][[3]][[2]] + plot_layout(tag_level = "new")


ggsave(
  "plots/01_Figure_05.pdf",
  plots[["detailed_plot"]],
  device = cairo_pdf,
  width = 140,
  height = 140,
  unit = "mm"
)
