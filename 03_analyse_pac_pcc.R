# Title: Calculate Partial Auto and Cross-Correlation
#
# Purpose: Calculating partial auto and cross-correlation, this script produces
# plots 8 and 9 in the publication.
#
# Data files:
#   - data/01_selected_data.fst
#
# Function files:
#   - functions/000_styles.R
#
# Author: M. Schaffer Contact details: msch@build.aau.dk

# Load packages -----------------------------------------------------------
library(data.table)
library(fst)
library(purrr)
library(furrr)
library(ggplot2)
library(patchwork)
library(lubridate)
library(ppcor)
library(mgsub)
library(scales)

options(future.globals.maxSize = 1200 * 1024^2)

source("functions/000_styles.R")

# Load data ---------------------------------------------------------------

all_data <- read_fst("data/01_selected_data.fst", as.data.table = TRUE)
setorder(all_data, customer_id, time_rounded)

# Partial autocorrelation -------------------------------------------------

# Function to calculate partial autocorrelation
fn_pac <- function(data, n = 72) {
  pac_result <- data.table(lag = NULL, pac = NULL, p_value = NULL)
  # Based on https://stats.stackexchange.com/a/31656
  for (i in 1:n) {
    data[, paste0("demand_spms_lag_", i) := shift(demand_spms, i, type = "lag")]
    tmp_result <- pcor(na.omit(data[, -c("customer_id")]))
    pac_result <- rbind(pac_result, data.table(n = i, pac = tmp_result$estimate[i + 1, 1], p_value = tmp_result$p.value[i + 1, 1]))
  }
  pac_result[, customer_id := data[1, customer_id]]
  return(pac_result)
}
# Split up for parallel processing
pac_data <- all_data[, .(demand_spms, customer_id)]
pac_data <- split(pac_data, by = "customer_id")

plan(multisession, workers = 20)
pac_result <- future_map(pac_data, ~ fn_pac(data = .x, n = 72)) |> rbindlist()
plan(sequential)
rm(pac_data)
pac_result[, n := -n]

# Save results to a CSV file
fwrite(pac_result, "data/partial_autocor_seperatly.csv")

# Partial cross-correlation ------------------------------------------------

# Function to calculate partial cross-correlation
fn_pcc <- function(data, n = 72, quantity = "dni") {
  pcc_result <- data.table(n = NULL, type = NULL, pac = NULL, p_value = NULL)
  for (t in c("lag")) {
    pcc_data <- copy(data)
    for (i in 1:n) {
      pcc_data[, paste0(quantity, "_", i) := shift(get(quantity), i, type = t)]
      tmp_result <- pcor(na.omit(pcc_data[, -c("customer_id")]))
      pcc_result <- rbind(pcc_result, data.table(n = i, type = t, pcc = tmp_result$estimate[i + 2, 1], p_value = tmp_result$p.value[i + 2, 1]))
    }
  }
  pcc_result[, customer_id := data[1, customer_id]]
  pcc_result[, quantity := quantity]
  return(pcc_result)
}

pcc_data <- all_data[, .(demand_spms, dni, dhi, ext_temp, customer_id)]
pcc_data <- split(pcc_data, by = "customer_id")

# Evaluate the partial cross-correlation for different weather quantities and meters.
# The outer loop (quantities) is serial, the inner (meters) is parallel.
plan(list(sequential, multisession))
pcc_result <- future_map(c("dni", "dhi", "ext_temp"), function(qty) {
  future_map(pcc_data, ~ fn_pcc(data = .x[, c("demand_spms", "customer_id", qty), with = FALSE], quantity = qty, n = 72)) |> rbindlist()
}) |> rbindlist()
plan(sequential)
rm(pcc_data)
pcc_result[type == "lag", n := -n]

# Save results to a CSV file
fwrite(pcc_result, "data/partial_croscor_seperated.csv")


# Plotting ----------------------------------------------------------------

plots <- list()

## PAC --------------------------------------------------------------------

# pac_result <- fread("data/partial_autocor_seperatly.csv")

plots[["pac"]] <- ggplot(pac_result, aes(x = abs(n), y = pac, group = n)) +
  geom_boxplot(linewidth = 0.1, outlier.size = 0.2, outlier.color = "black", fill = "gray90", color = "black", outlier.alpha = 0.25) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 72, 6), labels = number_format(prefix = "±")) +
  labs(x = "lag/lead", y = "PAC") +
  theme_nice()

ggsave(
  filename = "plots/03_Figure_08.pdf",
  plot = plots[["pac"]],
  device = cairo_pdf,
  width = 88,
  height = 88 * 4 / 6,
  units = "mm"
)

## PCC --------------------------------------------------------------------

# pcc_result <- fread("data/partial_croscor_seperated.csv")
pcc_result <- pcc_result[type == "lag"]

# Prettify for plotting
pcc_result[, quantity := mgsub(string = quantity, pattern = c("dhi", "dni", "ext_temp"), replacement = c("DHI", "DNI", "ext. temperature"))]
pcc_result[, quantity := factor(quantity, levels = c("ext. temperature", "DHI", "DNI"))]

plots[["pcc"]] <- ggplot(pcc_result, aes(x = n, y = pcc, group = n)) +
  geom_boxplot(linewidth = 0.1, outlier.size = 0.2, outlier.color = "black", fill = "gray90", color = "black", outlier.alpha = 0.25) +
  facet_grid(rows = vars(quantity)) +
  theme_bw() +
  labs(x = "lag", y = "PCC") +
  scale_x_continuous(breaks = seq(-72, 72, 12)) +
  theme_nice() +
  theme(strip.background = element_rect(colour = "black", fill = "white"))

ggsave(
  filename = "plots/03_Figure_09.pdf",
  p_pcc,
  device = cairo_pdf,
  width = 88,
  height = 120,
  units = "mm"
)
