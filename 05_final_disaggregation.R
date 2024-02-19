# Title: Run the Final Algorithm on the Data
#
# Purpose: Performing final disaggregation for all buildings and analyzing
# feature representativeness using two proposed methods, this script produces
# figure 12.
#
# Data files:
#   - data/01_selected_data.fst
#
# Function files:
#   - functions/cvrmse_measure.R
#
# Author: M. Schaffer Contact details: msch@build.aau.dk


# packages ----------------------------------------------------------------
library(mlr3)
require(mlr3tuning)
library(mlr3learners)
library(fst)
library(purrr)
library(progressr)
library(data.table)
library(furrr)
library(ggplot2)
library(patchwork)
library(cramer)

# Models
library(ranger)


# Load and process data -------------------------------------------------------
data_dt <- read_fst("data/01_selected_data.fst", as.data.table = TRUE)


# Add features ------------------------------------------------------------
# needed to get weekdays in English
Sys.setlocale("LC_TIME", "English")
# Time dependent features
data_dt[, `:=`(
  time_hour_cos = cos(hour(time_rounded) * (2 * pi / 24)),
  time_hour_sin = sin(hour(time_rounded) * (2 * pi / 24)),
  time_month_cos = cos((month(time_rounded) - 1) * (2 * pi / 12)),
  time_month_sin = sin((month(time_rounded) - 1) * (2 * pi / 12)),
  time_yday_cos = cos((yday(time_rounded) - 1) * (2 * pi / 365)),
  time_yday_sin = sin((yday(time_rounded) - 1) * (2 * pi / 365)),
  time_is_weekend = as.numeric(grepl("S(at|un)", weekdays(time_rounded))),
  time_week_day_cos = cos((lubridate::wday(time_rounded, label = FALSE, week_start = 1) - 1) * (2 * pi / 7)),
  time_week_day_sin = sin((lubridate::wday(time_rounded, label = FALSE, week_start = 1) - 1) * (2 * pi / 7))
)]

# Customer specific features
walk(1:2, ~ data_dt[, paste0("demand_spms_lag_", .x) := shift(demand_spms, type = "lag", n = .x), by = customer_id])
walk(1:2, ~ data_dt[, paste0("demand_spms_lead_", .x) := shift(demand_spms, type = "lead", n = .x), by = customer_id])

data_dt <- na.omit(data_dt)
data_lst <- split(data_dt, by = "customer_id")

# Similarity testing ------------------------------------------------------
fn_similarity <- function(data, pg) {
  features <- colnames(data)[grepl("zero_water|time|demand_spms_lag_(1|2)$|demand_spms_lead_(1|2)$", colnames(data))]
  features <- features[features != "time_rounded"]
  feature_dt <- subset(data, select = features)
  feature_dt[, colnames(feature_dt)[-1] := map(.SD, ~ scale(.x)), .SDcols = colnames(feature_dt)[-1]]

  # Clustering
  clusters <- kmeans(as.matrix(feature_dt[, -"zero_water"]), centers = 2, iter.max = 50, nstart = 10)$cluster - 1
  result <- data.table(mcc = abs(mlr3measures::mcc(truth = data[, as.factor(as.numeric(zero_water))], response = as.factor(clusters), positive = "0")))

  # Cramer Test
  sample1 <- as.matrix(feature_dt[zero_water == TRUE, -"zero_water"])
  sample2 <- as.matrix(feature_dt[zero_water == FALSE, -"zero_water"])
  result[, cramer_statistic := cramer.test(x = sample1, y = sample2, just.statistic = T)$statistic]

  result[, colnames(data[1, .(customer_id, heat_meter_id, water_meter_id)]) := data[1, .(customer_id, heat_meter_id, water_meter_id)]]
  pg()
  return(result)
}

plan(multisession)
with_progress({
  pg <- progressor(steps = length(data_lst))
  similarity_result <- future_map(data_lst, ~ fn_similarity(data = .x, pg = pg),
    .options = furrr_options(
      seed = 123,
      packages = c(
        "Rcpp", "cramer", "data.table"
      ),
      chunk_size = 10L
    )
  ) |> rbindlist()
})
plan(sequential)




# Disaggregation --------------------------------------------------------------

fn_disaggrgate <- function(data, pg) {
  # Load CVRMSE
  source("functions/cvrmse_measure.R")

  # Define training and prediction data/task
  features <- colnames(data)[grepl("time|demand_spms", colnames(data))]
  features <- features[features != "time_rounded"]
  train_data_subset <- subset(data[zero_water == TRUE], select = features)
  predict_data_subset <- subset(data[zero_water == FALSE], select = features)
  task <- as_task_regr(train_data_subset, target = "demand_spms", id = data[1, paste0(customer_id)])

  # Outer resampling for CV
  resampling <- rsmp("cv", folds = 10)

  ## RF not tuned
  rf_untuned <- lrn("regr.ranger")
  rf_untuned$param_set$set_values(num.trees = 1000)
  rf_untuned$id <- "rf_untuned"

  # Cross validation
  cv <- resample(task = task, learner = rf_untuned, resampling = resampling)
  cv_result <- cv$aggregate(c(msr("reg.cvrmse"), msr("regr.bias"), msr("regr.rmse"), msr("regr.mae"), msr("time_both")))
  regression_result <- as.data.table(as.list(cv_result))
  data[, colnames(regression_result) := regression_result]

  # Prediction
  rf_untuned$train(task)
  prediction_result <- rf_untuned$predict_newdata(predict_data_subset)


  ## DHW & SH calculation
  data[zero_water == FALSE, sh_calculated := prediction_result$response]
  data[, dhw_calculated := fifelse(is.na(sh_calculated), yes = 0, no = demand_spms - sh_calculated)]

  # Physic plausibility check
  # DHW must be >= 0kWh and not more than the energy needed to heat up the total water use from 10C to 55C
  # Fixed temperature at 10C and 55C
  dhw_hot_c <- 55
  dhw_cold_c <- 10
  # According enthalpies - kJ/kg
  dhw_hot_enthalpy <- 231.588972864125
  dhw_cold_enthalpy <- 43.5791315368973
  # Cold water specific volume m3/kg
  dhw_cold_sv <- 0.00099958376441487

  # Calculate maximum allowed energy in kWh
  data[, dhw_max := water_volume_demand_m3h * (1 / dhw_cold_sv) * ((dhw_hot_c - dhw_cold_c) * ((dhw_hot_enthalpy - dhw_cold_enthalpy) / (dhw_hot_c - dhw_cold_c)) / 3600)]
  # Plausibility check
  data[, dhw_calculated := fcase(
    dhw_calculated > dhw_max, dhw_max,
    dhw_calculated < 0, 0,
    dhw_calculated <= dhw_max & dhw_calculated >= 0, dhw_calculated
  )]
  # Recalculate SH
  data[, sh_calculated := demand_spms - dhw_calculated]
  data[, (features[features != "demand_spms"]) := NULL]
  pg()
  return(data)
}


plan(multisession)
with_progress({
  pg <- progressor(steps = length(data_lst))
  regression_result <- future_map(data_lst, ~ lgr::without_logging(fn_disaggrgate(data = .x, pg = pg)),
    .options = furrr_options(
      seed = 123,
      packages = c(
        "ranger", "mlr3learners", "mlr3", "mlr3tuning", "data.table"
      ),
      chunk_size = 10L
    )
  ) |> rbindlist()
})
plan(sequential)


# Merge and save results --------------------------------------------------

result <- merge(regression_result, similarity_result, by = c("water_meter_id", "heat_meter_id", "customer_id"))
write_fst(result, "data/05_disaggregated_data.fst", compress = 100)
# result <- read_fst("data/05_disaggregated_data.fst", as.data.table = TRUE)

# Analyse feature dissimilarity --------------------------------------------

plots <- list()

rep_result <- result[, .SD[1], by = customer_id, .SDcols = c("mcc", "cramer_statistic")]
rep_result[, special := fifelse(mcc > 0.5 | (mcc < 0.1 & cramer_statistic > 1000), TRUE, FALSE)]
rep_result[special == TRUE]

# Scatter plot of the two test against each other with two highlighted points
plots[["dis"]] <- ggplot(rep_result, aes(x = cramer_statistic, y = mcc, color = special)) +
  geom_point(size = 0.05) +
  scale_color_manual(values = c("black", "#EE7733")) +
  geom_text(data = rep_result[mcc > 0.5], aes(cramer_statistic, mcc, label = "1"), nudge_x = 80) +
  geom_text(data = rep_result[mcc < 0.1 & cramer_statistic > 1000], aes(cramer_statistic, mcc, label = "2"), nudge_x = 80) +
  labs(x = "Cramer statistic", y = "CSD") +
  theme_nice() +
  theme(legend.position = "none")

## Analyse the two highlighted points in detail ----------------------------
plot_dt <- result[customer_id %in% rep_result[special == TRUE, customer_id]]
plot_dt[, building := fifelse(customer_id == rep_result[special == TRUE & mcc > 0.5, customer_id], 1, 2)]

# Add features
Sys.setlocale("LC_TIME", "English")
# Time dependent features
plot_dt[, `:=`(
  time_hour_cos = cos(hour(time_rounded) * (2 * pi / 24)),
  time_hour_sin = sin(hour(time_rounded) * (2 * pi / 24)),
  time_month_cos = cos((month(time_rounded) - 1) * (2 * pi / 12)),
  time_month_sin = sin((month(time_rounded) - 1) * (2 * pi / 12)),
  time_yday_cos = cos((yday(time_rounded) - 1) * (2 * pi / 365)),
  time_yday_sin = sin((yday(time_rounded) - 1) * (2 * pi / 365)),
  time_is_weekend = as.numeric(grepl("S(at|un)", weekdays(time_rounded))),
  time_week_day_cos = cos((lubridate::wday(time_rounded, label = FALSE, week_start = 1) - 1) * (2 * pi / 7)),
  time_week_day_sin = sin((lubridate::wday(time_rounded, label = FALSE, week_start = 1) - 1) * (2 * pi / 7))
)]

# Customer specific features
walk(1:2, ~ plot_dt[, paste0("demand_spms_lag_", .x) := shift(demand_spms, type = "lag", n = .x), by = customer_id])
walk(1:2, ~ plot_dt[, paste0("demand_spms_lead_", .x) := shift(demand_spms, type = "lead", n = .x), by = customer_id])

# Plot distribution of data points per month
plot_dt[, water_use := fifelse(zero_water == T, "no water use", "water use")]
month_dt <- plot_dt[, .N, by = list(month = lubridate::month(time_rounded, label = T, abbr = T), building, water_use)]
month_dt[, density := N / sum(N), by = list(building, water_use)]

plots[["month"]] <- ggplot(month_dt, aes(x = month, y = density)) +
  geom_col(width = 0.7) +
  facet_grid(rows = vars(water_use), cols = vars(building)) +
  scale_x_discrete(labels = c("Jan", rep("", 4), "Jun", rep("", 5), "Dec")) +
  labs(x = NULL, y = "density") +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.spacing.x = unit(2, "mm"),
    panel.spacing.y = unit(1, "mm")
  )

# Plot distribution of energy use
plots[["hist"]] <- ggplot(plot_dt, aes(x = demand_spms_lead_2)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, na.rm = TRUE) +
  facet_grid(rows = vars(water_use), cols = vars(building)) +
  labs(x = "energy use - 1h lag") +
  theme_nice() +
  theme(
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.spacing.x = unit(2, "mm"),
    panel.spacing.y = unit(1, "mm")
  )

plots[["final"]] <- plots[["dis"]] + plots[["month"]] + plots[["hist"]] +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 8, face = "bold"))


ggsave(
  "plots/05_Figure_12",
  plots[["final"]],
  device = cairo_pdf,
  width = 180,
  height = 140 * 4 / 6,
  unit = "mm"
)
