# Title: Analyse Different Feature Sets for the Chosen Regression Models
#
# Purpose: Creating five different features and analyzing their performance for
# the selected regression models using all data, this script generates figures
# 10 and 11.
#
# Data files:
#   - data/01_selected_data.fst
#
# Function files:
#   - functions/cvrmse_measure.R
#   - functions/000_styles.R
#
# Author: M. Schaffer Contact details: msch@build.aau.dk


# packages ----------------------------------------------------------------
library(mlr3)
require(mlr3tuning)
library(mlr3learners)
library(future)
library(data.table)
library(fst)
library(purrr)
library(furrr)
library(progressr)
library(ggplot2)
library(patchwork)
library(ggmagnify)
library(mgsub)

# Models
library(ranger)
library(glmnet)

source("functions/000_styles.R")

# Load and process data -------------------------------------------------------

data_dt <- read_fst("data/01_selected_data.fst", as.data.table = TRUE)
setorder(data_dt, water_meter_id, time_rounded)

## Add features ---------------------------------------------------------------
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
walk(1:24, ~ data_dt[, paste0("demand_spms_lag_", .x) := shift(demand_spms, type = "lag", n = .x), by = customer_id])
walk(1:24, ~ data_dt[, paste0("demand_spms_lead_", .x) := shift(demand_spms, type = "lead", n = .x), by = customer_id])
walk(1:15, ~ data_dt[, paste0("ext_temp_", .x) := shift(ext_temp, type = "lag", n = .x), by = customer_id])
walk(1:18, ~ data_dt[, paste0("dni_", .x) := shift(dni, type = "lag", n = .x), by = customer_id])
walk(1:18, ~ data_dt[, paste0("dhi_", .x) := shift(dhi, type = "lag", n = .x), by = customer_id])
walk(1:2, ~ data_dt[, paste0("ghi_", .x) := shift(ghi, type = "lag", n = .x), by = customer_id])

data_dt[, time_rounded := NULL]



# Regression -----------------------------------------------------------------

## Main regression function ---------------------------------------------------
fn_test_models <- function(data, pg) {
  # Load CVRMSE function
  source("functions/cvrmse_measure.R")

  ### Define the different feature sets as data and tasks ---------------------
  data_org_subset <- subset(data, select = colnames(data)[grepl("^demand_spms$|ghi|^ext_temp$|^ext_temp_[1-2]$|demand_spms_lag_1$|demand_spms_lead_1$", colnames(data))])
  data_small_subset <- subset(data, select = colnames(data)[grepl("^demand_spms$|time|demand_spms_lag_(1|2)$|demand_spms_lead_(1|2)$", colnames(data))])
  data_medium_subset <- subset(data, select = colnames(data)[grepl("^demand_spms$|time|demand_spms_lag_(1|2)$|demand_spms_lead_(1|2)$|^ext_temp$|^ext_temp_[1-7]$", colnames(data))])
  data_large_subset <- subset(data, select = colnames(data)[grepl("^demand_spms$|time|demand_spms_lag_(1|2)$|demand_spms_lead_(1|2)$|^ext_temp$|^ext_temp_[1-7]$|^dni$|^dni_[1-8]$|^dhi$|^dhi_(1[0-2]|[1-9])$", colnames(data))])
  data_xlarge_subset <- subset(data, select = colnames(data)[grepl("^demand_spms$|time|demand_spms|ext_temp|dni|dhi", colnames(data))])

  task_original <- as_task_regr(data_org_subset, target = "demand_spms", id = data[1, paste0(customer_id, "_original")])
  task_small <- as_task_regr(data_small_subset, target = "demand_spms", id = data[1, paste0(customer_id, "_small")])
  task_medium <- as_task_regr(data_medium_subset, target = "demand_spms", id = data[1, paste0(customer_id, "_medium")])
  task_large <- as_task_regr(data_large_subset, target = "demand_spms", id = data[1, paste0(customer_id, "_large")])
  task_xlarge <- as_task_regr(data_xlarge_subset, target = "demand_spms", id = data[1, paste0(customer_id, "_xlarge")])

  ### Resampling and measure --------------------------------------------------
  inner_resampling <- rsmp("holdout", ratio = 0.7) # Inner resampling
  outer_resampling <- rsmp("cv", folds = 10) # Outer resampling
  measure <- msr("reg.cvrmse") # Define measure
  terminator <- trm("none") # No stopping criteria

  ### Define Models -----------------------------------------------------------

  #### Linear model (lm) -------------------------------------------------------
  lm_learner <- lrn("regr.lm")
  lm_learner$id <- "lm"

  #### GLMNET ----------------------------------------------------------------
  glmnet_learner <- lrn("regr.cv_glmnet")
  glmnet_learner$param_set$set_values(alpha = to_tune(0, 1))
  glmnet_tuned <- auto_tuner(
    tuner = tnr("grid_search", resolution = 11),
    learner = glmnet_learner,
    resampling = inner_resampling,
    measure = measure,
    terminator = terminator
  )
  glmnet_tuned$id <- "glmnet"

  #### Random forest ----------------------------------------------------------
  rf_untuned <- lrn("regr.ranger")
  rf_untuned$param_set$set_values(num.trees = 1000, verbose = FALSE)
  rf_untuned$id <- "rf_untuned"

  ### Resampling and evaluation --------------------------------------
  bm_design <- benchmark_grid(
    tasks = list(task_original, task_small, task_medium, task_large, task_xlarge),
    learners = c(
      glmnet_tuned,
      lm_learner,
      rf_untuned
    ),
    resamplings = outer_resampling
  )

  ### Evaluate models and process results -------------------------------------
  bmr <- benchmark(bm_design, store_models = FALSE)
  result_final <- as.data.table(bmr$aggregate(c(measure, msr("regr.bias"), msr("regr.rmse"), msr("regr.mae"), msr("time_both"))))[, -"resample_result"]
  result_final[, `:=`(sample_size = data_small_subset[, .N])]
  
  fwrite(result_final, paste0("data/tmp_reg_features/", data[1, paste0(customer_id)], ".csv"))

  pg()
  return(NULL)
}

## Execute regression models --------------------------------------------------
# Check that tmp folder exists
if (!dir.exists(file.path("data/tmp_reg_features"))) {
  dir.create(file.path("data/tmp_reg_features"))
}

# Only considering hours with no water consumption and
data_dt <- na.omit(data_dt)
data_dt_known <- data_dt[zero_water == TRUE]
data_lst <- split(data_dt_known, by = "customer_id")

rm(data_dt, data_dt_known)
gc()

# Run function over all buildings
plan(multisession)
with_progress({
  pg <- progressor(steps = length(data_lst))
  future_walk(data_lst, ~ lgr::without_logging(fn_test_models(data = .x, pg = pg)),
    .options = furrr_options(
      seed = 123,
      packages = c(
        "ranger", "glmnet", "mlr3learners",
        "mlr3", "mlr3tuning", "data.table"
      ),
      chunk_size = 10L
    )
  )
})
plan(sequential)


# Evaluate performance ----------------------------------------------------
plots <- list()

# Load data and split feature set and task_id (customer_id)
files <- fs::dir_ls(path = "data/tmp_reg_features/", glob = "*.csv")
result_dt <- map(files, ~ fread(.x)) |> rbindlist()
result_dt[, feature_set := sub(".*_([[:alpha:]]*)$", "\\1", task_id)]
result_dt[, task_id := sub("^([0-9]+).*$", "\\1", task_id)]

# Restructure for plotting
result_dt[, c("resampling_id", "iters", "sample_size", "nr") := NULL]
result_dt <- melt(result_dt, id.vars = c("task_id", "learner_id", "feature_set"))
result_dt[, feature_set := factor(feature_set, levels = c("original", "small", "medium", "large", "xlarge"))]

# Calculate medians
cvrmse_median <- result_dt[variable == "reg.cvrmse", .(median_cvrmse = round(median(value), 3)), by = list(learner_id, feature_set)]
setorder(cvrmse_median, median_cvrmse)
time_median <- result_dt[variable == "time_both", .(time_median = round(median(value), 3)), by = list(learner_id, feature_set)]
setorder(time_median, time_median)

# plot grouped boxplots
colors <- c("#DDDDDD", "#77AADD", "#BBCC33", "#EE8866", "#AAAA00")

plots[["cvrmse"]] <- ggplot(result_dt[variable == "reg.cvrmse"], aes(x = learner_id, y = value, fill = feature_set)) +
  geom_boxplot(linewidth = 0.1, outlier.size = 0.2, outlier.alpha = 0.1) +
  scale_fill_manual(values = colors, name = "feature set") +
  labs(x = NULL, y = "CVRMSE") +
  theme_nice() +
  guides(x = guide_axis(angle = 90))+
  geom_magnify(
    from = list(xmin = 0.7,  ymin = 0, xmax = 3.3, ymax = 0.3), to = list(xmin = 0.7,  ymin = 1.5, xmax = 3.3, ymax = 3),
    axes = "y", linewidth = 0.1, proj.linetype = 3
  )

plots[["bias"]] <- ggplot(result_dt[variable == "regr.bias"], aes(x = learner_id, y = value, fill = feature_set)) +
  geom_boxplot(linewidth = 0.1, outlier.size = 0.2, outlier.alpha = 0.1) +
  scale_fill_manual(values = colors, name = "feature set") +
  scale_y_continuous(breaks = seq(0, -0.2, -0.025)) +
  labs(x = NULL, y = "Bias - kWh") +
  theme_nice()+
  guides(x = guide_axis(angle = 90))

plots[["time"]] <- ggplot(result_dt[variable == "time_both"], aes(x = learner_id, y = value, fill = feature_set)) +
  geom_boxplot(linewidth = 0.1, outlier.size = 0.2, outlier.alpha = 0.1) +
  scale_fill_manual(values = colors, name = "feature set") +
  labs(x = NULL, y = "Time - s") +
  theme_nice()+
  guides(x = guide_axis(angle = 90))


plots[["final"]] <- plots[["cvrmse"]] + plots[["bias"]] + plots[["time"]] +
  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") &
  theme(legend.position = "bottom", plot.tag = element_text(size = 8, face = "bold"))


ggsave(
  filename = "plots/04_Figure_10.pdf",
  plots[["final"]],
  device = cairo_pdf,
  width = 160,
  height = 88,
  units = "mm"
)


# Analyse ranks of feature sets -------------------------------------------

rank_dt <- result_dt[, .(rank = rank(value), feature_set, learner_id), by = list(task_id, variable)]

rank_selected_dt <- result_dt[learner_id == "rf_untuned" | learner_id == "lm", .(rank = rank(value, ties.method = "random"), feature_set), by = list(task_id, variable, learner_id)]

# needed to keep order correct if elements are missing
rank_selected_dt <- rank_selected_dt[variable == "reg.cvrmse", .N, by = list(learner_id, rank, feature_set)]
full_ranks <- expand.grid(list(
  learner_id = rank_selected_dt[, unique(learner_id)],
  rank = rank_selected_dt[, unique(rank)],
  feature_set = rank_selected_dt[, unique(feature_set)]
)) |> as.data.table()

rank_selected_dt <- merge(rank_selected_dt, full_ranks, all = TRUE)
rank_selected_dt[is.na(N), N := 0]

# plot grouped column plot
plots[["rank"]] <- ggplot(rank_selected_dt, aes(x = rank, y = N, fill = feature_set)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  labs(y = "nr. of buildings") +
  scale_fill_manual(values = colors, name = "feature set") +
  facet_grid(cols = vars(learner_id)) +
  theme_nice() +
  theme(
    strip.background = element_rect(colour = "black", fill = "white"),
    legend.key.size = unit(3, "mm")
  )

ggsave(
  filename = "plots/04_Figure_11.pdf",
  plots[["rank"]],
  device = cairo_pdf,
  width = 88,
  height = 88 * 4 / 6,
  units = "mm"
)
