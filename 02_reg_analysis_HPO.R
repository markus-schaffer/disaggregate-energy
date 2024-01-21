# Title: Analyse Different Regression Models with and without HPO
# 
# Purpose: Selecting data from 160 buildings, this script performs analysis on
# various regression models with and without hyperparameter optimization,
# generating figures 6 and 7.
# 
# Data files:
#   - data/01_selected_data.fst
# 
# Function files:
#   - functions/000_styles.R
#   - functions/cvrmse_measure.R
# 
# Author: M. Schaffer
# Contact details: msch@build.aau.dk

# Load packages -----------------------------------------------------------

library(mlr3)
require(mlr3tuning)
require(mlr3tuningspaces)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3hyperband)
library(mlr3pipelines)
library(future)
library(data.table)
library(fst)
library(purrr)
library(furrr)
library(progressr)
library(ggplot2)
library(patchwork)

# Models
library(e1071)
library(xgboost)
library(ranger)
library(kknn)
library(glmnet)

source("functions/000_styles.R")


# Load and select data ----------------------------------------------------

data_dt <- read_fst("data/01_selected_data.fst", as.data.table = TRUE)
fraction_no_water <- data_dt[, .(no_water = mean(zero_water)), by = customer_id]

# Select data so that sample and remaining data have most similar distribution
# of hours without water
all_samples <- map(1:1e4, function(index) {
  set.seed((123 + index))
  samples <- fraction_no_water[sample(.N, 160)]
  distance <- ks.test(samples[, no_water], fraction_no_water[!customer_id %in% samples[, customer_id], no_water])$statistic
  data.table(samples = list(samples[, "customer_id"]), distance = distance)
}) |> rbindlist()
best_sample <- all_samples[which.min(distance), samples[1]]
data_dt <- data_dt[customer_id %in% best_sample[, customer_id]]



# Regression --------------------------------------------------------------

## Add features ----------------------------------------------------------
data_dt[, `:=`(
  ext_temp_1h = shift(ext_temp, n = 1L, type = "lag"),
  ext_temp_2h = shift(ext_temp, n = 2L, type = "lag"),
  ghi_1h = shift(ghi, n = 1L, type = "lag"),
  ghi_2h = shift(ghi, n = 2L, type = "lag"),
  demand_spms_1h_lag = shift(demand_spms, n = 1L, type = "lag"),
  demand_spms_1h_lead = shift(demand_spms, n = 1L, type = "lead")
), by = customer_id]
setnafill(data_dt, type = "locf", cols = c("ext_temp_1h", "ext_temp_2h", "ghi_1h", "ghi_2h", "demand_spms_1h_lag", "demand_spms_1h_lead"))
setnafill(data_dt, type = "nocb", cols = c("ext_temp_1h", "ext_temp_2h", "ghi_1h", "ghi_2h", "demand_spms_1h_lag", "demand_spms_1h_lead"))


## Main regression function ---------------------------------------------------
fn_test_models <- function(data, pg) {
  # Load CVRMSE function
  source("functions/cvrmse_measure.R")

  ### Data preparation, sampling and measure definition ----------------------
  data_subset <- subset(data, select = colnames(data)[grepl("demand_spms|ext_temp|ghi", colnames(data))])
  task <- as_task_regr(data_subset, target = "demand_spms", id = data[1, paste0(customer_id)])

  inner_resampling <- rsmp("holdout", ratio = 0.7) # Inner resampling
  outer_resampling <- rsmp("cv", folds = 10) # Outer resampling
  measure <- msr("reg.cvrmse") # Define measure
  terminator <- trm("none") # No stopping criteria

  ### Define Models ----------------------------------------------------------

  #### Linear models ---------------------------------------------------------
  # Additive model only
  lm_learner <- lrn("regr.lm")
  lm_learner$id <- "lm"
  # Model with main effects and all pairwise interactions
  lm_interact <- as_learner(po("modelmatrix", formula = ~ .^2) %>>% lm_learner)
  lm_interact$id <- "lm_interact"

  #### GLMNET ----------------------------------------------------------------
  # Additive model only
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

  # Model with main effects and all pairwise interactions
  glmnet_interact <- lrn("regr.cv_glmnet")
  glmnet_interact <- as_learner(po("modelmatrix", formula = ~ .^2) %>>% glmnet_interact)
  glmnet_interact$param_set$set_values(regr.cv_glmnet.alpha = to_tune(0, 1))
  glmnet_interact_tuned <- auto_tuner(
    tuner = tnr("grid_search", resolution = 11),
    learner = glmnet_interact,
    resampling = inner_resampling,
    measure = measure,
    terminator = terminator
  )
  glmnet_interact_tuned$id <- "glmnet_interact"

  #### knn -------------------------------------------------------------------
  # not tuned
  knn_untuned <- lrn("regr.kknn")
  knn_untuned$param_set$set_values(scale = TRUE) # scale is default
  knn_untuned$id <- "knn_untuned"

  # tuned
  tuning_space_knn <- lts("regr.kknn.default")
  knn_learner_tuning <- tuning_space_knn$get_learner(scale = TRUE) # scale is default
  knn_graph <- as_learner(po("subsample") %>>% knn_learner_tuning)

  # Set subsample as budget parameter in Hyperband
  knn_graph$param_set$set_values(subsample.frac = to_tune(p_dbl(0.1, 1, tags = "budget")))
  knn_tuned <- auto_tuner(
    tuner = tnr("hyperband", eta = 2, repetitions = 1),
    learner = knn_graph,
    resampling = inner_resampling,
    measure = measure,
    terminator = terminator
  )
  knn_tuned$id <- "knn_tuned"


  #### Random Forest ---------------------------------------------------------
  # not tuned
  rf_untuned <- lrn("regr.ranger")
  rf_untuned$param_set$set_values(num.trees = 1000)
  rf_untuned$id <- "rf_untuned"

  ## tuned
  tuning_space_rf <- lts("regr.ranger.default")
  rf_learner_tuning <- tuning_space_rf$get_learner()
  rf_learner_tuning$param_set$set_values(num.trees = to_tune(p_int(200, 2000, tags = "budget")))
  rf_tuned <- auto_tuner(
    tuner = tnr("hyperband", eta = 2, repetitions = 1),
    learner = rf_learner_tuning,
    resampling = inner_resampling,
    measure = measure,
    terminator = terminator
  )
  rf_tuned$id <- "rf_tuned"

  #### SVR -------------------------------------------------------------------
  # not tuned
  svr_untuned <- lrn("regr.svm")
  svr_untuned$id <- "svr_untuned"

  # as used by Leiria et al. 2023
  svr_leiria <- lrn("regr.svm")
  svr_leiria$param_set$set_values(type = "eps-regression", kernel = "radial", cost = 7, gamma = 0.01)
  svr_leiria$id <- "svr_leira"

  # tuned
  tuning_space_svm <- lts("regr.svm.default")
  svr_learner_tuning <- tuning_space_svm$get_learner(type = "eps-regression", scale = TRUE)
  # Subsample for Hyperband HPO
  svr_learner_graph <- as_learner(po("subsample") %>>% svr_learner_tuning)
  # Set subsample as budget parameter in Hyperband
  svr_learner_graph$param_set$set_values(subsample.frac = to_tune(p_dbl(0.1, 1, tags = "budget")))

  # Fall back learner - regr.featureless - mean() if timeout (in sec) exceeds limit
  svr_learner_graph$encapsulate <- c(train = "evaluate", predict = "evaluate")
  svr_learner_graph$timeout <- c(train = 60, predict = 60)
  svr_learner_graph$fallback <- lrn("regr.featureless")

  svr_tuned <- auto_tuner(
    tuner = tnr("hyperband", eta = 2, repetitions = 1),
    learner = svr_learner_graph,
    resampling = inner_resampling,
    measure = measure,
    terminator = terminator
  )
  svr_tuned$id <- "svr_tuned"

  #### XGBoost ---------------------------------------------------------------
  # Not tuned
  xgboost_untuned <- lrn("regr.xgboost")
  xgboost_untuned$id <- "xgboost_untuned"

  # tuned
  tuning_space_xgboost <- lts("regr.xgboost.default")
  xgboost_learner_tuning <- tuning_space_xgboost$get_learner()
  # Update to mark as budget parameter for Hyperband
  xgboost_learner_tuning$param_set$set_values(nrounds = to_tune(p_int(500, 5000, tags = "budget")))
  xgboost_tuned <- auto_tuner(
    tuner = tnr("hyperband", eta = 4, repetitions = 1),
    learner = xgboost_learner_tuning,
    resampling = inner_resampling,
    measure = measure,
    terminator = terminator
  )
  xgboost_tuned$id <- "xgboost_tuned"

  ### (Nested) resampling and evaluation --------------------------------------
  bm_design <- benchmark_grid(
    tasks = task,
    learners = c(
      glmnet_tuned,
      glmnet_interact_tuned,
      knn_untuned,
      knn_tuned,
      lm_learner,
      lm_interact,
      rf_untuned,
      rf_tuned,
      svr_untuned,
      svr_leiria,
      svr_tuned,
      xgboost_untuned,
      xgboost_tuned
    ),
    resamplings = outer_resampling
  )

  ### Evaluate models and process results -------------------------------------
  bmr <- benchmark(bm_design, store_models = FALSE)
  result_final <- as.data.table(bmr$aggregate(c(measure, msr("regr.bias"), msr("regr.rmse"), msr("regr.mae"), msr("time_both"))))[, -"resample_result"]
  result_final[, `:=`(sample_size = data_subset[, .N])]
  
  fwrite(result_final, paste0("data/tmp_reg_hyperband/", data[1, paste0(customer_id)], ".csv"))
  
  pg()
  return(NULL)
}



## Execute regression models --------------------------------------------------
# Check that tmp folder exists
if (!dir.exists(file.path("data/tmp_reg_hyperband"))) {
  dir.create(file.path("data/tmp_reg_hyperband"))
}

# Only considering hours with no water consumption and
data_dt[, demand_spms_area := NULL]
data_dt_known <- data_dt[zero_water == TRUE]
data_lst <- split(data_dt_known, by = "customer_id")

# Run function over all buildings
plan(multisession, workers = 10)
with_progress({
  pg <- progressor(steps = length(data_lst))
  future_walk(data_lst, ~ lgr::without_logging(fn_test_models(data = .x, pg = pg)),
    .options = furrr_options(
      seed = 123,
      packages = c(
        "e1071", "xgboost", "ranger", "kknn", "glmnet", "mlr3learners", "mlr3extralearners",
        "mlr3", "mlr3tuning", "mlr3tuningspaces", "data.table", "mlr3hyperband", "mlr3pipelines"
      ),
      chunk_size = 1L
    )
  )
})
plan(sequential)


# Evaluate regression performance ---------------------------------------------
plots <- list()

files <- fs::dir_ls(path = "data/tmp_reg_hyperband", glob = "*.csv")
result_dt <- map(files, ~ fread(.x)) |> rbindlist()
result_dt <- result_dt[learner_id != "xgboost_untuned"]

plots[["cvrmse"]] <- ggplot(result_dt, aes(x = reorder(learner_id, reg.cvrmse, median), y = reg.cvrmse)) +
  geom_boxplot(
    fill = "#DDDDDD",
    linewidth = 0.1,
    outlier.size = 0.2,
    outlier.color = "black",
    color = "black",
    outlier.alpha = 0.5
  ) +
  labs(x = NULL, y = "CVRMSE") +
  theme_nice() +
  guides(x = guide_axis(angle = 90))

plots[["bias"]] <- ggplot(result_dt, aes(x = reorder(learner_id, reg.cvrmse, median), y = regr.bias)) +
  geom_boxplot(
    fill = "#DDDDDD",
    linewidth = 0.1,
    outlier.size = 0.2,
    outlier.color = "black",
    color = "black",
    outlier.alpha = 0.5
  ) +
  labs(x = NULL, y = "Bias - kWh") +
  theme_nice() +
  guides(x = guide_axis(angle = 90))

plots[["time"]] <- ggplot(result_dt, aes(x = reorder(learner_id, reg.cvrmse, median), y = time_both)) +
  geom_boxplot(
    fill = "#DDDDDD",
    linewidth = 0.1,
    outlier.size = 0.2,
    outlier.color = "black",
    color = "black",
    outlier.alpha = 0.5
  ) +
  labs(x = NULL, y = "Time - s") +
  theme_nice() +
  guides(x = guide_axis(angle = 90))

plots[["final"]] <- plots[["cvrmse"]] + plots[["bias"]] + plots[["time"]] +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 8, face = "bold"))


ggsave(
  filename = "plots/02_Figure_06.pdf",
  plots[["final"]],
  device = cairo_pdf,
  width = 160,
  height = 88,
  units = "mm"
)


# Analyse worst building --------------------------------------------------

# calculate mean energy use per building
meand_demand <- data_dt[customer_id %in% result_dt[learner_id == "rf_untuned", task_id],
  .(mean_demand = mean(demand_spms)),
  by = customer_id
]
result_dt <- merge(result_dt, meand_demand, by.x = "task_id", by.y = "customer_id")
setorder(result_dt, mean_demand)

# Select the worst building and a good performing building with similar mean energy use
worst_data <- data_dt[customer_id == result_dt[learner_id == "rf_untuned", task_id[which.max(reg.cvrmse)]]]
good_data <- data_dt[customer_id == result_dt[reg.cvrmse < 0.3][1, task_id]]

# Plot two winter working days
plot_data <- good_data[as.Date(time_rounded, tz = "Europe/Copenhagen") %in% c(as.Date(c("2022-12-15", "2022-12-16"), tz = "Europe/Copenhagen")), .(time_rounded, demand_spms, zero_water, type = "good result")]
plot_data <- rbind(plot_data, worst_data[as.Date(time_rounded, tz = "Europe/Copenhagen") %in% c(as.Date(c("2022-12-15", "2022-12-16"), tz = "Europe/Copenhagen")), .(time_rounded, zero_water, demand_spms, type = "worst result")])

Sys.setlocale("LC_TIME", "English")

plots[["detailed"]] <- ggplot(plot_data, aes(x = time_rounded, y = demand_spms, shape = zero_water, group = type, color = type)) +
  geom_line(linewidth = 0.1) +
  geom_point(size = 0.7) +
  scale_shape_manual(values = c(17, 16), labels = c("water use", "no water use"), name = NULL) +
  scale_color_manual(values = c("#5AAE61", "#9970AB"), name = NULL) +
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  labs(x = NULL, y = "total energy use - kWh") +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 day") +
  theme_nice() +
  theme(
    legend.spacing = unit(1, "mm"),
    legend.box = "horitontal",
    legend.box.margin = margin(rep(0, 4), unit = "mm"),
    legend.margin = margin(rep(0, 4), unit = "mm")
  )

ggsave(
  filename = "plots/02_Figure_07.pdf",
  plots[["detailed"]],
  device = cairo_pdf,
  width = 88,
  height = 88 * 4 / 6,
  units = "mm"
)
