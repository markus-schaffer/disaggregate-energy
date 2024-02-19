fn_similarity <- function(data) {
  features <- colnames(data)[grepl("zero_water|time|demand_spms_lag_(1|2)$|demand_spms_lead_(1|2)$", colnames(data))]
  features <- features[features != "reading_time"]
  feature_dt <- subset(data, select = features)
  feature_dt[, colnames(feature_dt)[-1] := map(.SD, ~ scale(.x)), .SDcols = colnames(feature_dt)[-1]]
  # Clustering
  clusters <- kmeans(as.matrix(feature_dt[, -"zero_water"]), centers = 2, iter.max = 50, nstart = 10)$cluster - 1
  result <- data.table(mcc = abs(mlr3measures::mcc(truth = data[, as.factor(as.numeric(zero_water))], response = as.factor(clusters), positive = "0")))
  # Cramer Test
  sample1 <- as.matrix(feature_dt[zero_water == TRUE, -"zero_water"])
  sample2 <- as.matrix(feature_dt[zero_water == FALSE, -"zero_water"])
  # cramer_statistic <- cramer.test(x = sample1, y = sample2, just.statistic = TRUE)
  # result[, cramer_statistic := cramer_statistic$statistic]
  result[, cramer_statistic := cramer.test(x = sample1, y = sample2, just.statistic = T)$statistic]
  result[, colnames(data[1, .(house_nr)]) := data[1, .(house_nr)]]
  return(result)
}

fn_disaggrgate <- function(data) {
  # Load CVRMSE
  source("functions/cvrmse_measure.R")

  # Define training and prediction data
  features <- colnames(data)[grepl("time|demand_spms", colnames(data))]
  features <- features[features != "reading_time"]
  train_data_subset <- subset(data[zero_water == TRUE], select = features)
  predict_data_subset <- subset(data[zero_water == FALSE], select = features)
  task <- as_task_regr(train_data_subset, target = "demand_spms", id = data[1, paste0(house_nr)])

  # Outer resampling for CV
  resampling <- rsmp("cv", folds = 10) # Outer resampling

  # RF not tuned
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

  # DHW & SH calculation
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
  data[, dhw_max := total_water_m3_demand * (1 / dhw_cold_sv) * ((dhw_hot_c - dhw_cold_c) * ((dhw_hot_enthalpy - dhw_cold_enthalpy) / (dhw_hot_c - dhw_cold_c)) / 3600)]
  # Plausibility check
  data[, dhw_calculated := fcase(
    dhw_calculated > dhw_max, dhw_max,
    dhw_calculated < 0, 0,
    dhw_calculated <= dhw_max & dhw_calculated >= 0, dhw_calculated
  )]
  # Recalculate SH
  data[, sh_calculated := demand_spms - dhw_calculated]
  data[, (features[features != "demand_spms"]) := NULL]
  return(data)
}