reg.cvrmse <- R6::R6Class("reg.cvrmse",
  inherit = mlr3::MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        # custom id for the measure
        id = "reg.cvrmse",

        # additional packages required to calculate this measure
        packages = character(),

        # properties, see below
        properties = character(),

        # required predict type of the learner
        predict_type = "response",

        # feasible range of values
        range = c(0, Inf),

        # minimize during tuning?
        minimize = TRUE
      )
    }
  ),
  private = list(
    # custom scoring function operating on the prediction object
    .score = function(prediction, ...) {
      cvrmse <- function(truth, response) {
        sqrt(mean((truth - response)^2))/mean(truth)
      }
      cvrmse(prediction$truth, prediction$response)
    }
  )
)

mlr3::mlr_measures$add("reg.cvrmse", reg.cvrmse)

