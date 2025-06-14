#' Fits the specified survival model at the landmark times and up to the horizon
#' times specified by the user
#'
#' @param x An object of class \code{\link{Landmarking}}.
#' @param landmarks Numeric vector of landmark times
#' @param windows Vector of prediction windows determining horizon times.
#' @param method Method for survival analysis, either "survfit" or "coxph".
#' @param dynamic_covariates Vector of time-varying covariates to be used
#' in the survival model.
#'
#' @returns An object of class Landmarking.
#' @export
#'
#' @examples
setGeneric(
  "fit_survival",
  function(x,
           landmarks,
           windows,
           method,
           dynamic_covariates = c()) {
    standardGeneric("fit_survival")
  }
)

#' Fits the specified survival model at the landmark times and up to the horizon
#' times specified by the user
#'
#' @inheritParams fit_survival
#'
#' @returns An object of class Landmarking.
#' @export
#'
#' @examples
setMethod(
  "fit_survival",
  "Landmarking",
  function(x,
           landmarks,
           windows,
           method,
           dynamic_covariates = c()) {
    # Check that method is a function with arguments formula, data, ...
    if (is(method)[1] == "character" && method == "survfit") {
      method <- survival::survfit
    } else if (is(method)[1] == "character" && method == "coxph") {
      method <- survival::coxph
    }
    if (!(is(method)[1] == "function")) {
      stop(
        "Argument ",
        method,
        " must be a function",
        "\n"
      )
    }
    if (!("data" %in% names(as.list(args(method))))) {
      stop(
        "Argument ",
        method,
        " must be a function, and data must be an argument to that function",
        "\n"
      )
    }
    # Check that vectors of landmark times and horizons have the same length
    if (length(landmarks) == 1) { # Base case for recursion
      if (!(landmarks %in% x@landmarks)) {
        message(
          "Risk set for landmark time ",
          landmarks,
          " has not been computed\n"
        )
      }
      # Recover risk sets (ids of individuals who are at risk at landmark time)
      at_risk_individuals <- x@risk_sets[[as.character(landmarks)]]

      for (window in windows) {
        horizon <- landmarks + window
        # Construct dataset for survival analysis (censor events past horizon time)
        dataset <- x@data_static[which(x@data_static[, x@event_time] >= landmarks), ] |>
          mutate(
            event_status = ifelse(get(x@event_time) > horizon,
              0,
              get(x@event_indicator)
            ),
            event_time = ifelse(get(x@event_time) > horizon,
              horizon,
              get(x@event_time)
            )
          )
        # Construct formula for survival analysis
        survival_formula <- paste0(
          "survival::Surv(",
          "event_time",
          ", ",
          "event_status",
          ") ~ "
        )
        # Add time-varying covariates to formula and dataset for survival analysis
        if (length(dynamic_covariates) == 0) {
          survival_formula <- as.formula(paste0(survival_formula, "1"))
        } else {
          survival_formula <- as.formula(paste0(
            survival_formula,
            paste(dynamic_covariates,
                  collapse = " + "
            )
          ))
        dataset <- cbind(
          dataset,
          do.call(
            cbind,
            x@longitudinal_predictions[[as.character(landmarks)]]
          )
        )
        }
        # Call to method that performs survival analysis
        x@survival_fits[[paste0(landmarks, "-", window)]] <- method(survival_formula,
          data = dataset
        )
      }
    } else {
      # Recursion
      x <- fit_survival(
        x,
        landmarks[1],
        windows,
        method,
        dynamic_covariates
      )
      x <- fit_survival(
        x,
        landmarks[-1],
        windows,
        method,
        dynamic_covariates
      )
    }
    x
  }
)

#' Make predictions for time-to-event outcomes at specified horizon times
#'
#' @param x An object of class \code{\link{Landmarking}}.
#' @param landmarks A numeric vector of landmark times.
#' @param windows Vector of prediction windows determining horizon times.
#' @param method R function that is used to make predictions
#' @param ... Additional arguments passed to the prediction function (e.g.
#'   number of classes/clusters for lcmm).
#'
#' @returns An object of class \code{\link{Landmarking}}.
#' @export
#'
#' @examples
setGeneric(
  "predict_survival",
  function(x, landmarks, windows, method, ...) {
    standardGeneric("predict_survival")
  }
)

#' Make predictions for time-to-event outcomes at specified horizon times
#'
#' @inheritParams predict_survival
#'
#' @returns An object of class \code{\link{Landmarking}}.
#' @export
#'
#' @examples
setMethod(
  "predict_survival", "Landmarking",
  function(x, landmarks, windows, method, ...) {
    # Check that method is a function with arguments formula, data, ...
    if (is(method)[1] == "character" && method == "coxph") {
      method <- predict
    }
    if (!(is(method)[1] == "function")) {
      stop(
        "Argument method",
        " must be a function",
        "\n"
      )
    }
    # Base case for recursion
    if (length(landmarks) == 1) {
      # Check that relevant risk set is available
      if (!(landmarks %in% x@landmarks)) {
        stop("Risk set for landmark time ", landmarks, " has not been computed\n")
      }
      for (window in windows) {
        model_name <- paste0(landmarks, "-", window)
        # Check that relevant model fit is available
        if (!(model_name %in% names(x@survival_fits))) {
          stop(
            "Survival model has not been fit for prediction window",
            window,
            " at landmark time",
            landmarks,
            "\n"
          )
        }

        x@survival_predictions[[model_name]] <- method(
          x@survival_fits[[model_name]],
          ...
        )
      }
    } else {
      # Recursion
      x <- predict_survival(x, landmarks[1],  windows, method, ...)
      x <- predict_survival(x, landmarks[-1], windows, method, ...)
    }
    x
  }
)
