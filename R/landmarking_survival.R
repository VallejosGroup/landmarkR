#' Fits the specified survival model at the landmark times and up to the horizon
#' times specified by the user
#'
#' @param x An object of class Landmarking.
#' @param landmarks Vector of landmark times
#' @param horizons Vector of horizon times corresponding to the landmark times.
#' @param method Method for survival analysis, either "survfit" or "coxph".
#' @param dynamic_covariates Vector of time-varying covariates that to be used in the survival model.
#'
#' @returns An object of class Landmarking.
#' @export
#'
#' @examples
setGeneric("fit_survival", function(x, landmarks, horizons, method, dynamic_covariates = c()) standardGeneric("fit_survival"))

#' Fits the specified survival model at the landmark times and up to the horizon
#' times specified by the user
#'
#' @param x An object of class Landmarking.
#' @param landmarks Vector of landmark times
#' @param horizons Vector of horizon times corresponding to the landmark times.
#' @param method Method for survival analysis, either "survfit" or "coxph".
#' @param dynamic_covariates Vector of time-varying covariates that to be used in the survival model.
#'
#' @returns An object of class Landmarking.
#' @export
#'
#' @examples
setMethod("fit_survival", "Landmarking", function(x, landmarks, horizons, method, dynamic_covariates = c()) {
  # Check that method for survival analysis is available
  if (!(method %in% c("survfit", "coxph"))) {
    message("Method ", method, " not supported")
  }
  # Check that vectors of landmark times and horizons have the same length
  if (length(landmarks) != length(horizons)) {
    message("Arguments landmarks and horizons must have equal length")
  }
  if (length(landmarks) == 1) { # Base case for recursion
    if (!(landmarks %in% x@landmarks)) {
      message("Risk set for landmark time ", landmarks, " has not been computed")
    }
    # Construct formula for survival analysis
    survival_formula <- paste0("Surv(", x@event_time, ", ", x@event_indicator, ") ~ ")
    # Recover risk sets (ids of individuals who are at risk at landmark time)
    at_risk_individuals <- x@risk_sets[[as.character(landmarks)]]
    # Construct dataset for survival analysis (censor observations of events past horizon time)
    dataset <- x@data_static[at_risk_individuals, ] |>
      mutate(
        event_status = ifelse(get(x@event_time) > horizons, 0, get(x@event_indicator)),
        event_time = ifelse(get(x@event_time) > horizons, horizons, get(x@event_time))
      )
    # Add time-varying covariates to formula and dataset for survival analysis
    if (length(dynamic_covariates) == 0) {
      survival_formula <- as.formula(paste0(survival_formula, "1"))
    } else {
      survival_formula <- as.formula(paste0(survival_formula, paste(dynamic_covariates, collapse = " + ")))
      dataset <- cbind(dataset, do.call(cbind, x@longitudinal_predictions[[as.character(landmarks)]]))
    }

    # Call to method that performs survival analysis
    if (method == "coxph") {
      x@survival_fits[[as.character(landmarks)]] <- survival::coxph(survival_formula, data = dataset)
    } else if (method == "survfit") {
      x@survival_fits[[as.character(landmarks)]] <- survival::coxph(survival_formula, data = dataset)
    }
  } else {
    # Recursion
    x <- fit_survival(x, landmarks[1], horizons[1], method, dynamic_covariates)
    x <- fit_survival(x, landmarks[-1], horizons[-1], method, dynamic_covariates)
  }
  x
})
