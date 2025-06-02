#' An S4 class for a landmarking analysis
#'
#' @slot landmarks Array of landmark times.
#' @slot data_static Data frame with id, static covariates, censoring indicator and event time.
#' @slot data_dynamic Data frame in long format with id, measurement, measurement time and covariate measured.
#' @slot event_indicator Name of the column indicating event or censoring.
#' @slot dynamic_covariates List whose name indicate columns for biomarker measurements, and values indicate columns for biomarker measurement times
#' @slot ids Name of the column indicating patient id.
#' @slot times Name of the column indicating time in the dynamic df.
#' @slot measurements Name of the column indicating measurement values in the dynamic df.
#' @slot dynamic_covariate_names Name of the column indicating names of the dynamic covariates in the dynamic df.
#' @slot event_time Name of the column indicating time of the event/censoring.
#' @slot risk_sets List of indices.
#' @slot longitudinal_fits List of model fits for the specified landmark times and biomarkers.
#' @slot longitudinal_predictions List of model predictions for the specified landmark times and biomarkers.
#' @slot survival_fits List of survival model fits at each of the specified landmark times.
#'
#' @export
setClass("Landmarking",
  slots = c(
    landmarks = "numeric",
    data_static = "data.frame",
    data_dynamic = "data.frame",
    event_indicator = "character",
    dynamic_covariates = "character",
    ids = "character",
    event_time = "character",
    times = "character",
    measurements = "character",
    dynamic_covariate_names = "character",
    risk_sets = "list",
    longitudinal_fits = "list",
    longitudinal_predictions = "list",
    survival_fits = "list"
  )
)

# Validator for object of class Landmarking
#
# @param object An object of class Landmarking.
#
# @returns TRUE if the input is valid, else a description of the problem
setValidity("Landmarking", function(object) {
  if (!(object@event_indicator %in% colnames(object@data_static))) {
    "@event_indicator must be a column in dataframe @data_static"
  } else if (!(object@ids %in% colnames(object@data_static))) {
    "@ids must be a column in dataframe @data_static"
  } else if (!(object@ids %in% colnames(object@data_dynamic))) {
    "@ids must be a column in dataframe @data_dynamic"
  } else if ((!(object@event_time %in% colnames(object@data_static)))) {
    "@event_time must be a column in dataframe @data_static"
  } else {
    TRUE
  }
})

#' Creates an S4 class for a landmarking analysis
#'
#' @param data_static Data frame with id, static covariates, censoring indicator and event time.
#' @param data_dynamic Data frame in long format with id, measurement, measurement time and covariate measured.
#' @param event_indicator Name of the column indicating event or censoring.
#' @param dynamic_covariates Names of dynamic (time-varying) covariates.
#' @param ids Name of the column indicating patient id.
#' @param event_time Name of the column indicating time of the event/censoring.
#' @slot times Name of the column indicating time in the dynamic df.
#' @slot measurements Name of the column indicating measurement values in the dynamic df.
#' @slot dynamic_covariate_names Name of the column indicating names of the dynamic covariates in the dynamic df.
#'
#' @returns An object of class Landmarking
#' @export
Landmarking <- function(data_static, data_dynamic, event_indicator, dynamic_covariates, ids, event_time, times, measurements, dynamic_covariate_names) {
  new("Landmarking",
      data_static = data_static,
      data_dynamic = data_dynamic,
      event_indicator = event_indicator,
      dynamic_covariates = dynamic_covariates,
      ids = ids,
      event_time = event_time,
      times = times,
      measurements = measurements,
      dynamic_covariate_names = dynamic_covariate_names,
      risk_sets = list(),
      longitudinal_fits = list(),
      longitudinal_predictions = list(),
      survival_fits = list()
  )
}

# show method for class "Landmarking"
setMethod(
  f = "show",
  signature = "Landmarking",
  definition = function(object) {
    cat("Summary of Landmarking Object:\n")
    cat("  Landmarks:", object@landmarks, "\n")
    cat("  Number of observations:", nrow(object@data_static), "\n")
    cat("  Event indicator:", object@event_indicator, "\n")
    cat("  Event time:", object@event_time, "\n")
    cat("  Risk sets:", "\n")
    if (length(object@risk_sets) > 0) {
      for(i in 1:length(object@risk_sets)) {
        cat("    Landmark ", object@landmarks[i], ": ", head(object@risk_sets[[i]], 10), " ...\n")
      }
    }
  }
)

# Accessor for landmarks
setGeneric("getLandmarks", function(object) standardGeneric("getLandmarks"))
setMethod("getLandmarks", "Landmarking", function(object) object@landmarks)

# Accessor for event
setGeneric("getEvent", function(object) standardGeneric("getEvent"))
setMethod("getEvent", "Landmarking", function(object) object@event)

# Accessor for ids
setGeneric("getIds", function(object) standardGeneric("getIds"))
setMethod("getIds", "Landmarking", function(object) object@ids)

# Accessor for event_time
setGeneric("getEventTime", function(object) standardGeneric("getEventTime"))
setMethod("getEventTime", "Landmarking", function(object) object@event_time)

# Accessor for risk_sets
setGeneric("getRiskSets", function(object) standardGeneric("getRiskSets"))
setMethod("getRiskSets", "Landmarking", function(object) object@risk_sets)



#' Compute the list of individuals at risk at given landmark times,
#' and stores them in an object of class Landmarking
#'
#' @param x An object of type Landmarking
#' @param landmarks Vector of landmark times
#' @param ... Additional arguments (not used)
#'
#' @returns An object of class Landmarking, including desired risk sets for the relevant landmark times.
#' @export
#'
#' @examples
setGeneric("compute_risk_sets", function(x, landmarks, ...) standardGeneric("compute_risk_sets"))


#' Compute the list of individuals at risk at given landmark times,
#' and stores them in an object of class Landmarking
#'
#' @param x An object of type Landmarking
#' @param landmarks Vector of landmark times
#' @param ... Additional arguments (not used)
#'
#' @returns An object of class Landmarking, including desired risk sets for the relevant landmark times.
#' @export
#'
#' @examples
setMethod("compute_risk_sets", "Landmarking", function(x, landmarks, ...) {
  if (length(landmarks) == 1) { # If the vector of landmark times is of length 1
    if (landmarks %in% x@landmarks) { # Risk set for given landmark time was already in memory
      warning("Risk set for landmark time ", landmarks, " already computed")
    }
    x@landmarks <- c(x@landmarks, landmarks) # Add landmark time to the Landmarking object
    # Compute risk set for given landmark time
    x@risk_sets[[as.character(landmarks)]] <- which(x@data_static[, x@event_time] >= landmarks)
  } else {
    # Recursion to compute risk sets one-by-one
    x <- compute_risk_sets(x, landmarks[1])
    x <- compute_risk_sets(x, landmarks[-1])
  }
  x
})

# Accessor for survival fits
setGeneric("getSurvivalFits", function(object) standardGeneric("getSurvivalFits"))
setMethod("getSurvivalFits", "Landmarking", function(object) object@survival_fits)





