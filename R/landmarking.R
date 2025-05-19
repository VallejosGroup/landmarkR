#' An S4 class for a landmarking analysis
#'
#' @slot landmarks Array of landmark times.
#' @slot horizons Array of horizon times. Must have the same length as horizons.
#' @slot dataset Dataframe containing relevant data in long format.
#' @slot event_indicator Name of the column indicating event or censoring.
#' @slot baseline_covariates Names of the columns for baseline covariates.
#' @slot biomarkers List whose name indicate columns for biomarker measurements, and values indicate columns for biomarker measurement times
#' @slot ids Name of the column indicating patient id.
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
    horizons = "numeric",
    dataset = "data.frame",
    event_indicator = "character",
    baseline_covariates = "character",
    biomarkers = "list",
    ids = "character",
    event_time = "character",
    risk_sets = "list",
    longitudinal_fits = "list",
    longitudinal_predictions = "list",
    survival_fits = "list"
  )
)

#' Creates an S4 class for a landmarking analysis
#'
#' @slot landmarks Array of landmark times.
#' @slot horizons Array of horizon times. Must have the same length as horizons.
#' @slot dataset Dataframe containing relevant data in long format.
#' @slot event_indicator Name of the column indicating event or censoring.
#' @slot baseline_covariates Names of the columns for baseline covariates.
#' @slot ids Name of the column indicating patient id.
#' @slot event_time Name of the column indicating time of the event/censoring.
#'
#' @returns An object of class Landmarking
#' @export
Landmarking <- function(landmarks, horizons, dataset, event_indicator, baseline_covariates, biomarkers, ids, event_time) {
  new("Landmarking",
      landmarks = landmarks,
      horizons = horizons,
      dataset = dataset,
      event_indicator = event_indicator,
      baseline_covariates = baseline_covariates,
      biomarkers = biomarkers,
      ids = ids,
      event_time = event_time,
      risk_sets = list(),
      longitudinal_fits = list(),
      longitudinal_predictions = list(),
      survival_fits = list()
  )
}

setValidity("Landmarking", function(object) {
  if (length(object@landmarks) != length(object@horizons)) {
    "@landmarks and @horizons must have the same length"
  } else {
    TRUE
  }
})

setGeneric("fit_longitudinal", function(x) standardGeneric("fit_longitudinal"))
setGeneric("fit_survival", function(x) standardGeneric("fit_survival"))

setMethod(
  f = "show",
  signature = "Landmarking",
  definition = function(object) {
    cat("Summary of Landmarking Object:\n")
    cat("  Landmarks:", object@landmarks, "\n")
    cat("  Horizons:", object@horizons, "\n")
    cat("  Number of observations:", nrow(object@dataset), "\n")
    cat("  Event indicator:", object@event_indicator, "\n")
    cat("  Baseline covariates:", paste(object@baseline_covariates, collapse = ", "), "\n")
    cat("  Number of unique IDs:", length(unique(object@dataset[, object@ids])), "\n")
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

# Accessor for horizons
setGeneric("getHorizons", function(object) standardGeneric("getHorizons"))
setMethod("getHorizons", "Landmarking", function(object) object@horizons)

# Accessor for dataset
setGeneric("getDataset", function(object) standardGeneric("getDataset"))
setMethod("getDataset", "Landmarking", function(object) object@dataset)

# Accessor for event
setGeneric("getEvent", function(object) standardGeneric("getEvent"))
setMethod("getEvent", "Landmarking", function(object) object@event)

# Accessor for baseline_covariates
setGeneric("getBaselineCovariates", function(object) standardGeneric("getBaselineCovariates"))
setMethod("getBaselineCovariates", "Landmarking", function(object) object@baseline_covariates)

# Accessor for ids
setGeneric("getIds", function(object) standardGeneric("getIds"))
setMethod("getIds", "Landmarking", function(object) object@ids)

# Accessor for event_time
setGeneric("getEventTime", function(object) standardGeneric("getEventTime"))
setMethod("getEventTime", "Landmarking", function(object) object@event_time)

# Accessor for risk_sets
setGeneric("getRiskSets", function(object) standardGeneric("getRiskSets"))
setMethod("getRiskSets", "Landmarking", function(object) object@risk_sets)

setGeneric("compute_risk_sets", function(x) standardGeneric("compute_risk_sets"))
setMethod("compute_risk_sets", "Landmarking", function(x) {
  x@risk_sets <- list()
  for(i in 1:length(x@landmarks)) {
    x@risk_sets[[i]] <- which(x@dataset[, x@event_time] >= x@landmarks[i])
  }
  x
})

setGeneric("getSurvivalFits", function(object) standardGeneric("getSurvivalFits"))
setMethod("getSurvivalFits", "Landmarking", function(object) object@survival_fits)

setGeneric("longitudinal_wrapper", function(x, ...) standardGeneric("longitudinal_wrapper"), signature = "x")

setGeneric("fit_longitudinal", function(x) standardGeneric("fit_longitudinal"))
setMethod("fit_longitudinal", "Landmarking", function(x) {
  for (i in 1:length(x@landmarks)) {
    x@longitudinal_fits[[i]] <- list()
    x@longitudinal_predictions[[i]] <- list()
    for (biomarker in names(x@biomarkers)) {
      time_covariate <- x@biomarkers[[biomarker]]
      x@longitudinal_fits[[i]][[biomarker]] <- longitudinal_wrapper(x, i, biomarker)
      x@longitudinal_predictions[[i]][[biomarker]] <- predict(x@longitudinal_fits[[i]][[biomarker]])
    }
  }
  x
})

setGeneric("survival_wrapper", function(x, ...) standardGeneric("survival_wrapper"), signature = "x")

setGeneric("fit_survival", function(x) standardGeneric("fit_survival"))
setMethod("fit_survival", "Landmarking", function(x) {
  for (i in 1:length(x@landmarks)) {
    x@survival_fits[[i]] <- survival_wrapper(x, i)
  }
  x
})



