#' An S4 class for a landmarking analysis
#'
#' @slot landmarks Array of landmark times.
#' @slot data_static Data frame with id, static covariates, censoring indicator and event time.
#' @slot data_dynamic Data frame in long format with id, measurement, measurement time and covariate measured.
#' @slot event_indicator Name of the column indicating event or censoring.
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
    data_static = "data.frame",
    data_dynamic = "data.frame",
    event_indicator = "character",
    biomarkers = "list",
    ids = "character",
    event_time = "character",
    risk_sets = "list",
    longitudinal_fits = "list",
    longitudinal_predictions = "list",
    survival_fits = "list"
  )
)

#' Validator for object of class Landmarking
#'
#' @param object An object of class Landmarking.
#'
#' @returns TRUE if the input is valid, else a description of the problem
#' @export
#'
#' @examples
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
#' @slot data_static Data frame with id, static covariates, censoring indicator and event time.
#' @slot data_dynamic Data frame in long format with id, measurement, measurement time and covariate measured.
#' @slot event_indicator Name of the column indicating event or censoring.
#' @slot biomarkers List whose name indicate columns for biomarker measurements, and values indicate columns for biomarker measurement times
#' @slot ids Name of the column indicating patient id.
#' @slot event_time Name of the column indicating time of the event/censoring.
#'
#' @returns An object of class Landmarking
#' @export
Landmarking <- function(data_static, data_dynamic, event_indicator, biomarkers, ids, event_time) {
  new("Landmarking",
      data_static = data_static,
      data_dynamic = data_dynamic,
      event_indicator = event_indicator,
      biomarkers = biomarkers,
      ids = ids,
      event_time = event_time,
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


#' Fits the specified survival model at the landmark times and up to the horizon
#' times specified by the user
#'
#' @param x An object of class Landmarking.
#' @param horizons Vector of horizon times corresponding to the landmark times.
#' @param horizons Vector of horizon times.
#' @param method Method for survival analysis, either "survfit" or "coxph".
#' @param dynamic_covariates Vector of time-varying covariates that to be used in the survival model.
#'
#' @returns An object of class Landmarking.
#' @export
#'
#' @examples
setGeneric("fit_survival", function(x, landmarks, horizons, method, dynamic_covariates = c()) standardGeneric("fit_survival"))
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
    dataset <- data_static[at_risk_individuals, ] |>
      mutate(event_status = ifelse(event_time > horizons, 0, event_status), event_time = ifelse(event_time > horizons, horizons, event_time))
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

#' Fits the specified longitudinal model for the latent processes underlying the
#' relevant time-varying covariates, up until the landmarking times
#'
#' @param x An object of class Landmarking.
#' @param landmarks A vector of Landmark times.
#' @param method Method for longitudinal analysis, currently only "lme4" or "lcmm" is supported.
#' @param static_covariates Vector of names of static covariates to be included in the longitudinal analysis.
#' @returns An object of class Landmarking.
#' @export
#'
#' @examples
setGeneric("fit_longitudinal", function(x, landmarks, method, static_covariates,...) standardGeneric("fit_longitudinal"))
setMethod("fit_longitudinal", "Landmarking", function(x, landmarks, method, static_covariates,...) {
  # Check that the method for longitudinal analysis is implemented
  if (!(method %in% c("lme4", "lcmm"))) {
    stop("Method ", method, " not supported", "/n")
  }
  # Base case for recursion
  if (length(landmarks) == 1) {
    # Check that relevant risk set is available
    if (!(landmarks %in% x@landmarks)) {
      stop("Risk set for landmark time ",
           landmarks,
           " has not been computed",
           "/n")
    }
    # Create list for storing model fits for longitudinal analysis
    x@longitudinal_fits[[as.character(landmarks)]] <- list()

    # Loop that iterates over all time-varying covariates, to fit a longitudinal model for the underlying trajectories
    for (predictor in names(landmarking_object@biomarkers)) {
      # Risk set for the landmark time
      at_risk_individuals <- x@risk_sets[[as.character(landmarks)]]
      # Construct dataset for the longitudinal analysis (static measurements + time-varying covariate and its recording time)
      dataframe <- x@data_dynamic |>
        filter(covariate == predictor) |>         # Subset with records of the relevant time-varying predictor
        filter(id %in% at_risk_individuals) |>    # Subset with individuals who are at risk only
        filter(measurement_time <= landmarks) |>  # Subset with observations prior to landmark time
        left_join(data_static, by = join_by(id))  # Join with static covariates
      # Construct formula for longitudinal analysis
      longitudinal_formula <- as.formula(paste0("measurement ~ ", paste(static_covariates, collapse = " + "), " + (measurement_time|id)"))
      # Fit longitudinal model according to chosen method
      if (method == "lme4") {
        # Model fit
        lme4_fit <- lme4::lmer(longitudinal_formula, data = dataframe)
        x@longitudinal_fits[[as.character(landmarks)]][[predictor]] <- lme4_fit
        ##### # Model predictions at landmark time
        ##### x@longitudinal_predictions[[as.character(landmarks)]][[predictor]] <-
        #####   predict(lme4_fit,
        #####           newdata =x@data_static |> select(id, matches(paste0(static_covariates, collapse="|"))) |> mutate(measurement_time = landmarks),
        #####           allow.new.levels = TRUE
        #####   )
      } else if (method == "lcmm") {
        # Model fit
        additional_args <- list(...)
        if (!("ng" %in% names(additional_args))) {
          stop("Argument ng (number of clusters) missing. ")
        }
        # Formula for fixed effects
        fixed_formula <- as.formula(paste0("measurement ~ ", paste(static_covariates, collapse = " + ")))
        # Fir LCMM with only one cluster (to facilitate parameter initialisation later n)
        lcmm_fit_init <- lcmm::hlme(fixed_formula, data = dataframe, subject = x@ids, ng = 1)
        # Fit LCMM with desired number of clusters
        lcmm_fit <- lcmm::hlme(fixed_formula, data = dataframe, subject = x@ids, B = lcmm_fit_init, ...)
        # Store formulas in the LCMM  object
        lcmm_fit$call$fixed <- fixed_formula
        lcmm_fit$call$mixture <- additional_args$mixture
        # Store model results in Landmarking objects
        x@longitudinal_fits[[as.character(landmarks)]][[predictor]] <- lcmm_fit
      }
    }
  } else {
    # Recursion
    x <- fit_longitudinal(x, landmarks[1], method, static_covariates, ...)
    x <- fit_longitudinal(x, landmarks[-1], method, static_covariates, ...)
  }
  x
})

#' Make predictions for time-varying covariates at specified landmark times
#'
#' @param x An object of class Landmarking.
#' @param landmarks Vector of landmark times.
#' @param method Longitudinal data analysis method used to make predictions
#'
#' @returns An object of class Landmarking
#' @export
#'
#' @examples
setGeneric("predict_longitudinal", function(x, landmarks, method, static_covariates, ...) standardGeneric("predict_longitudinal"))
setMethod("predict_longitudinal", "Landmarking", function(x, landmarks, method, static_covariates, ...) {
  # Check that the method for longitudinal analysis is implemented
  if (!(method %in% c("lme4", "lcmm"))) {
    stop("Method ", method, " not supported\n")
  }
  # Base case for recursion
  if (length(landmarks) == 1) {
    # Check that relevant risk set is available
    if (!(landmarks %in% x@landmarks)) {
      stop("Risk set for landmark time ", landmarks, " has not been computed\n")
    }
    # Check that relevant model fit is available
    if (!(as.character(landmarks) %in% names(x@longitudinal_fits))) {
      stop("Longitudinal model has not been fit for landmark time",
           landmarks,
           "\n")
    }
    # Relevant risk set
    risk_set <- x@risk_sets[[as.character(landmarks)]]
    # Create list for storing model predictions, for longitudinal analysis
    x@longitudinal_predictions[[as.character(landmarks)]] <- list()
    # Loop that iterates over all time-varying covariates, to fit a longitudinal model for the underlying trajectories
    for (predictor in names(landmarking_object@biomarkers)) {
      # Check that relevant model fit is available
      if (!(predictor %in% names(x@longitudinal_fits[[as.character(landmarks)]]))) {
        stop("Longitudinal model has not been fit for dynamic covariate ",
             predictor,
             " at landmark time",
             landmarks,
             "/n")
      }
      # Risk set for the landmark time
      at_risk_individuals <- x@risk_sets[[as.character(landmarks)]]
      # Fit longitudinal model according to chosen method
      if (method == "lme4") {
        # Model fit
        lme4_fit <- x@longitudinal_fits[[as.character(landmarks)]][[predictor]]
        # Model predictions at landmark tiem
        x@longitudinal_predictions[[as.character(landmarks)]][[predictor]] <-
          predict(
            lme4_fit,
            newdata = x@data_static |>
              filter(id %in% risk_set) |>
              select(id, matches(paste0(static_covariates, collapse="|"))) |>
              mutate(measurement_time = landmarks),
            allow.new.levels = TRUE
          )
      } else if (method == "lcmm") {
        # Model fit
        lcmm_fit <- x@longitudinal_fits[[as.character(landmarks)]][[predictor]]
        # pprob contains probability of observation belonging to a certain cluster
        pprob <- lcmm_fit$pprob
        # Finds out the largest cluster.
        mode_cluster <- as.integer(names(sort(-table(pprob$class)))[1])
        # Allocation of clusters for prediction
        cluster_allocation <- rbind(
          data.frame(id = pprob$id, cluster = pprob$class),
          data.frame(id = setdiff(risk_set, pprob$id), cluster = mode_cluster)
        ) |> arrange(id) |> select(-id)
        rownames(cluster_allocation) <- risk_set
        # Make predictions with lcmm package
        predictions <- lcmm::predictY(
          lcmm_fit,
          newdata = x@data_static |>
            select(id, matches(paste0(static_covariates, collapse="|"))) |>
            mutate(measurement_time = landmarks)
        )
        # Choose correct cluster for prediction
        predictions <- predictions$pred[risk_set, ] * model.matrix(~as.factor(cluster)-1,data=cluster_allocation)
        # Store predictions in Landmarking object
        x@longitudinal_predictions[[as.character(landmarks)]][[predictor]] <- rowSums(predictions)
        names(x@longitudinal_predictions[[as.character(landmarks)]][[predictor]]) <- risk_set
      }
    }
  } else {
    # Recursion
    x <- predict_longitudinal(x, landmarks[1], method, static_covariates, ...)
    x <- predict_longitudinal(x, landmarks[-1], method, static_covariates, ...)
  }
  x
})

