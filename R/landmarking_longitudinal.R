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
    stop("Method ", method, " not supported", "\n")
  }
  # Base case for recursion
  if (length(landmarks) == 1) {
    # Check that relevant risk set is available
    if (!(landmarks %in% x@landmarks)) {
      stop("Risk set for landmark time ",
           landmarks,
           " has not been computed",
           "\n")
    }
    # Create list for storing model fits for longitudinal analysis
    x@longitudinal_fits[[as.character(landmarks)]] <- list()

    # Loop that iterates over all time-varying covariates, to fit a longitudinal model for the underlying trajectories
    for (predictor in x@dynamic_covariates) {
      # Risk set for the landmark time
      at_risk_individuals <- x@risk_sets[[as.character(landmarks)]]
      # Construct dataset for the longitudinal analysis (static measurements + time-varying covariate and its recording time)
      dataframe <- x@data_dynamic |>
        filter(covariate == predictor) |>         # Subset with records of the relevant time-varying predictor
        filter(get(x@ids) %in% at_risk_individuals) |>    # Subset with individuals who are at risk only
        filter(measurement_time <= landmarks) |>  # Subset with observations prior to landmark time
        left_join(x@data_static, by = x@ids)  # Join with static covariates
      # Construct formula for longitudinal analysis
      longitudinal_formula <- as.formula(paste0(
        "measurement ~ ",
        paste(static_covariates, collapse = " + "),
        " + (measurement_time|", x@ids, ")")
      )
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
    for (predictor in x@dynamic_covariates) {
      # Check that relevant model fit is available
      if (!(predictor %in% names(x@longitudinal_fits[[as.character(landmarks)]]))) {
        stop("Longitudinal model has not been fit for dynamic covariate ",
             predictor,
             " at landmark time",
             landmarks,
             "\n")
      }
      # Risk set for the landmark time
      at_risk_individuals <- x@risk_sets[[as.character(landmarks)]]
      # Fit longitudinal model according to chosen method
      if (method == "lme4") {
        # Model fit
        lme4_fit <- x@longitudinal_fits[[as.character(landmarks)]][[predictor]]
        # Model predictions at landmark time
        x@longitudinal_predictions[[as.character(landmarks)]][[predictor]] <-
          predict(
            lme4_fit,
            newdata = x@data_static |>
              filter(get(x@ids) %in% risk_set) |>
              select(x@ids, matches(paste0(static_covariates, collapse="|"))) |>
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
        if (length(risk_set) == nrow(pprob)) {
          cluster_allocation <- data.frame(id = pprob[,x@ids], cluster = pprob$class)
        } else {
          cluster_allocation <- rbind(
            data.frame(id = pprob[,x@ids], cluster = pprob$class),
            data.frame(id = setdiff(risk_set, pprob[,x@ids]), cluster = mode_cluster)
          ) |> arrange(id) |> select(-id)
        }
        rownames(cluster_allocation) <- risk_set
        # Make predictions with lcmm package
        predictions <- lcmm::predictY(
          lcmm_fit,
          newdata = x@data_static |>
            select(x@ids, matches(paste0(static_covariates, collapse="|"))) |>
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
