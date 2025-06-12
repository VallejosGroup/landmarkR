#' Title
#'
#' @slot folds Vector of integers associating observations to CV folds.
#' @slot landmarking_list List of Landmarking objects, one per CV fold.
#'
#' @export
setClass("LandmarkingCV",
  slots = c(
    folds = "integer",
    landmarking_list = "list"
  )
)

#' Title
#'
#' @inheritParams Landmarking
#' @param K Number of cross-validation folds.
#'
#' @returns An object of class LandmarkingCV.
#' @export
LandmarkingCV <- function(data_static,
                          data_dynamic,
                          event_indicator,
                          ids,
                          event_time,
                          times,
                          measurements,
                          K = 10) {
  N <- nrow(data_static)
  individuals <- data_static[, ids]
  folds <- rep(1:K, floor(N/K))
  if (N %% K > 0) {
    folds <- c(folds, 1:(N %% K))
  }
  folds <- sample(folds)
  names(folds) <- individuals

  landmarking_list <- list()
  for (k in 1:K) {

    data_static_training <- data_static[which(folds != k), ]
    ids_in_training <- data_static_training[, ids]
    data_dynamic_training <- list()
    for (predictor in names(data_dynamic)) {
      data_dynamic_training[[predictor]] <- data_dynamic[[predictor]] |>
        filter(get(ids) %in% ids_in_training)
    }

    landmarking_list[[k]] <- Landmarking(data_static_training,
                              data_dynamic_training,
                              event_indicator,
                              ids,
                              event_time,
                              times,
                              measurements)
  }
  new("LandmarkingCV",
      folds = folds,
      landmarking_list = landmarking_list
  )
}


