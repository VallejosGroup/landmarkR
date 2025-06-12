#' Title
#'
#' @slot folds List of Landmarking objects, one per cross-validation fold.
#'
#' @export
setClass("LandmarkingCV",
  slots = c(
    folds = "list"
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
  folds <- c(0, rep(1:K, floor(N/K)))
  if (N %% K > 0) {
    folds <- c(folds, 1:(N %% K))
  }

  folds = list()
  for (k in 1:K) {
    folds[[k]] <- Landmarking(data_static,
                              data_dynamic,
                              event_indicator,
                              ids,
                              event_time,
                              times,
                              measurements)
  }
  new("LandmarkingCV",
      folds = folds
  )
}
