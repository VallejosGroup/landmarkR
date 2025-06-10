### #' C-index
### #'
### #' Computes concordance index (c-index) at the desired landmark times and
### #' prediction windows.
### #'
### #' @param x
### #' @param landmarks
### #' @param windows
### #'
### #' @returns
### #' @export
### #'
### #' @examples
### setGeneric(
###   "c_index",
###   function(x, landmarks, windows) {
###     standardGeneric("c_index")
###   }
### )
###
### setMethod(
###   "c_index",
###   "Landmarking",
###   function(x, landmarks, windows) {
###     # TODO
###   }
### )
###
### #' Brier Score
### #'
### #' Computes concordance index (c-index) at the desired landmark times and
### #' prediction windows.
### #'
### #' @param x
### #' @param landmarks
### #' @param windows
### #'
### #' @returns
### #' @export
### #'
### #' @examples
### setGeneric(
###   "brier_score",
###   function(x, landmarks, windows) {
###     standardGeneric("brier_score")
###   }
### )
###
### setMethod(
###   "brier_score",
###   "Landmarking",
###   function(x, landmarks, windows) {
###     scores <- list()
###     for (model_name in names(landmarking_object@survival_predictions)) {
###       predictions <- landmarking_object@survival_predictions[[model_name]]
###       BinaryBrierScore(
###         predictions,
###       )
###     }
###   }
### )
###
