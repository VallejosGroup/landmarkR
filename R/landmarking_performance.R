#' C-index
#'
#' Computes concordance index (c-index) at the desired landmark times and
#' prediction windows.
#'
#' @param x
#' @param landmarks
#' @param windows
#'
#' @returns
#' @export
#'
#' @examples
setGeneric(
 "c_index",
 function(x, landmarks, windows) {
   standardGeneric("c_index")
 }
)

setMethod(
 "c_index",
 "Landmarking",
 function(x, landmarks, windows) {
   # TODO
 }
)
#' Brier Score
#'
#' Computes concordance index (c-index) at the desired landmark times and
#' prediction windows.
#'
#' @param x
#' @param landmarks
#' @param windows
#'
#' @returns
#' @export
#'
#' @examples
setGeneric(
 "brier_score",
 function(x, landmarks, windows) {
   standardGeneric("brier_score")
 }
)

setMethod(
 "brier_score",
 "Landmarking",
 function(x, landmarks, windows) {
   error_str <- NULL
   if (!(is(x) == "Landmarking")) {

   }
   scores <- list()
   for (landmark in landmarks) {
     scores[[as.character(landmark)]] <- list()
     for (window in windows) {
       horizon <- landmark + window
       at_risk_individuals <- x@risk_sets[[as.character(landmark)]]

       # Construct dataset for survival analysis (censor events past horizon time)
       dataset <- x@data_static[at_risk_individuals, ] |>
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

       predictions <- x@survival_predictions[[paste0(horizon, "-", window)]]
       scores[[as.character(landmark)]][[as.character(window)]] <-
         BinaryBrierScore(
           predictions = predictions,
           time = dataset$event_time,
           status = dataset$event_status,
           tau = horizon,
           cause = 1
       )
     }
   }
   return(scores)
 }
)
