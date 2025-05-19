### setClass("LandmarkingLongitudinal",
###   contains = "Landmarking",
###   slots = c(
###     landmarking = "Landmarking",
###     fits = "list",
###     predictions = "list"
###   )
### )

### LandmarkingLongitudinal <- function(landmarking) {
###   new("LandmarkingLongitudinal", landmarking = landmarking, fits = list(), predictions = list())
### }
###
### fit_longitudinal_model <-function(landmarking) {
###   if(!(is(landmarking) == "Landmarking")) {
###     "fit_longitudinal_model expects as argument an object of class Landmarking."
###   }
###   fits <- list()
###   formula <- as.formula(paste0(landmarking@))
###   for (i in 1:length(landmarking@landmarks)) {
###     fits[[i]] <- lme4::lmer()
###   }
###   fits
### }
###
### setMethod(
###   f = "show",
###   signature = "LandmarkingLongitudinal",
###   definition = function(object) {
###     show(object@landmarking)
###   }
### )
