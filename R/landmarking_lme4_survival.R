### setClass("LandmarkingLme4Survival",
###   contains = "Landmarking"
### )
###
### LandmarkingLme4Survival <- function(landmarks, horizons, dataset, event_indicator, baseline_covariates, biomarkers, ids, event_time) {
###   new("LandmarkingLme4Survival",
###       landmarks = landmarks,
###       horizons = horizons,
###       dataset = dataset,
###       event_indicator = event_indicator,
###       baseline_covariates = baseline_covariates,
###       biomarkers = biomarkers,
###       ids = ids,
###       event_time = event_time,
###       risk_sets = list(),
###       longitudinal_fits = list(),
###       longitudinal_predictions = list(),
###       survival_fits = list()
###   )
### }
###
### setMethod("longitudinal_wrapper", "LandmarkingLme4Survival", function(x, landmark_index, biomarker) {
###   longitudinal_formula <- as.formula(paste0(
###     biomarker, " ~ ",
###     paste(x@baseline_covariates, collapse =  " + "), " + ",
###     "(", x@biomarkers[[biomarker]], "|" , x@ids, ")"
###   ))
###   at_risk_individuals <- x@risk_sets[[landmark_index]]
###   lme4::lmer(longitudinal_formula, data = x@dataset[at_risk_individuals, ])
### })
###
### setMethod("survival_wrapper", "LandmarkingLme4Survival", function(x, landmark_index) {
###   at_risk_individuals <- x@risk_sets[[landmark_index]]
###   dataset <- x@dataset[at_risk_individuals, ] |> group_by(!! sym(x@ids)) |> slice_tail(n=1) |> ungroup()
###   for (i in 1:length(x@biomarkers)) {
###     marker <- names(x@biomarkers)[i]
###     dataset <- cbind(dataset, x@longitudinal_predictions[[landmark_index]][[marker]])
###     colnames(dataset)[ncol(dataset)] <- marker
###   }
###   survival_formula <- as.formula(paste0(
###     "Surv(", x@event_time, ", ", x@event_indicator, ") ~ ", paste0(names(x@biomarkers), collapse = " + ")
###   ))
###
###   survival::survfit(survival_formula, data = dataset)
### })
