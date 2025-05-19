setClass("LandmarkingLOCFSurvival",
  contains = "Landmarking"
)

LandmarkingLOCFSurvival <- function(landmarks, horizons, dataset, event_indicator, baseline_covariates, biomarkers, ids, event_time) {
  new("LandmarkingLOCFSurvival",
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

setMethod("survival_wrapper", "LandmarkingLOCFSurvival", function(x, landmark_index) {
  survival_formula <- as.formula(paste0("Surv(", x@event_time, ", ", x@event_indicator, ") ~ 1"))
  at_risk_individuals <- x@risk_sets[[landmark_index]]
  survival::survfit(survival_formula, data = x@dataset[at_risk_individuals, ])
})


