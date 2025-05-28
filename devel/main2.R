library(tidyverse)
devtools::load_all()

data("epileptic")
epileptic |> head()

# DF with Static covariates
df_static <- epileptic |>
  select(id, with.time, with.status, treat, age, gender, learn.dis) |>
  rename(patient.id = id, event.time = with.time, event.status = with.status) |>
  unique()

head(df_static)
str(df_static)

# DF with Dynamic covariates
df_dynamic <- epileptic |>
  select(id, time, dose) |>
  mutate(covariate = "dose") |>
  rename(patient.id = id, measurement = dose, measurement_time = time)

landmarking_object <- Landmarking(
  data_static = df_static,
  data_dynamic = df_dynamic,
  event_indicator = "event.status",
  dynamic_covariates = "dose",
  ids = "patient.id",
  event_time = "event.time"
)

landmarks <- 365.25*c(1:5)

landmarking_object <- landmarking_object |>
  compute_risk_sets(landmarks)

horizons <- 365.25 + landmarks
method <- "coxph"
dynamic_covariates <- c()

landmarking_object <- landmarking_object |>
  fit_survival(landmarks, horizons, method, dynamic_covariates)

landmarking_object <- Landmarking(
  data_static = df_static,
  data_dynamic = df_dynamic,
  event_indicator = "event.status",
  dynamic_covariates = "dose",
  ids = "patient.id",
  event_time = "event.time"
)

landmarks <- 365.25*c(1:5)

landmarking_object <- landmarking_object |>
  compute_risk_sets(landmarks)

horizons <- 365.25 + landmarks
method <- "coxph"
dynamic_covariates <- c("dose")

landmarking_object <- landmarking_object |>
  compute_risk_sets(landmarks) |>
  fit_longitudinal(landmarks, "lme4", c("treat", "age", "gender", "learn.dis")) |>
  predict_longitudinal(landmarks, "lme4", c("treat", "age", "gender", "learn.dis")) |>
  fit_survival(landmarks, horizons, method, dynamic_covariates)

landmarking_object <- Landmarking(
  data_static = df_static,
  data_dynamic = df_dynamic,
  event_indicator = "event.status",
  dynamic_covariates = "dose",
  ids = "patient.id",
  event_time = "event.time"
)

landmarking_object <- landmarking_object |>
  compute_risk_sets(landmarks) |>
  fit_longitudinal(landmarks, "lcmm", c("treat", "age", "gender", "learn.dis"),
                   ng = 2, mixture = ~treat+age+gender+learn.dis) |>
  predict_longitudinal(landmarks, "lcmm", c("treat", "age", "gender", "learn.dis")) |>
  fit_survival(landmarks, horizons, method, dynamic_covariates)
