library(tidyverse)
devtools::load_all()

data("epileptic")
epileptic |> head()

# DF with Static covariates
df_static <- epileptic |>
  select(id, with.time, with.status, treat, age, gender, learn.dis) |>
  rename(patient_id = id, event.time = with.time, event.status = with.status) |>
  unique()
df_static$age <- (df_static$age - mean(df_static$age))/sd(df_static$age)

head(df_static)
str(df_static)

# DF with Dynamic covariates
df_dynamic <- epileptic |>
  select(id, time, dose) |>
  mutate(covariate = "dose") |>
  rename(patient_id = id, times = time, measurements = dose, covariates = covariate)

landmarking_object <- Landmarking(
  data_static = df_static,
  data_dynamic = df_dynamic,
  event_indicator = "event.status",
  dynamic_covariates = "dose",
  ids = "patient_id",
  event_time = "event.time",
  times = "times",
  measurements = "measurements",
  dynamic_covariate_names = "covariates"
)

landmarking_object <- landmarking_object |>
  compute_risk_sets(landmarks = seq(from = 365.25, to = 5*365.25, by = 365.25))

landmarking_object <- landmarking_object |>
  fit_survival(
    landmarks = seq(from = 365.25, to = 5*365.25, by = 365.25),
    horizons = seq(from = 2*365.25, to = 6*365.25, by = 365.25),
    method = "coxph",
    dynamic_covariates = c()
  )

landmarking_object <- Landmarking(
  data_static = df_static,
  data_dynamic = df_dynamic,
  event_indicator = "event.status",
  dynamic_covariates = "dose",
  ids = "patient_id",
  event_time = "event.time",
  times = "times",
  measurements = "measurements",
  dynamic_covariate_names = "covariates"
)

landmarking_object <- landmarking_object |>
  compute_risk_sets(seq(from = 365.25, to = 5*365.25, by = 365.25)) |>
  fit_longitudinal(
    landmarks = seq(from = 365.25, to = 5*365.25, by = 365.25),
    method = "lcmm",
    formula = measurements ~ treat + age + gender + learn.dis,
    mixture = ~treat+age+gender+learn.dis,
    subject = "patient_id",
    ng = 2
  ) |>
  predict_longitudinal(
    landmarks = seq(from = 365.25, to = 5*365.25, by = 365.25),
    method = "lcmm",
    subject = "patient_id",
    avg = FALSE
  ) |>
  fit_survival(
    landmarks = seq(from = 365.25, to = 5*365.25, by = 365.25),
    horizons = seq(from = 2*365.25, to =6*365.25, by = 365.25),
    method = "coxph",
    dynamic_covariates = c("dose")
  )


landmarking_object <- Landmarking(
  data_static = df_static,
  data_dynamic = df_dynamic,
  event_indicator = "event.status",
  dynamic_covariates = "dose",
  ids = "patient_id",
  event_time = "event.time",
  times = "times",
  measurements = "measurements",
  dynamic_covariate_names = "covariates"
)

landmarking_object <- landmarking_object |>
  compute_risk_sets(
    landmarks = seq(from = 365.25, to = 5*365.25, by = 365.25)
  )

landmarking_object <- landmarking_object |>
  fit_longitudinal(
    landmarks = seq(from = 365.25, to = 5*365.25, by = 365.25),
    method = "lme4",
    formula = measurements ~ treat + age + gender + learn.dis + (times|patient_id)
  ) |>
  predict_longitudinal(
    landmarks = seq(from = 365.25, to = 5*365.25, by = 365.25),
    method = "lme4",
    allow.new.levels = TRUE
  ) |>
  fit_survival(
    landmarks = seq(from = 365.25, to = 5*365.25, by = 365.25),
    horizons = seq(from = 2*365.25, to =6*365.25, by = 365.25),
    method = survival::coxph,
    dynamic_covariates = c("dose")
  )

