test_that("Validity checks for Landmarking class work", {
  # Data manipulation
  data(epileptic)
  static <- epileptic %>%
    distinct(id, .keep_all = TRUE) %>%
    select(-time, -dose)

  dynamic <- epileptic %>%
    select(-with.time, -with.status, -treat, -gender, -learn.dis, -age) %>%
    mutate(value = dose, type = "dose") %>%
    select(-dose)

  # Test: event_indicator column missing from data_static
  expect_error(
    landmarkR::Landmarking(
      data_static = static,
      data_dynamic = dynamic,
      event_indicator = "missing.column",
      dynamic_covariates = c("dose"),
      ids = "ids",
      event_time = "with.time",
      times = "time",
      measurements = "value",
      dynamic_covariate_names = "type"
    ),
    "@event_indicator must be a column in dataframe @data_static"
  )

  # Test: ids column missing from data_static
  expect_error(
    landmarkR::Landmarking(
      data_static = static,
      data_dynamic = dynamic,
      event_indicator = "with.status",
      dynamic_covariates = c("dose"),
      ids = "missing.column",
      event_time = "with.time",
      times = "time",
      measurements = "value",
      dynamic_covariate_names = "type"
    ),
    "@ids must be a column in dataframe @data_static"
  )

  # Test: event_time column missing from data_static
  expect_error(
    landmarkR::Landmarking(
      data_static = static,
      data_dynamic = dynamic,
      event_indicator = "with.status",
      dynamic_covariates = c("dose"),
      ids = "id",
      event_time = "missing.column",
      times = "time",
      measurements = "value",
      dynamic_covariate_names = "type"
    ),
    "@event_time must be a column in dataframe @data_static"
  )

  # Test: times column missing from data_dynamic
  expect_error(
    landmarkR::Landmarking(
      data_static = static,
      data_dynamic = dynamic,
      event_indicator = "with.status",
      dynamic_covariates = c("dose"),
      ids = "id",
      event_time = "with.time",
      times = "missing.column",
      measurements = "value",
      dynamic_covariate_names = "type"
    ),
    "@times must be a column in dataframe @data_dynamic"
  )

  # Test: measurements column missing from data_dynamic
  expect_error(
    landmarkR::Landmarking(
      data_static = static,
      data_dynamic = dynamic,
      event_indicator = "with.status",
      dynamic_covariates = c("dose"),
      ids = "id",
      event_time = "with.time",
      times = "time",
      measurements = "missing.column",
      dynamic_covariate_names = "type"
    ),
    "@measurements must be a column in dataframe @data_dynamic"
  )

  # Test: dynamic_covariate_names column missing from data_dynamic
  expect_error(
    landmarkR::Landmarking(
      data_static = static,
      data_dynamic = dynamic,
      event_indicator = "with.status",
      dynamic_covariates = c("dose"),
      ids = "id",
      event_time = "with.time",
      times = "time",
      measurements = "value",
      dynamic_covariate_names = "missing.column"
    ),
    "@dynamic_covariate_names must be a column in dataframe @data_dynamic"
  )

  # Test: dynamic_covariate_names is not a character column
  expect_error(
    landmarkR::Landmarking(
      data_static = static,
      data_dynamic = dynamic,
      event_indicator = "with.status",
      dynamic_covariates = c("dose"),
      ids = "id",
      event_time = "with.time",
      times = "time",
      measurements = "dose",
      dynamic_covariate_names = data.frame("type")
    )
  )
})
