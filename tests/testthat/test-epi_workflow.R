library(dplyr)
library(parsnip)
library(tidymodels)

data <- filter(case_death_rate_subset, time_value > "2021-08-01")

r <- epi_recipe(data) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
  step_naomit(all_predictors()) %>%
  step_naomit(all_outcomes(), skip = TRUE)

# Something is going wrong below!!!
my_fit <- epi_workflow(r, linear_reg()) %>%
  fit(data)


test_that("epi_workflow is indeed a workflow",{
  expect_true(inherits(wf,"workflow"))
})

test_that("is_epi_workflow properly checks if a workflow is an epi_workflow", {
  expect_true(is_epi_workflow(wf))
  expect_false(is_epi_workflow(workflows::workflow(r)))
})

test_that("predict.epi_workflow won't work on an unfitted workflow", {
  expect_error(predict.epi_workflow(wf))
})

test_that("grab_forged_keys works properly", {
  # include expect_warn
})

test_that("augment.epi_workflow works properly", {
  # include expect_error
})

test_that("new_epi_workflow works properly", {

})
