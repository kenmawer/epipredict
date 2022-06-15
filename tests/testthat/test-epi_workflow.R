library(dplyr)
jhu <- jhu_csse_daily_subset %>%
  filter(time_value > "2021-08-01") %>%
  select(geo_value:death_rate_7d_av) %>%
  rename(case_rate = case_rate_7d_av, death_rate = death_rate_7d_av)

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
  step_naomit(all_predictors()) %>%
  step_naomit(all_outcomes(), skip = TRUE)

wf <- epi_workflow(r, linear_reg())

test_that("epi_workflow is indeed a workflow",{
  expect_true(inherits(wf,"workflow"))
})

test_that("is_epi_workflow works properly", {
  expect_true(is_epi_workflow(wf))
  expect_false(is_epi_workflow(workflows::workflow(r)))
})

test_that("predict.epi_workflow works properly", {
  expect_error(predict(wf))
})

test_that("grab_forged_keys works properly", {

})

test_that("augment.epi_workflow works properly", {

})

test_that("new_epi_workflow works properly", {

})
