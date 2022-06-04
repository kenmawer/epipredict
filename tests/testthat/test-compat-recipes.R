add_five <- function(x) x + 5
triple <- function(x) {
  3 * x
}

test_that("fun_calls works as intended",{
  expect_equal(fun_calls(add_five),"+")
  expect_equal(fun_calls(triple),c("{","*"))
})

test_that("fun_calls works as intended",{
  expect_error(inline_check(add_five),NA)
  expect_error(inline_check(triple))
})
