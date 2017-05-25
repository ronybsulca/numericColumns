# NumericColumns Tests

library(testthat)

## Testing check_numeric_object

test_that("checking numeric object", {
  expect_that(check_numeric_object(1), equals(TRUE))
  expect_that(check_numeric_object("-2.7"), equals(TRUE))
  expect_that(check_numeric_object("bacon1"), equals(FALSE))

  expect_that(check_numeric_object(TRUE), equals(FALSE))
  expect_that(check_numeric_object(TRUE, with_bool = TRUE), equals(TRUE))
})

## Testing check_numeric_list

test_that("checking numeric list", {
  expect_that(check_numeric_list(c(1,2,-5,6.7)), equals(TRUE))
  expect_that(check_numeric_list(c("-2.7", 7, 9, -5, "9.8")), equals(TRUE))
  expect_that(check_numeric_list(list("-2.7", 7, 9, -5, "9.8")), equals(TRUE))
  expect_that(check_numeric_list(c("1","3", -5, "bacon")), equals(FALSE))
  expect_that(check_numeric_list(c(3, TRUE, 1)), equals(TRUE)) # Vector coerces TRUE to 1
  expect_that(check_numeric_list(c("3", TRUE, 1)), equals(FALSE)) # Vector coerces TRUE to "TRUE"

  expect_that(check_numeric_list(c(TRUE,TRUE, FALSE)), equals(FALSE))
  expect_that(check_numeric_list(c(TRUE,TRUE, FALSE),with_bool = T), equals(TRUE))
})

## Testing extract_numeric_columns

test_that("extract numeric columns", {
  # All of mtcars columns are numeric
  expect_that(extract_numeric_columns(mtcars), equals(mtcars))

  # Coercing the function to examine certain columns
  temp = mtcars
  temp$names = row.names(temp)

  result = mtcars[, c("mpg", "cyl")]
  result$names = rep(NA, nrow(mtcars))
  row.names(result) = row.names(mtcars)
  result$names = as.numeric(result$names)

  expect_that(extract_numeric_columns(temp, columns = c("mpg", "cyl", "names")), equals(result))
})
