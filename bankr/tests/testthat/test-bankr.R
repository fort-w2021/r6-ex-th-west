library(testthat)

context("bankr")


test_that("basic implementation of class account is correct", {
  expect_error(account$new()$withdraw(150))
  expect_error(account$new()$withdraw(-200))
  expect_error(account$new()$deposit(-300))
  expect_identical(
    {
      test_account <- account$new()
      test_account$deposit(1500)
      test_account$deposit(1500)
      test_account$balance
    },
    3000
  )
  expect_identical(
    {
      test_account <- account$new()
      test_account$deposit(2500)
      test_account$withdraw(1500)
      test_account$balance
    },
    1000
  )
})

test_that("implementation of subclass giroaccount is correct", {
  expect_error(giro_account$new$withdraw(-520))
  expect_error(giro_account$new$withdraw(520))
  expect_error(giro_account$new$withdraw(496))
  expect_identical(
    {
      test_giro <- giro_account$new()
      test_giro$deposit(1500)
      test_giro$withdraw(1200)
      test_giro$balance
    },
    300
  )
  expect_identical(
    {
      test_giro <- giro_account$new()
      test_giro$deposit(1500)
      test_giro$withdraw(1600)
      test_giro$balance
    },
    -105
  )
})
