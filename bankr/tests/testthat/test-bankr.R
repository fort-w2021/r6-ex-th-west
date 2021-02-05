library(testthat)

context("bankr")


test_that("basic implementation of class account is correct", {
  expect_error(account$new()$withdraw(150))
  expect_error(account$new()$withdraw(-200))
  expect_error(account$new()$deposit(-300))
  expect_error(account$new()$deposit("100"))
  expect_error(account$new()$withdraw(FALSE))
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
  expect_error(giro_account$new()$withdraw(-520))
  expect_error(giro_account$new()$withdraw(520))
  expect_error(giro_account$new()$withdraw(496))
  expect_error(giro_account$new()$withdraw("100"))
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


test_that("implementation of Class SafeAccount is correct", {
  expect_null({
    test_safe <- safe_account$new()
    test_safe$balance
  })
  expect_error({
    test_safe <- safe_account$new()
    test_safe$balance <- 100
  })
  expect_error({
    test_safe <- safe_account$new()
    test_safe$deposit <- 100
    test_Safe$withdraw <- 200
  })
  expect_identical(
    {
      test_safe <- safe_account$new()
      test_safe$deposit <- 250
      test_safe$withdraw <- 100
      test_safe$get_balance
    },
    150
  )
  expect_error(safe_account$new()$deposit <- "100")
  expect_error(safe_account$new()$withdraw <- FALSE)
})
