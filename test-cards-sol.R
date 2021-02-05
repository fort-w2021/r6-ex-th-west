library(testthat)

context("cards-sol")

test_that("basic implementation is correct", {
  expect_error({
    test_cards <- kartenspiel$new()
    test_cards$draw_cards(-5)
  })
  expect_error({
    test_cards <- kartenspiel$new()
    test_cards$draw_cards(45)
  })
  expect_error({
    test_cards <- kartenspiel$new()
    test_cards$lift_off(-5)
  })
  expect_error({
    test_cards <- kartenspiel$new()
    test_cards$lift_off(45)
  })
  expect_equal(
    {
      test_cards <- kartenspiel$new()
      test_cards$draw_cards(5)
      test_cards$draw %in% test_cards$get_cards()
    },
    rep(FALSE, 5)
  )
  expect_equal(
    {
      test_cards <- kartenspiel$new()
      test_cards$lift_off(10)
      test_cards$get_cards()
    },
    c(paste0(
      rep(c("G", "H", "E", "S"), each = 9),
      rep(c(6:10, "U", "O", "K", "A"), times = 4)
    )[-(1:10)], paste0(
      rep(c("G", "H", "E", "S"), each = 9),
      rep(c(6:10, "U", "O", "K", "A"), times = 4)
    )[(1:10)])
  )
})


