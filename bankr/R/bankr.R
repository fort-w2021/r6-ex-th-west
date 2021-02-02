#' R6 Class for a bank account
#'
#' @description
#' A bank account with...
account <- R6::R6Class("Account",
  public = list(
    #' @field balance the account balance
    balance = 0,
    #' @description
    #' public method to deposit money
    #' @param value the amount of money to add to the account
    deposit = function(value) {
      checkmate::assert_number(value, lower = 0, upper = Inf)
      self$balance <- self$balance + value
      cat("current balance: ", self$balance)
    },
    #' @description
    #' public method to withdraw money
    #' @param value the amount of money to withdraw from the account
    withdraw = function(value) {
      checkmate::assert_number(value, lower = 0)
      if (value > self$balance) {
        stop("You cannot withdraw more than ", self$balance, ".")
      }
      self$balance <- self$balance - value
      cat("current balance: ", self$balance)
    }
  )
)

giro_account <- R6::R6Class("GiroAccount",
  inherit = account,
  public = list(
    withdraw = function(value) {

    }
  )
  private = list(
    #' @field overdraft limit
    overdraft_limit = -50
    #' @field overdraft fee
    overdraft_fee = - 5
  )
)
