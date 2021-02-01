#' R6 Class for a bank account
#'
#' @description
#' A bank account with...
account <- R6Class("Account",
  public = list(
    #' @field balance the account balance
    balance = numeric(0),
    #' @description
    #' public method to deposit money
    #' @param value the amount of money to add to the account
    deposit = function(value) {
      balance <- balance + value
    },
    #' @description
    #' public method to withdraw money
    #' @param value the amount of money to withdraw from the account
    withdraw = function(value) {
      balance <- balance - value
    }
  )
)
