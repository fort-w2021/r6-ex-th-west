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
      if (value > self$balance && class(self)[[1]] == "Account") {
        stop("You cannot withdraw more than ", self$balance, ".")
      }
      self$balance <- self$balance - value
      cat("current balance: ", self$balance)
    }
  )
)

#' R6 Subclass for a giro account
#'
#' @description
#' A special form of bank account that has an overdraft limit and fee
giro_account <- R6::R6Class("GiroAccount",
  inherit = account,
  public = list(
    #' @description
    #' method to withdraw money from giro account
    #' @param value the amount of money to withdraw from account
    withdraw = function(value) {
      if (self$balance - value - private$overdraft_fee < private$overdraft_limit) {
        stop("Balance needs to be at least ", private$overdraft_limit, ".")
      }
      if (self$balance - value < 0) {
        super$withdraw(value + private$overdraft_fee)
      } else {
        super$withdraw(value)
      }
    }
  ),
  private = list(
    # @field overdraft_limit the overdraft limit of given account
    overdraft_limit = -500,
    # @field overdraft_fee overdraft fee that gets deducted if balance is negative
    overdraft_fee = 5
  )
)

#' R6 Class for a safe bank account
#'
#' @description
#' A bank account with a private field for balance instead of a public field

safe_account <- R6::R6Class("SafeAccount",
  private = list(
    balance = 0
  ),
  active = list(
    deposit = function(value) {
      if (missing(value)) {
        cat("current balance is :", self$get_balance)
      } else {
        checkmate::assert_number(value, lower = 1, upper = Inf)
        private$balance <- private$balance + value
        cat("current balance is :", self$get_balance)
      }
    },
    get_balance = function() {
      private$balance
    },
    withdraw = function(value) {
      if (missing(value)) {
        cat("current balance is :", self$get_balance)
      } else {
        checkmate::assert_number(value, lower = 0)
        stopifnot("Cannot withdraw more money than bank account yields!" =
                    !(value > self$get_balance))
        private$balance <- private$balance - value
        cat("current balance is :", self$get_balance)
      }
    }
  )
)
