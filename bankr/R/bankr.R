# PROBLEM:
# Mir ist leider nicht klar wie ich das warning von devtools:check() "Missing
# link or links in documentation object 'giro_account.Rd':..." beheben soll.
# Google hat mir dabei leider nicht geholfen (vielleicht hab ich die Section
# Cross-References von 'Writing R Extensions' auch nicht wirklich verstanden).
# Ich habe lediglich https://github.com/r-lib/roxygen2/issues/1155 das hier
# gefunden. Wie es allerdings scheint kann man die Superklasse nicht exportieren
# bei der Subclass geht das schon. Also das scheint auch kein Workaround zu sein,
# Um Tipps wäre ich dankbar. Es wäre ja auch hilfreich unter der Sektion "See also"
# auf Account zu verlinken. Aber auch das ist mir leider nicht gelungen.
#
# PROBLEM2: Bei der Klasse SafeAccount habe ich das Problem, dass get_balance()
# zwar den derzeitigen Kontostand ausgibt. Wenn self$get_balance() aber innerhalb
# von deposit und withdraw aufgerufen wird und man  kein value zuweist, wird
# "current balance is: 0NULL" ausgegeben. Ich komme einfach nicht dahinter, wieso
# hier NULL angehängt wird, da ja nicht direkt über ...$balance zugegriffen wird.


#' R6 Class for a bank account
#'
#' @description
#' A bank account with basic functionality for adding or removing money from its
#' balance.
#' @param value The amount of money to add  to the account or to withdraw from
#'   the account.
#'   @export
account <- R6::R6Class("Account",
  public = list(
    #' @field balance the account balance
    balance = 0,
    #' @description
    #' Method to deposit money.

    #' @return The current balance after performing the transaction.
    deposit = function(value) {
      checkmate::assert_number(value, lower = 0, upper = Inf)
      self$balance <- self$balance + value
      cat("current balance: ", self$balance)
    },
    #' @description
    #' Method to withdraw money.

    #' @return The current balance after performing the transaction.
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
#' A special form of bank account that has an overdraft limit and fee.
#' @param value The amount of money to add  to the account or to withdraw from
#'   the account.
#' @seealso [bankr::Account]
#' @export
giro_account <- R6::R6Class("GiroAccount",
  inherit = account,
  public = list(
    #' @description
    #' method to withdraw money from giro account

    #' @return The current balance after performing the transaction.
    withdraw = function(value) {
      checkmate::assert_number(value, lower = 0)
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

# Welchen Vorteil hat so eine Konstruktion möglicherweise?
#
# Diese Konstruktion hat den Vorteil, dass nicht mehr direkt auf das Feld balance
# zugegriffen werden kann. Damit lassen sich Manipulationen verhindern, da der
# Wert von Balance nur noch mittels den active bindings deposit und withdraw,
# also den legitimen "Methoden" verändert werden kann.


#' R6 Class for a safe bank account
#'
#' @description
#' A bank account with a private field for balance instead of a public field
#' @param value The amount of money to add  to the account or to withdraw from
#'   the account.
safe_account <- R6::R6Class("SafeAccount",
  private = list(

    balance = 0
  ),
  active = list(
    #' @field deposit Deposits money to the safe account and returns the balance
    #'   after the transaction is finished.
    deposit = function(value) {
      if (missing(value)) {
        cat("current balance is :", self$get_balance)
      } else {
        checkmate::assert_number(value, lower = 1, upper = Inf)
        private$balance <- private$balance + value
        cat("current balance is :", self$get_balance)
      }
    },
    #' @field get_balance Returns the current account balance.
    get_balance = function() {
      private$balance
    },
    #' @field withdraw Withdraws money from the safe account and returns the balance
    #'   after the transaction is finished.
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
