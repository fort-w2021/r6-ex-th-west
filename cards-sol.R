library(R6)
#' R6 Class representing a bavarian Cardgame

kartenspiel <- R6Class("Kartenspiel")

#' @field farbe a vector of colors contained in the deck
kartenspiel$set("private", "farbe", c("G", "H", "E", "S"))

#' @field wert a vector containing the values of the cards in the deck
kartenspiel$set("private", "wert", c(6:10, "U", "O", "K", "A"))

#' @field ..cards the whole card deck
kartenspiel$set("private", "..cards", 
                paste0(rep(c("G", "H", "E", "S"), each = 9),
                       rep(c(6:10, "U", "O", "K", "A"), times = 4)))

#' @field draw the cards that were drawn
kartenspiel$set("public", "draw", NA)

#' @description 
#' Public method to test which cards in the deck at a given moment.
#' @return Returns a character vector of the cards in the deck.
kartenspiel$set("public", "get_cards", function() private$..cards, overwrite = TRUE)

#' @description 
#' Private method tu shuffle the card deck
kartenspiel$set("private", "shuffle_cards", function() (private$..cards),
                overwrite = TRUE)

#' @description 
#' Public method to draw cards that assigned to public field `draw` and to
#' update the state of the card deck
#' @param number single numeric value indicating how many cards are drawn. Must lie
#'   between 1 and the maximum size of the deck.
#' @return Returns a character vector of the drawn cards.
kartenspiel$set("public", "draw_cards", function(number = 5) {
  checkmate::assert_number(number, lower = 1, upper = length(private$..cards))
  self$draw <- sample(private$..cards, size = number)
  private$..cards <- setdiff(private$..cards, self$draw)
  self$draw
}, overwrite = TRUE)

#' @description 
#' Public method to merge the drawn cards with the remaining card deck and
#' shuffling the deck afterwards
kartenspiel$set("public", "fill_stack", function() {
  private$..cards <- c(private$..cards, self$draw)
  self$draw <- NA
  private$..cards <- sample(private$..cards)
}, overwrite = TRUE)

#' @description 
#' Public method to lift of the deck at a specific position and to put the lower
#' part of the deck on top of the part of the deck that was lifted off
#' @param number single numeric value indicating how man cards are going to be
#'   lifted off. Must lie between 1 and the maximum size of the deck.
kartenspiel$set("public", "lift_off", function(number = 10) {
  checkmate::assert_number(number, lower = 1, upper = length(private$..cards))
  private$..cards <- c(private$..cards[-c(1:number)],
                       private$..cards[1:number])
  
}, overwrite = TRUE)
