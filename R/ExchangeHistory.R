#'
#' The history of an actor his exchanges
#'
#'
#' @export
#' @author Jelmer Draaijer
#'

ExchangeHistory = R6Class(
  "ExchangeHistory",

  public = list(
    moves = list(),
    issue = NA,
    actor = NA,
    initialize = function(issue, actor)
    {
      self$issue = issue
      self$actor = actor
    },
    isValidMove = function(move)
    {
      if (length(self$moves) == 0)
      {
        self$moves = c(self$moves, move)
        return(TRUE)
      }

      newMoves = c(self$moves, move) < 0

      valid = isTRUE(all.equal(min(newMoves), max(newMoves)))

      if (valid)
      {
        self$moves = c(self$moves, move)
        return(TRUE)
      }
      else
      {
        #browser()
        return(FALSE)
      }
    }
  )
)