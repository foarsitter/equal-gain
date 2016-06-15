#'
#' Nash Barganing Solution
#' @export
#' @author Jelmer Draaijer
#'

NBS <- R6Class(
  "NBS",
  public = list(
    initialize = function(c, s, x) {
      self$calculate(c, s, x)
    },
    isLeft = function(actor, issue) {
      return(self$leftMatrix[issue, actor])
    },
    calculate = function(c, s, x) {
      # matrix calculation for the Nash Bargaining Solution
      #
      nbs = apply(c * s * x, 1, sum, na.rm = TRUE) / apply(c * s, 1, sum, na.rm = TRUE)
      self$nbsMatrix = nbs

      # determine is all a position is left of the NBS
      self$leftMatrix = nbs < x
    },
    adjusted = function(c,s,x){

      return(sum(c * s * x, na.rm = TRUE) / sum(c*s,na.rm = TRUE))

    },

    nbsMatrix = NA,
    leftMatrix = NA
  ),
)