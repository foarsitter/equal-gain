#' The main body of the model.
#'
#' @author Jelmer Draaijer
#' @export
#'

EqualGainModel <- R6Class(
  "EqualGainModel",
  public = list(
    cMatrix = NA,
    #power
    sMatrix = NA,
    #salience
    xMatrix = NA,
    #history
    dMatrix = NA,
    #history
    hMatrix = NA,
    demandSupplyIssue = NA,
    #position
    actors = NA,
    #list of all the actors anvailable
    issues = NA,
    #list of all the issues in this dataset
    issuePairs = NA,
    exchangeState = NA,
    #2xn list of all pairs of issues
    initialize = function(actors, issues, c, s, x, dsi, h)
    {
      self$actors = actors
      self$issues = issues

      # all combinations of the topics
      self$issuePairs = combn(self$issues, 2)

      self$demandSupplyIssue = dsi

      self$cMatrix = c
      self$sMatrix = s
      self$xMatrix = x
      self$dMatrix = x
      self$hMatrix = h

      self$exchangeState = matrix(
        nrow = length(issues),
        ncol = length(actors),
        dimnames = list(issues, actors)
      )
    },
    calculateNBS = function() {

      self$nbs = NBS$new(self$cMatrix, self$sMatrix, self$xMatrix)
      return(self$nbs)
    },

    nbs = NA,

    calculatePairs = function() {
      pairs = list()

      self$nbs = NBS$new(self$cMatrix, self$sMatrix, self$xMatrix)

      actors = self$actors

      for (i in 1:ncol(self$issuePairs))
      {
        group_a = vector()
        group_b = vector()
        group_c = vector()
        group_d = vector()

        p = self$issuePairs[1, i]
        q = self$issuePairs[2, i]

        for (actor in actors)
        {
          issue1 = self$nbs$isLeft(actor, p)
          issue2 = self$nbs$isLeft(actor, q)

          if (!is.na(issue1) && !is.na(issue2))
          {
            if (issue1 == TRUE && issue2 == TRUE) {
              group_a = c(actor, group_a) #A
            }
            else if (issue1 == TRUE &&
                     issue2 == FALSE) {
              group_b = c(actor, group_b)#B
            }
            else if (issue1 == FALSE &&
                     issue2 == TRUE) {
              group_c = c(actor, group_c)#C
            }
            else if (issue1 == FALSE &&
                     issue2 == FALSE) {
              group_d = c(actor, group_d)#D
            }
          }
        }

        if (length(group_a) > 0 && length(group_d) > 0)
        {
          #foreach actor in a combine with d if p=suplly & q=demand.
          for (a in 1:length(group_a))
          {
            for (d in 1:length(group_d))
            {
              newPair = ExchangePair$new(group_a[a],
                                         group_d[d],
                                         p,
                                         q,
                                         self)

              newPair$calculate()

              pairs = c(pairs, newPair)
            }
          }
        }

        if (length(group_b) > 0 && length(group_c) > 0)
        {
          #foreach actor in a combine with d if p=suplly & q=demand.
          for (b in 1:length(group_b))
          {
            for (c in 1:length(group_c))
            {
              newPair = ExchangePair$new(group_b[b],
                                         group_c[c],
                                         p,
                                         q,
                                         self)

              newPair$calculate()

              pairs = c(pairs, newPair)
            }
          }
        }
      }

      return(pairs)
    },
    setState = function(exchange)
    {
      self$exchangeState[exchange$p, exchange$i] = "demand"
      self$exchangeState[exchange$p, exchange$j] = "supply"
      self$exchangeState[exchange$q, exchange$i] = "supply"
      self$exchangeState[exchange$q, exchange$j] = "demand"
    },
    checkState = function(exchange) {
      b1 = self$exchangeState[exchange$p, exchange$i] == "demand"
      b2 = self$exchangeState[exchange$p, exchange$j] == "supply"
      b3 = self$exchangeState[exchange$q, exchange$i] == "supply"
      b4 = self$exchangeState[exchange$q, exchange$j] == "demand"

      return((is.na(b1) ||
                b1) &&
               (is.na(b2) ||
                  b2) &&
               (is.na(b3) || b3) && (is.na(b4) || b4))
    }

  )
)