#'
#' A set of two actors and two issues which can exchange
#' @export
#' @author Jelmer Draaijer
#'

ExchangePair = R6Class(
  "ExchangePair",
  public = list(
    #actors
    i = NA,
    j = NA,

    #demand issue of i
    p = NA,
    #demand issue of j
    q = NA,

    #starting postions
    xip = NA,
    xiq = NA,
    xjp = NA,
    xjq = NA,

    #suppy issues
    supply_i = NA,
    supply_j = NA,

    #exchange rates
    dp = NA,
    dq = NA,

    #moves
    move_j = NA,
    move_i = NA,

    #new positions
    yiq = NA,
    yjp = NA,

    #gains
    eu_i = NA,
    eu_j = NA,
    gain = NA,
    gain_check = FALSE,

    #booleans
    recalc = FALSE,
    isValid = TRUE,
    isSwitched = FALSE,

    #Model reference
    model = NA,

    initialize = function(i, j, p, q, model) {
      self$model = model

      self$p = p
      self$q = q

      self$supply_i = q
      self$supply_j = p

      #test if condition 2 holds
      if ((model$sMatrix[p, i] / model$sMatrix[q, i]) < (model$sMatrix[p, j] / model$sMatrix[q, j])) {
        self$i = j
        self$j = i
        self$isSwitched = TRUE
      }
      else{
        self$i = i
        self$j = j
      }

      self$initPositionFromModel()
    },
    initPositionFromModel = function()
    {
      self$xip = self$model$dMatrix[self$p, self$i]
      self$xiq = self$model$xMatrix[self$q, self$i]
      self$xjp = self$model$xMatrix[self$p, self$j]
      self$xjq = self$model$dMatrix[self$q, self$j]
    },
    calculate = function() {
      private$is_J = TRUE

      self$exchangeRatioP()
      self$exchangeRatioQ()



      #
      #AOSIS	amb2050
      #EU28	adaptfinance

      if (self$i == "AOSIS" && self$j == "EU28" && self$q == "amb2050" && self$p == "adaptfinance")
      {
        #browser()
      }

      #compare the calculated shift with the maximum shift, if it exceeds, calculate with the maximum
      self$move_i = self$getShift_I()
      self$move_j = self$getShift_J()

      #calculate the gains
      self$eu_i = private$EU_I()
      self$eu_j = private$EU_J()

      private$is_J = FALSE

      if (self$move_i > self$getShift_I())
      {
        self$exchangeRatioQ()
        self$exchangeRatioP()

        self$move_i = self$getShift_I()
        self$move_j = self$getShift_J()

        #should never happen
        if (self$move_j > self$getShift_J())
          stop("Unreachable state: both shift exceed the maximum interval")

      }
      else
      {
        private$is_J = TRUE
      }

      # correct the direction
      if (self$xip < self$xjp)
        self$move_j = self$move_j * -1
      if (self$xjq < self$xiq)
        self$move_i = self$move_i * -1

      #calculate the gains
      self$eu_i = private$EU_I()
      self$eu_j = private$EU_J()

      if (all.equal(self$eu_i, self$eu_j) == FALSE)
        stop("Unreachable state: gains are not equal.")
      else
        self$gain = self$eu_i


      self$yjp = self$model$xMatrix[self$supply_j, self$j] + self$move_j
      self$yiq = self$model$xMatrix[self$supply_i, self$i] + self$move_i

      x = self$model$xMatrix[self$p,]
      x[[self$j]] = self$yjp
      nbs_p = self$model$nbs$adjusted(self$model$cMatrix[self$p,],self$model$sMatrix[self$p,],x)

      x = self$model$xMatrix[self$q,]
      x[[self$i]] = self$yiq

      nbs_q = self$model$nbs$adjusted(self$model$cMatrix[self$q,],self$model$sMatrix[self$q,],x)

      shift_nbs_p = abs(self$model$nbs$nbsMatrix[self$p] - nbs_p)
      shift_nbs_q = abs(self$model$nbs$nbsMatrix[self$q] - nbs_q)



      gain_j = as.numeric(shift_nbs_q * self$model$sMatrix[self$q,self$j] - shift_nbs_p * self$model$sMatrix[self$p,self$j])
      gain_i = as.numeric(shift_nbs_p * self$model$sMatrix[self$p,self$i] - shift_nbs_q * self$model$sMatrix[self$q,self$i])
      if (all.equal(gain_i, gain_j) == FALSE)
        stop("Unreachable state: gains are not equal.")
      else
        self$gain_check = TRUE

      bMoveJ = self$model$hMatrix[[paste0(self$p, self$j)]]$isValidMove(self$move_j)
      bMoveI = self$model$hMatrix[[paste0(self$q, self$i)]]$isValidMove(self$move_i)

      if (self$move_i == 0 || self$move_j == 0)
      {
        self$isValid = FALSE
      }

      if (bMoveJ != TRUE || bMoveI != TRUE)
      {
        self$isValid = FALSE
      }
    },
    recalculate = function() {
      self$recalc = TRUE

      if (self$i == "Brazil" &&
          self$q == "legal" && self$j == "EIG" && self$p == "mrv")
      {
        # browser()
      }

      dsi = self$model$demandSupplyIssue

      b1 = dsi[self$supply_i, self$i] == "SUPPLY"
      #b2 = is.na(dsi[self$supply_i, self$i])
      b3 = dsi[self$supply_j, self$j] == "SUPPLY"
      #b4 = is.na(dsi[self$supply_j, self$j])

      # wanneer mogen we door? is hij leeg is, of als hij supply is dus of b1 of b2

      # both are na or both are SUPPLY
      if (1 == 1) #(is.na(b1) || b1 == TRUE) && (is.na(b3) || b3 == TRUE) )
      {
        self$initPositionFromModel()
        self$calculate()
      }
      else
      {
        self$isValid = FALSE
        self$gain = 0
      }

    },
    exchangeRatioP = function() {
      if (private$is_J == FALSE)
      {
        self$dp = private$byExchangeRatio(
          actor = self$i,
          issue = self$supply_i,
          exchange_ratio = self$dq
        )
      }
      else
      {
        self$dp = private$byAbsMove(actor = self$j, issue = self$supply_j)
      }
    },
    exchangeRatioQ = function() {
      if (private$is_J == TRUE) {
        self$dq = private$byExchangeRatio(
          actor = self$j,
          issue = self$supply_j,
          exchange_ratio = self$dp
        )
      }
      else{
        self$dq = private$byAbsMove(actor = self$i, issue = self$supply_i)
      }
    },
    getShift_I = function() {
      #x = self$model$xMatrix

      if (private$is_J == TRUE) {
        return(private$reverseMove(
          actor = self$i,
          issue = self$supply_i,
          self$dq
        ))
      }
      else
        return(abs(self$xiq - self$xjq))

    },
    getShift_J = function() {
      x = self$model$xMatrix

      if (private$is_J == FALSE) {
        return(private$reverseMove(
          actor = self$j,
          issue = self$supply_j,
          self$dp
        ))
      }
      else
        return(abs(self$xip - self$xjp))
    },
    asList = function()
    {
      #x = self$model$xMatrix

      i = self$i
      j = self$j

      p = self$p
      q = self$q

      move_i = self$move_i
      move_j = self$move_j

      yiq = self$yiq
      yjp = self$yjp


      l = list(
        i,
        q,
        j,
        p,
        self$gain,
        self$xiq,
        move_i,
        yiq,
        self$xjq,
        self$xjp,
        move_j,
        yjp,
        self$xip,
        self$recalc,
        self$gain_check
      )

      return(l)
    }

  ),
  private = list(
    # the current state for calculation, actor i or j. Depends on which actor can move complelty to the other his position.
    is_J = NA,
    byAbsMove = function(actor, issue) {
      p = issue
      j = actor
      i = private$oppositeActor(actor)

      #shortcuts
      c = self$model$cMatrix
      s = self$model$sMatrix
      x = self$model$xMatrix

      dp = (abs(self$model$dMatrix[p, i] - x[p, j]) * s[p, j] * c[p, j])  / sum(c[p,] * s[p,], na.rm = TRUE)

      return(dp)
    },
    byExchangeRatio = function(actor, issue, exchange_ratio)
    {
      p = issue
      q = private$oppositeIssue(issue)

      j = actor
      i = private$oppositeActor(actor)

      s = self$model$sMatrix

      dq  = ((s[p, i] + s[p, j]) / (s[q, i] + s[q, j])) * exchange_ratio

      return(dq)
    },
    reverseMove = function(actor, issue, exchange_ratio) {
      i = actor
      q = issue

      #shortcuts
      c = self$model$cMatrix
      s = self$model$sMatrix

      return((exchange_ratio * sum(c[q,] * s[q,], na.rm = TRUE)) /  (c[q, i] * s[q, i]))

    },
    oppositeActor = function(actor) {
      if (actor == self$i)
        return(self$j)
      else
        return(self$i)
    },
    oppositeIssue = function(issue) {
      if (issue == self$p)
        return(self$q)
      else
        return(self$p)
    },
    EU_I = function() {
      s = self$model$sMatrix
      return(abs(s[self$p, self$i] * self$dp - s[self$q, self$i] * self$dq))
    },
    EU_J = function() {
      s = self$model$sMatrix
      return(abs(s[self$q, self$j] * self$dq - s[self$p, self$j] * self$dp))
    }
  )
)