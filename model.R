library(R6)
library(hash)

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

ExchangePair = R6Class(
     "ExchangePair",
     public = list(
          i = NA,
          j = NA,
          p = NA,
          isValid = TRUE,
          #demand issue of i
          q = NA,
          #demand issue of j
          
          supply_i = NA,
          supply_j = NA,
          
          dp = NA,
          dq = NA,
          
          move_j = NA,
          move_i = NA,
          
          yiq = NA,
          yjp = NA,
          
          eu_i = NA,
          eu_j = NA,
          gain = NA,
          recalc = FALSE,
          
          model = NA,
          
          original_i = NA,
          original_j = NA,
          
          isSwitched = FALSE,
          
          initialize = function(i, j, p, q, model) {
               self$model = model
               
               self$p = p
               self$q = q
               
               self$supply_i = q
               self$supply_j = p
               
               self$original_i = model$xMatrix[q, i]
               self$original_j = model$xMatrix[p, j]
               
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
          },
          
          calculate = function() {
               private$is_J = TRUE
               
               self$exchangeRatioP()
               self$exchangeRatioQ()
               
               #India	mrv	EU28	legal
               
               if (self$i == "Russia" && self$j == "LDCs_BGD")
               {
                    # browser()
               }
               
               #compare the calculated shift with the maximum shift, if it exceeds, calculate with the maximum
               self$move_i = self$getShift_I()
               self$move_j = self$getShift_J()
               
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
               if (self$model$xMatrix[self$p, self$i] < self$model$xMatrix[self$p, self$j])
                    self$move_j = self$move_j * -1
               if (self$model$xMatrix[self$q, self$j] < self$model$xMatrix[self$q, self$i])
                    self$move_i = self$move_i * -1
               
               #calculate the gains
               self$eu_i = private$EU_I()
               self$eu_j = private$EU_J()
               
               if (all.equal(self$eu_i, self$eu_j) == FALSE)
                    stop("Unreachable state: gains are not equal.")
               else
                    self$gain = self$eu_i
               
               self$yiq = self$model$xMatrix[self$supply_i, self$i] + self$move_i
               self$yjp = self$model$xMatrix[self$supply_j, self$j] + self$move_j
               
               bMoveJ = self$model$hMatrix[[paste0(self$p, self$j)]]$isValidMove(self$move_j)
               bMoveI = self$model$hMatrix[[paste0(self$q, self$i)]]$isValidMove(self$move_i)
               
               if (bMoveJ != TRUE || bMoveI != TRUE)
               {
                    self$isValid = FALSE
               }
               
          },
          recalculate = function() {
               self$recalc = TRUE
               
               if (self$i == "EIG" && self$q == "legal" && self$j == "Russia" && self$p == "mrv")
               {
                    #browser()
               }
               
               self$calculate()
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
               x = self$model$xMatrix
               
               if (private$is_J == TRUE) {
                    return(private$reverseMove(
                         actor = self$i,
                         issue = self$supply_i,
                         self$dq
                    ))
               }
               else
                    return(abs(x[self$supply_i, self$i] - x[self$supply_i, self$j]))
               
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
                    return(abs(x[self$supply_j, self$i] - x[self$supply_j, self$j]))
          },
          asList = function()
          {
               x = self$model$xMatrix
               
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
                    x[q, i],
                    move_i,
                    yiq,
                    x[q, j],
                    x[p, j],
                    move_j,
                    yjp,
                    x[p, i],
                    self$recalc
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
               
               dp = (abs(x[p, i] - x[p, j]) * s[p, j] * c[p, j])  / sum(c[p, ] * s[p, ], na.rm = TRUE)
               
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
               
               return((exchange_ratio * sum(c[q, ] * s[q, ], na.rm = TRUE)) /  (c[q, i] * s[q, i]))
               
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

NBS <- R6Class(
     "NBS",
     public = list(
          initialize = function(c, s, x) {
               self$calculate(c, s, x)
          },
          isLeft = function(actor, issue) {
               return(private$leftMatrix[issue, actor])
          },
          calculate = function(c, s, x) {
               # matrix calculation for the Nash Bargaining Solution
               #
               nbs = apply(c * s * x, 1, sum, na.rm = TRUE) / apply(c * s, 1, sum, na.rm = TRUE)
               private$nbsMatrix = nbs
               
               # determine is all a position is left of the NBS
               private$leftMatrix = nbs < x
          }
     ),
     private = list(nbsMatrix = NA,
                    leftMatrix = NA)
)

EqualGainModel <- R6Class(
     "EqualGainModel",
     public = list(
          cMatrix = NA,
          #power
          sMatrix = NA,
          #salience
          xMatrix = NA,
          #position
          xSupplyMatrix = NA,
          #position
          hMatrix = NA,
          #history
          actors = NA,
          #list of all the actors anvailable
          issues = NA,
          #list of all the issues in this dataset
          issuePairs = NA,
          exchangeState = NA,
          #2xn list of all pairs of issues
          initialize = function(actors, issues, c, s, x, h)
          {
               self$actors = actors
               self$issues = issues
               
               # all combinations of the topics
               self$issuePairs = combn(self$issues, 2)
               
               self$cMatrix = c
               self$sMatrix = s
               self$xMatrix = x
               self$xSupplyMatrix = x
               self$hMatrix = h
               
               self$exchangeState = matrix(
                    nrow = length(issues),
                    ncol = length(actors),
                    dimnames = list(issues, actors)
               )
          },
          calculateNBS = function() {
               return(NBS$new(self$cMatrix, self$sMatrix, self$xMatrix))
          },
          calculatePairs = function() {
               pairs = list()
               
               nbs = NBS$new(self$cMatrix, self$sMatrix, self$xMatrix)
               
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
                         issue1 = nbs$isLeft(actor, p)
                         issue2 = nbs$isLeft(actor, q)
                         
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

CSVParser <- R6Class(
     "CSVParser",
     
     public = list(
          filename = NA,
          COLUMN_ACTOR = 2,
          COLUMN_TOPIC = 3,
          
          COLUMN_POSITION = 4,
          COLUMN_SALIENCE = 5,
          COLUMN_POWER = 6,
          
          initialize = function(filename) {
               self$filename = filename
          },
          parse = function() {
               actors = vector(mode = "character")
               issues = vector(mode = "character")
               
               input = read.csv(
                    file = self$filename,
                    sep = ";",
                    dec = ".",
                    header = FALSE,
                    stringsAsFactors = FALSE
               )
               
               for (i in 1:nrow(input))
               {
                    rowType = input[i, 1]
                    
                    if (rowType == "#P")
                    {
                         issues = c(issues, input[i, 2])
                    }
                    
                    if (rowType == "#A")
                    {
                         #add an actor to the list. I don't know a better solution for list appending.
                         actors = c(actors, input[i, 2])
                    }
               }
               
               input = read.csv(
                    file = self$filename,
                    sep = ";",
                    dec = ".",
                    header = FALSE,
                    stringsAsFactors = FALSE
               )
               
               xPostionMatrix = private$initMatrix(issues, actors)
               salienceMatrix = private$initMatrix(issues, actors)
               cPowerMatrix = private$initMatrix(issues, actors)
               historyMatrix = hash()
               # parse all the values for each actor on each issue.
               for (i in 1:nrow(input))
               {
                    rowType = input[i, 1]
                    
                    if (rowType == "#D")
                    {
                         issue = input[i, self$COLUMN_TOPIC]
                         actor = input[i, self$COLUMN_ACTOR]
                         
                         xPostionMatrix[issue, actor] = as.integer(input[i, self$COLUMN_POSITION])
                         salienceMatrix[issue, actor] = as.double(gsub(",", ".", (input[i, self$COLUMN_SALIENCE])))
                         cPowerMatrix[issue, actor] = as.double(gsub(",", ".", (input[i, self$COLUMN_POWER])))
                         
                         .set(
                              historyMatrix,
                              paste0(issue, actor),
                              ExchangeHistory$new(issue, actor)
                         )
                    }
               }
               
               return(
                    EqualGainModel$new(
                         actors,
                         issues,
                         cPowerMatrix,
                         salienceMatrix,
                         xPostionMatrix,
                         historyMatrix
                    )
               )
          }
          
     ),
     private = list(
          initMatrix = function(issues, actors)
          {
               return(matrix(
                    nrow = length(issues),
                    ncol = length(actors),
                    dimnames = list(issues, actors)
               ))
          }
     )
)


filename = "data_short"

p = CSVParser$new(paste0("data/", filename, ".csv"))

model = p$parse()

exchanges = model$calculatePairs()

realized = list()

while (length(exchanges) > 0)
{
     best = exchanges[[1]]
     exchanges = exchanges[-1]
     
     if (best$isValid)
     {
          
          nBest = list()
          
          for (exchange in exchanges)
          {
               if (exchange$isValid)
               {
                    if (exchange$gain > best$gain)
                    {
                         #if the gain is higher the replace the best and add the previous best to the list
                         nBest = c(nBest, best)
                         best = exchange
                    }
                    else
                    {
                         if (exchange$gain != 0)
                         {
                              nBest = c(nBest, exchange)
                         }
                    }
               }
          }
          
          if (best$isValid)
          {
               realized = c(realized, best$asList())
               
               model$xMatrix[best$supply_i, best$i] = best$yiq
               model$xMatrix[best$supply_j, best$j] = best$yjp
               
               for (ex in nBest)
               {
                    if ((ex$i == best$i &&
                         ex$supply_i == best$supply_i) ||
                        (ex$j == best$i && ex$supply_j == best$supply_i))
                    {
                         ex$recalculate()
                    }
                    
                    if ((ex$j == best$j &&
                         ex$supply_j == best$supply_j) ||
                        (ex$i == best$j && ex$supply_i == best$supply_j))
                    {
                         ex$recalculate()
                    }
               }
               
               exchanges = nBest
          }
     }
}

# resultList = list()
# 
# for (exchange in realized)
# {
#      resultList = c(resultList, exchange$asList())
# }

resultMatrix = matrix(data = unlist(realized),
                      byrow = TRUE,
                      ncol = 14)

write.table(
     resultMatrix,
     file = paste0("data/output/", filename, ".output.csv"),
     row.names = FALSE,
     na = "",
     col.names = FALSE,
     sep = ";"
)

print("Finished...")
