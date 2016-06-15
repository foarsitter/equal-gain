
#' CSV File Parser
#'
#' Reads a .csv document
#'
#' @author Jelmer Draaijer
#' @export
#'
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

      demandSupplyIssue = private$initMatrix(issues, actors)

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
          demandSupplyIssue,
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