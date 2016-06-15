

filename = "CoP21"
print(getwd())
#filename = file.choose(new = FALSE)
p = CSVParser$new(paste0("inst/", filename, ".csv"))

model = p$parse()

# for (iteration in 1:10)
# {
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

      model$demandSupplyIssue[best$supply_i, best$i] = "SUPPLY";
      model$demandSupplyIssue[best$supply_i, best$j] = "DEMAND";
      model$demandSupplyIssue[best$supply_j, best$i] = "DEMAND";
      model$demandSupplyIssue[best$supply_j, best$j] = "SUPPLY";

      model$xMatrix[best$supply_i, best$i] = best$yiq
      model$xMatrix[best$supply_j, best$j] = best$yjp

      for (ex in nBest)
      {
        if ((ex$i == best$i &&
             ex$supply_i == best$supply_i) ||
            (ex$j == best$i &&
             ex$supply_j == best$supply_i))
        {
          ex$recalculate()
        }

        if ((ex$j == best$j &&
             ex$supply_j == best$supply_j) ||
            (ex$i == best$j &&
             ex$supply_i == best$supply_j))
        {
          ex$recalculate()
        }
      }

      exchanges = nBest
    }
  }
}

resultMatrix = matrix(data = unlist(realized),
                      byrow = TRUE,
                      ncol = 15)

write.table(
  resultMatrix,
  file = paste0("inst/output/", filename, ".output.csv"),
  row.names = FALSE,
  na = "",
  col.names = FALSE,
  sep = ";"
)
# }
print("Finished...")
