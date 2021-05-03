#----------------------------------------------------------------------------
# localsolver
# Copyright (c) 2014, WLOG Solutions
#----------------------------------------------------------------------------

#'
#' @demo
#' This problem consists in planning transport of certain resources. There are 4 factories (each of them
#' with given supply) and 3 shops (with given demand) and the resources have to be transported from factories
#' to shops. Transportation cost between location is also provided. The objective is to organize the transport so that
#' its cost is minimized.
#' 

library(localsolver)

model <- "function model() {
  amount[i in 1..3][j in 1..4] <- float(0,500);

  // demand constraint
  for [i in 1..3]
   constraint sum[j in 1..4](amount[i][j]) == Demand[i];

  // supply constraint
  for [j in 1..4]
   constraint sum[i in 1..3](amount[i][j]) == Supply[j];

  // minimize value
  TransportCost<- sum[i in 1..3][j in 1..4](cost[i][j] * amount[i][j]);
  minimize TransportCost;
}"


data <- list(cost = matrix(c(2L,3L,5L,2L,3L,4L,4L,3L,3L,3L,5L,1L),byrow=T, ncol=4), 
             Demand=c(250L, 400L, 150L),  Supply = c(100L, 200L, 150L, 350L))

if (!exists("ls.path")) {
  ls.path <- NULL
}
tryCatch({  
  lsp <- ls.problem(model)
  lsp <- set.params(lsp, lsTimeLimit=60, lsIterationLimit=250)
  lsp <- add.output.expr(lsp, "TransportCost")
  lsp <- add.output.expr(lsp, "amount",dimensions=c(3,4))

  ls.solve(lsp, data)
}, error = function(e) {
  if (sum(grep("has.localsolver", geterrmessage(), fixed=TRUE)) > 0) {
    cat("LocalSolver is required to run the demo.
        Verify you have LocalSolver installed.
        Try storing the path to the LocalSolver executable in ls.path variable.")
  } else { stop(e) }
})
