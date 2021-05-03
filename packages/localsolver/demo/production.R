#----------------------------------------------------------------------------
# localsolver
# Copyright (c) 2014, WLOG Solutions
#----------------------------------------------------------------------------

#'
#' @demo
#' This problem consists in organizing production of 4 kinds of items. Each product requires certain production time,
#' certain amout of raw and pre-processed materials, and can be soled at a determined price. There are determined constraints
#' for the overall materials and production times of all the products. The objective is to maximize the revenue of the produced items.
#' 

model <- "function model() {
  x[i in 1..4] <- int(0,50);

  // time constraint
  productionTime <- sum[i in 1..4](time[i] * x[i]);
  constraint productionTime <= 200;
  
  // raw material constraint
  rawMaterials <- sum[i in 1..4](materialsR[i] * x[i]);
  constraint rawMaterials <= 300;

  // pre produced material constraint
  preMaterials <- sum[i in 1..4](materialsP[i] * x[i]);
  constraint  preMaterials <= 500;

  //maximize revenue
  revenue <- sum[i in 1..4](price[i] * x[i]);
  maximize revenue;

}"


data <- list(time=c(5,2,6,3), materialsR=c(10,8,9,7), materialsP=c(10,20,25,22), price=c(5,4,6,3) )

if (!exists("ls.path")) {
  ls.path <- NULL
}
tryCatch({  
  lsp <- ls.problem(model, ls.path)
  lsp <- set.params(lsp, lsTimeLimit=60, lsIterationLimit=250)
  lsp <- add.output.expr(lsp, "x", 4)
  lsp <- add.output.expr(lsp, "revenue")
  lsp <- add.output.expr(lsp, "x", dimensions=4)

  ls.solve(lsp, data)
}, error = function(e) {
  if (sum(grep("has.localsolver", geterrmessage(), fixed=TRUE)) > 0) {
    cat("LocalSolver is required to run the demo.
        Verify you have LocalSolver installed.
        Try storing the path to the LocalSolver executable in ls.path variable.")
  } else { stop(e) }
})
