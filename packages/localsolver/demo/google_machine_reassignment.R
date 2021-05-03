#----------------------------------------------------------------------------
# localsolver
# Copyright (c) 2014, WLOG Solutions
#----------------------------------------------------------------------------

#' 
#' @demo
#' Google machine reassignment example.
#' Requires license for the full version of localsolver!
#' 

model.text.lsp <- lsp.model.example('extdata/google.txt')

data(google_example_1_data)

if (!exists("ls.path")) {
  ls.path <- NULL
}

tryCatch({  
  lsp <- ls.problem(model.text.lsp, ls.path)
  lsp <- set.params(lsp, 
                    lsTimeLimit=c(150, 50, 40, 30, 30), 
                    indexFromZero=TRUE)
  
  lsp <- add.output.expr(lsp, "x", c(google_example_1_data$nbProcesses, google_example_1_data$nbMachines))
  lsp <- add.output.expr(lsp, "totalLoadCost", 1)
  lsp <- add.output.expr(lsp, "obj", 1)
  
  ls.solve(lsp, google_example_1_data)
}, error = function(e) {
  if (sum(grep("has.localsolver", geterrmessage(), fixed=TRUE)) > 0) {
    cat("LocalSolver is required to run the demo.
        Verify you have LocalSolver installed.
        Try storing the path to the LocalSolver executable in ls.path variable.")
  } else { stop(e) }
})

