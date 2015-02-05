library(Rsymphony, quietly=TRUE)

getOptimisedWeights <- function(body, target) { 
  result <- rep(target/NROW(body), NROW(body))
  
  #Absolute deviation.
  createConstraintsMatrix <- function(d) cbind(diag(NROW(d)),d)
  constraints <- createConstraintsMatrix(body)
  constraints <- rbind(createConstraintsMatrix(body*-1), constraints)
  constraintTypes <- rep(">=", NROW(constraints))
  rhs <- c(-1*result, 1*result)

  #All weights sum to 1.
  constraints <- rbind(constraints,c(rep(0, NROW(body)), rep(1, NCOL(body))))
  constraintTypes <- c(constraintTypes, "==")
  rhs <- c(rhs, 1)
  
  #Minimum profit.
  constraints <- rbind(constraints,c(rep(0, NROW(body)), as.vector(colSums(body))))
  constraintTypes <- c(constraintTypes, ">=")
  rhs <- c(rhs, target)
  
  obj <- c(rep(1, NROW(body)), rep(0, NCOL(body)))
  r <- Rsymphony::Rsymphony_solve_LP(obj, constraints, constraintTypes, rhs, max = FALSE, time_limit=60)
  if (is.na(r$status) | r$status != 0) return(NA)
  weights <- as.numeric(tail(r$solution, NCOL(body)))
  attr(weights,"status") <- as.numeric(r$status)
  weights
}