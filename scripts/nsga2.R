variation_de <- function(X, P, phi = 0.5, ...) {
  phi <- 0.5
  new.solution <- X
  dimX <- dim(X)[1]
  for (i in 1:dim(X)[1]) {
    idx <- sample.int(dimX, 3,
                      replace = TRUE)#,
    # prob    = P[, i])
    new.solution[i, ] <-
      X[idx[1], ] + phi * (X[idx[2], ] - X[idx[3], ])
  }
  return (new.solution)
}

function (inputData) 
{
  popSize = nrow(inputData)
  idxDominators = vector("list", popSize)
  idxDominatees = vector("list", popSize)
  for (i in 1:(popSize - 1)) {
    for (j in i:popSize) {
      if (i != j) {
        xi = inputData[i, ]
        xj = inputData[j, ]
        if (all(xi <= xj) && any(xi < xj)) {
          idxDominators[[j]] = c(idxDominators[[j]], 
                                 i)
          idxDominatees[[i]] = c(idxDominatees[[i]], 
                                 j)
        }
        else if (all(xj <= xi) && any(xj < xi)) {
          idxDominators[[i]] = c(idxDominators[[i]], 
                                 j)
          idxDominatees[[j]] = c(idxDominatees[[j]], 
                                 i)
        }
      }
    }
  }
  noDominators <- lapply(idxDominators, length)
  rnkList <- list()
  rnkList <- c(rnkList, list(which(noDominators == 0)))
  solAssigned <- c()
  solAssigned <- c(solAssigned, length(which(noDominators == 
                                               0)))
  while (sum(solAssigned) < popSize) {
    Q <- c()
    noSolInCurrFrnt <- solAssigned[length(solAssigned)]
    for (i in 1:noSolInCurrFrnt) {
      solIdx <- rnkList[[length(rnkList)]][i]
      hisDominatees <- idxDominatees[[solIdx]]
      for (i in hisDominatees) {
        noDominators[[i]] <- noDominators[[i]] - 1
        if (noDominators[[i]] == 0) {
          Q <- c(Q, i)
        }
      }
    }
    rnkList <- c(rnkList, list(sort(Q)))
    solAssigned <- c(solAssigned, length(Q))
  }
  return(rnkList)
}


function (inputData) 
{
  popSize = nrow(inputData)
  idxDominators = vector("list", popSize)
  idxDominatees = vector("list", popSize)
  for (i in 1:(popSize - 1)) {
    for (j in i:popSize) {
      if (i != j) {
        xi = inputData[i, ]
        xj = inputData[j, ]
        if (all(xi <= xj) && any(xi < xj)) {
          idxDominators[[j]] = c(idxDominators[[j]], 
                                 i)
          idxDominatees[[i]] = c(idxDominatees[[i]], 
                                 j)
        }
        else if (all(xj <= xi) && any(xj < xi)) {
          idxDominators[[i]] = c(idxDominators[[i]], 
                                 j)
          idxDominatees[[j]] = c(idxDominatees[[j]], 
                                 i)
        }
      }
    }
  }
  noDominators <- lapply(idxDominators, length)
  rnkList <- list()
  rnkList <- c(rnkList, list(which(noDominators == 0)))
  solAssigned <- c()
  solAssigned <- c(solAssigned, length(which(noDominators == 
                                               0)))
  while (sum(solAssigned) < popSize) {
    Q <- c()
    noSolInCurrFrnt <- solAssigned[length(solAssigned)]
    for (i in 1:noSolInCurrFrnt) {
      solIdx <- rnkList[[length(rnkList)]][i]
      hisDominatees <- idxDominatees[[solIdx]]
      for (i in hisDominatees) {
        noDominators[[i]] <- noDominators[[i]] - 1
        if (noDominators[[i]] == 0) {
          Q <- c(Q, i)
        }
      }
    }
    rnkList <- c(rnkList, list(sort(Q)))
    solAssigned <- c(solAssigned, length(Q))
  }
  return(rnkList)
}

nsga2 <-
  function(problem,
           varNo,
           objDim,
           X,
           lowerBounds = rep(-Inf, varNo),
           upperBounds = rep(Inf, varNo),
           popSize = 100,
           tourSize = 2,
           maxevals = 10000,
           cprob = 0.7,
           XoverDistIdx = 5,
           mprob = 0.2,
           MuDistIdx = 10,
           saving.dir = NULL,
           ...) {
    
    # evaluating the population
    Y <- evaluate_population(X       = X,
                             problem = problem,
                             nfe     = 0)$Y
    
    nfe <- popSize
    
    parent <- cbind(X, Y)
    
    # ranking the initial population
    ranking <-
      fastNonDominatedSorting(parent[, (varNo + 1):(varNo + objDim)])
    
    # Rank index for each chromosome
    rnkIndex <- integer(popSize)
    
    i <- 1
    
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      
      i <- i + 1
      
    }
    parent <- cbind(parent, rnkIndex)
    
    objRange <-
      apply(parent[, (varNo + 1):(varNo + objDim)], 2, max) - apply(parent[, (varNo +
                                                                                1):(varNo + objDim)], 2, min)
    
    cd <- crowdingDist4frnt(parent, ranking, objRange)
    
    parent <- cbind(parent, apply(cd, 1, sum))
    
    iter <- 0
    # saving data for analysis
    if(!is.null(saving.dir)){
      write.table(data.frame(X = X, Y =Y, iter = iter, nfe = nfe), paste0(saving.dir, "/all_solutions.csv"), append = F, col.names = T,row.names = F, sep =",")
    }
    while (nfe < maxevals) {
      
      
      # tournament selection
      matingPool <- tournamentSelection(parent, popSize, tourSize)
      
      # crossover operator
      childAfterX <-
        boundedSBXover(matingPool[, 1:varNo], lowerBounds, upperBounds, cprob, XoverDistIdx)
      # Only design parameters are input as the first argument
      
      # mutation operator
      childAfterM <-
        boundedPolyMutation(childAfterX, lowerBounds, upperBounds, mprob, MuDistIdx)
      childAfterM <- t(childAfterM)
      childAfterM <- t(matrix(pmax(0, pmin(childAfterM, 1)),
                            nrow  = nrow(childAfterM),
                            byrow = FALSE))
      
      # evaluate the objective functions of childAfterM
      Y <- evaluate_population(X       = childAfterM,
                               problem = problem,
                               nfe     = 0)$Y
      
      childAfterM <- cbind(childAfterM, Y)
      
      # Consider use child again and again ...
      # "Rt = Pt + Qt"
      # Combine the parent with the childAfterM (No need to retain the rnkIndex and cd of parent)
      parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
      # ranking again
      ranking <-
        fastNonDominatedSorting(parentNext[, (varNo + 1):(varNo + objDim)])
      
      i <- 1
      
      while (i <= length(ranking)) {
        rnkIndex[ranking[[i]]] <- i
        
        i <- i + 1
        
      }
      parentNext <- cbind(parentNext, rnkIndex)
      
      # crowded comparison again
      objRange <-
        apply(parentNext[, (varNo + 1):(varNo + objDim)], 2, max) - apply(parentNext[, (varNo +
                                                                                          1):(varNo + objDim)], 2, min)
      
      cd <- crowdingDist4frnt(parentNext, ranking, objRange)
      
      parentNext <- cbind(parentNext, apply(cd, 1, sum))
      
      parentNext.sort <-
        parentNext[order(parentNext[, varNo + objDim + 1], -parentNext[, varNo +
                                                                         objDim + 2]), ]
      
      
      # choose the first 'popSize' rows for next generation
      parent <- parentNext.sort[1:popSize, ]
      iter <- iter + 1
      nfe <- nfe + popSize
      if(!is.null(saving.dir)){
        x.save = parent[, 1:varNo]
        Y.save = parent[, (varNo + 1):(varNo + objDim)]
        nd = is_nondominated(Y.save)
        x.save = matrix(x.save[nd,], ncol = ncol(x.save))
        Y.save = matrix(Y.save[nd,], ncol = ncol(Y.save))
        write.table(data.frame(X = x.save, Y = Y.save, iter = iter, nfe = nfe), paste0(saving.dir, "/all_solutions.csv"), append = T, col.names = F, row.names = F, sep =",")
      }
    }
    
    # report on nsga2 settings and results
    result = list(
      iter = iter,
      nfe = nfe,
      parameters = parent[, 1:varNo],
      objectives = parent[, (varNo + 1):(varNo + objDim)]
    )
    
    class(result) = "nsga2R"
    
    return(result)
  }