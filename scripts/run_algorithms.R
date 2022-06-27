rm(list = ls(all = TRUE))

library(MOEADr)
library(MOEADps)
library(smoof)
library(eaf)
library(feather)


source('nsga2.R')
source('Utils_stn.R')

source('moead.R')

maxeval <- 300
popSize <- 100
dimensions <- 10
n.obj <- 3

repetition <- 3



fun <- paste0("UF", 1:10)


for (f in fun) {
  for (rep in 1:repetition) {
    print(f)
    print(rep)
    num = as.integer(strsplit(f, "UF")[[1]][2])
    if(num <= 7){
      n.obj = 2
    }
    else{
      n.obj = 3
    }
    
    problem.smoof.UF <-
      makeUFFunction(dimensions = dimensions, id = as.numeric(strsplit(f, split = "[A-Z]")[[1]][3]))
    problem.sr <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = problem.smoof.UF))
    }

    par.set = ParamHelpers::getParamSet(problem.smoof.UF)

    ## Set the input parameters for the moead() routine
    ## This reproduces the Original MOEA/D of Zhang and Li (2007)
    ## (with a few changes in the computational budget, to make it run faster)
    problem   <- list(
      name       = "problem.sr",
      xmin       = as.numeric(ParamHelpers::getLower(par.set)),
      xmax       = as.numeric(ParamHelpers::getUpper(par.set)),
      m          = n.obj
    )

    if (n.obj == 2){
      decomp2    <-
        list(name       = "sld", H = popSize - 1) # <-- H = 99 in the original
      
      W2  <- generate_weights(decomp = decomp2,
                              m      = n.obj)  
      
      X  <- create_population(N       = nrow(W2),
                              problem = problem)
    }
    else{
      decomp2    <-
        list(name       = "sld", H = 21) # <-- H = 99 in the original
      
      W2  <- generate_weights(decomp = decomp2,
                              m      = n.obj)
      
      X  <- create_population(N       = nrow(W2),
                              problem = problem)
      popSize = nrow(W2)
      }
    


    resource.allocation <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = 50
      )

    scaling <- preset_moead("moead.de")$scaling
    scaling$name <- "simple"
    update <- preset_moead("moead.de")$update
    update$UseArchive = TRUE

    

    print("MOEA/D")
    dir.name <- paste0("algorithm_data/tmp_moead_", f, "_", rep, "/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)


    out1 <- MOEADr::moead(
      X = X,
      preset   = preset_moead("moead.de"),
      problem  = problem,
      saving.dir = dir.name,
      decomp = decomp2,
      scaling = scaling,
      update = update,
      showpars = list(show.iters = "dots", showevery = 1000),
      stopcrit = list(list(name    = "maxeval",
                           maxeval = maxeval))
    )

    dir.name <- paste0("algorithm_data/nsga2_", f, "_", rep, "/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)

    print("NSGA-II")
    out2 <-
      nsgaps(
        X = X,
        problem = problem,
        varNo = dimensions,
        objDim = n.obj,
        maxeval = maxeval,
        lowerBounds = as.numeric(ParamHelpers::getLower(par.set)),
        upperBounds = as.numeric(ParamHelpers::getUpper(par.set)),
        popSize = popSize,
        tourSize = 2,
        cprob = 0.9,
        XoverDistIdx = 20,
        mprob = 0.1,
        MuDistIdx = 3,
        saving.dir = dir.name
      )


  }
}


