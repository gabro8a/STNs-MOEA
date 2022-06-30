rm(list = ls(all = TRUE))

options(scipen=999)

library(dplyr)
library(feather)

source("~/Documents/estudos/STN_MOP/functions.R")

#parameters
number.repetitions = 3
inpath = 'algorithm_data/'

# problem
problem <-
  paste0("UF", 1:10)
for (prob in problem){
  
  if (prob == 'UF10' || prob == 'UF8' || prob == 'UF9'){
    n.obj = 3
  }
  else{
    n.obj = 2
  }
  
# getting all data

## 1st for moead
moead.data = read.data.continuous(
  algorithm = 'moead',
  number.repetitions = number.repetitions,
  problem = prob
)

## 2nd for nsga2
nsga2.data = read.data.continuous(
  algorithm = 'nsga2',
  number.repetitions = number.repetitions,
  problem = prob
)

# generating the 5 weight vectors for a 2, 3 or more obj problem
# might not work for all number of objectives
W = uniform.decomposition(N = 5, n.obj = n.obj)

# getting the objective values from the representative solutions
obj.values.moead = get.objectives.values(moead.data)
obj.values.nsga2 = get.objectives.values(nsga2.data)

# This is a reference point for the calculation of the
## Weighted Tchebycheff method.
### should be combined w/ data of all algorithms!
minP.moead =
  getminP(obj.values.moead)
minP.nsga2 =
  getminP(obj.values.nsga2)
combined.minP = data.frame(rbind(minP.moead, minP.nsga2))

minP = getminP(combined.minP)

# finding the representative solutions for each of the vectors
moead.data.vectors = generate.vector.data.continuous(moead.data, minP, 3)
nsga2.data.vectors  = generate.vector.data.continuous(nsga2.data, minP, 3)
if (n.obj == 2){
  nsga2.data.vectors <- rename(nsga2.data.vectors, Y.1 = Y.V1, Y.2 =Y.V2)  
}
else{
  nsga2.data.vectors <- rename(nsga2.data.vectors, Y.1 = Y.V1, Y.2 =Y.V2, Y.3 =Y.V3)
}
write.table(
  moead.data.vectors,
  file = paste0(
    # '~/Documents/estudos/STN_MOP/processed_data/moead_',
    '~/Desktop/MOEA/data/uf/moead_',
    prob,
    '.txt'
  ),
  quote = FALSE,
  row.names = FALSE
)

write.table(
  nsga2.data.vectors,
  file = paste0(
    # '~/Documents/estudos/STN_MOP/processed_data/nsga2_',
    '~/Desktop/MOEA/data/uf/nsga2_',
    prob,
    '.txt'
  ),
  quote = FALSE,
  row.names = FALSE
)

print(max(nchar(moead.data.vectors$Solution1)))
print(max(nchar(moead.data.vectors$Solution2)))
print(max(nchar(nsga2.data.vectors$Solution1)))
print(max(nchar(nsga2.data.vectors$Solution2)))

# print(dim(moead.data.vectors))
# print((max(moead.data.vectors$Gen)+1)*3*5)
# print(dim(nsga2.data.vectors))
# print((max(nsga2.data.vectors$Gen)+1)*3*5)

}
