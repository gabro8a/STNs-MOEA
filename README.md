# STNs-MOEA

<img src="/plots/stn-logo.png" alt="STN" width="125" /> 

Search Trajectory Networks (STNs) for multi-objective evolutionary algorithms when solving continuous optimisation problems.

Resources for constructing, visualising and calculating metrics of STNs of continuous benchmark functions.

This repository is associated with the revised version of the article:

Lavinas, Y., Aranha, C., Ochoa, G. (2022). Search Trajectories Networks of Multiobjective Evolutionary Algorithms. Applications of Evolutionary Computation. EvoApplications 2022. Lecture Notes in Computer Science, vol 13224. Springer, Cham.

[Here is the  revised version of the article.](./revised_version/EvoStar_2021_STNs.pdf)

------

## Software Dependencies

- **R** libraries: `igraph, dplyr, tidyr`

------

The resources are organised into four folders:

## scripts

Three R scripts are provided:

### create.R 

- Creates  STN models for all the input files contained in the **data** folder
- Models are saved within  `.Rdata` files in the **stns** folder

### plot.R 

- Plots the STN models from the `.RData` files in the **stns** folder
- Files with plots (`.pdf`) are  saved in the **plots** folder
- Each file pages has 3 pages, each a graph visualisation with a different graph layout

### metrics.R 

- Computes a set of metrics associated to the STNs models from the `.RData` files in the **stns** folder
- A single `.csv` file with metrics , which is saved in the **metrics** folder.

------

## data

- Text files with data containing post-processed algorithm trajectories of two multi-objective evolutionary algorithms: moead and nsga-2, when solving the 10 unconstrained  multi-objective benchmark functions UF1 , ..., UF10, of the CEC2009 special session and competition ([Zhang etal.,2009](https://www.researchgate.net/publication/265432807_Multiobjective_optimization_Test_Instances_for_the_CEC_2009_Special_Session_and_Competition)). 
- There is one file for each of the 10 functions for the two algorithms (a total of 20 files). 
- These files were produced using the code in the following repository (add link)
- These data files have the following columns:
  - 

------

## metrics

- A single .`csv` file containing a set of network metrics for each of the STN models, for the two algorithms and the 10 benchmark functions
- This file is the output of running the script named **metrics.R** described above
- A brief description of the metrics is as follows
  - *algorithm*: name of the algorithm 
  - *instance*: name of the instance 
  - *nodes*:   total number of nodes
  - *edges*:   total number of edges
  - *pedges*:  tatio of edges to nodes
  - *pareto*: number of Pareto optimal solutions
  - *end*:  number of nodes at the end of trajectories (excluding the Pareto nodes)
  - *shared*: number of shared nodes among vectors 
  - *pshared*: ratio of shared nodes among vectors    
  - *v1 - v5*: number of nodes per vector
  - *components*: number of connected components
  - maxin:  max incoming degree of nodes
  - *meanin*: mean incoming degree of nodes
  - *maxout*: max outgoing degree of nodes
  - *meanout*: max outgoing degree of nodes

------

## pf

- Text files contains the Pareto front for each of the 10 benchmark instances, UF1 ... UF10. 
- These Pareto fronts were obtained from the  [MOEADr](https://fcampelo.github.io/MOEADr/) R package

------

## plots

- PDF files with graph visualisations of each of the STN models
- There is one file for each of the 10 functions for the two algorithms (a total of 20 files). 
- These files were produced using the script named **plot.R** described above
- Each file has 3 pages, each visualising the same model with a different [force-directed] (https://en.wikipedia.org/wiki/Force-directed_graph_drawing)layout:
  1. Fruchterman-Reingoldlayout
  2. Kamada-Kawai layout
  3. Component-wise layout with  Fruchterman-Reingold for each compponent
