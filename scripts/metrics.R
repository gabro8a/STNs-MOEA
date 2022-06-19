#########################################################################
# Search Trajectory Networks for Multi-Objective Evolutionary Algorithms
# Gabriela Ochoa and Yuri Lavinas, June 2022
# STN Metrics - Version 2
# Input:  STN graph objects
# Output: CSV file with STN metrics
#########################################################################

rm(list = ls(all = TRUE))
library(igraph)

#--------------------------------------------------------------------------
# Create dataframe with metrics
# algorithm: Name of the algorithm 
# instance: Name of the instance 
# nodes:   Total number of nodes
# edges:   Total number of edges
# pedges:  Ratio of edges to node s
# pareto: Number of pareto optimal solutions
# end:    Number of nodes at the end of trajectories (excluding the best nodes)
# nshared: Number of shared nodes among vectors 
# shared: ration of shared nodes among vectors    
# v1 - v5: Number of nodes per vector
# components: Number of connected components
# maxin:  max incoming degree of nodes
# meanin: mean incoming degree of nodes
# maxout: max outgoing degree of nodes
# meanout: max outgoing degree of nodes


col_types = c("character", "character", "integer", "integer", "numeric", 
              "integer", "integer", "integer", "numeric",
              "integer", "integer", "integer", "integer", "integer", "integer",
              "integer", "numeric", "integer", "numeric")

col_names = c("algorithm", "instance", "nodes", "edges", "pedges", 
              "pareto", "end", "shared", "pshared",   
              "v1", "v2", "v3", "v4", "v5", "components",
              "maxin", "meanin", "maxout", "meanout")

metrics  <- read.table(text = "", colClasses = col_types, col.names = col_names)


# ---- Process all datasets in the given inpath folder ----------------
instances<- list.files("stns/")  # filenames in folder

i = 1    # index to store in dataframe
for (inst in instances) {
  print(inst)
  fname <- paste0("stns/",inst)
  load(fname, verbose = F)
  iname <- strsplit(gsub('.{6}$', '', inst),"_")[[1]] # Extract components of file name
  metrics[i,"algorithm"] <- iname[1]
  metrics[i,"instance"] <- iname[2]
  metrics[i,"nodes"] <- vcount(STN)
  metrics[i,"edges"] <- ecount(STN)
  metrics[i,"pedges"] <- round(ecount(STN)/vcount(STN), 4)
  metrics[i,"pareto"] <- length( which(V(STN)$Vector == "Pareto"))
  metrics[i,"end"] <- length(which(V(STN)$Type == "End"))
  nshared <- length(which(V(STN)$Vector== "Shared"))  # number of shared  nodes
  metrics[i,"shared"] <- nshared
  metrics[i,"pshared"] <- round(nshared/vcount(STN), 4)
  
  metrics[i,"v1"] <- length(which(V(STN)$Vector == "V1"))
  metrics[i,"v2"] <- length(which(V(STN)$Vector == "V2"))
  metrics[i,"v3"] <- length(which(V(STN)$Vector == "V3"))
  metrics[i,"v4"] <- length(which(V(STN)$Vector == "V4"))
  metrics[i,"v5"] <- length(which(V(STN)$Vector == "V5"))
  
  metrics[i,"components"] <- components(STN)$no
  
  metrics[i,"maxin"] <- max(strength(STN, mode="in"))
  metrics[i,"meanin"] <- round(mean(strength(STN, mode="in")), 4)
  metrics[i,"maxout"] <- max(strength(STN, mode="out"))
  metrics[i,"meanout"] <-round(mean(strength(STN, mode="out")),4)

  i = i+1
}

# Save metrics as .csv file

ofname <- paste0("metrics/","UF_metrics.csv")
write.csv(metrics, file = ofname)