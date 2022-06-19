#########################################################################
# Search Trajectory Networks for Multi-Objective Evolutionary Algorithms
# Gabriela Ochoa and Yuri Lavinas, June 2022
# STN construction - Version 2
# Input:  Text file with trajectory data of several runs
# Output: STN graph objects
#########################################################################
rm(list = ls(all = TRUE))
library(igraph)
library(dplyr)
library(tidyr)


nGen <-  118  # number of algorithm iterations/generations 
od <- 3     # Objective decimal precision

# -----------------------------------------------------------------------------
# Function to create a STN object from the input data
# The input file contains several runs 
# A RData file is saved contained the STN and the nodes and edges dataframes
# -----------------------------------------------------------------------------
create_stn <- function(instance)  {
  aux <- substr(instance, 1, nchar(instance)-4)  # remove .txt 
  fn <- strsplit(aux, "_")[[1]][2]
  print(aux)
  pfname <- paste0("pf/", fn, "_pf.txt")
  pf <- read.table(pfname, colClasses=c("double", "double"))  # Read the Pareto Front file
  fi <- as.integer(substr(fn, 3, nchar(fn))) # Number of the function
  # Convert the objective vector into a planar string
  # This is to later check membership of STN solutions to the Pareto front
  if (fi >= 8) { # functions 8, 9 and 10 have 3 objectives
    colnames(pf) <- c("f1", "f2", "f3")
    pf_str <- paste(as.integer(round(pf$f1,od)*10^od),
                    as.integer(round(pf$f2, od)*10^od),
                    as.integer(round(pf$f3, od)*10^od), sep = "_")
  } else {
    colnames(pf) <- c("f1", "f2")
    pf_str <- paste(as.integer(round(pf$f1,od)*10^od),
                    as.integer(round(pf$f2, od)*10^od), sep = "_")
  }
  
  pf_size <- nrow(pf)
  
  # Read trajectory data
  if (fi >= 8) { # functions 8, 9 and 10 have 3 objectives
    df <- read.table(paste0(fname,instance), stringsAsFactors = F, header = T,
                     colClasses=c("double", "double", "double", "character", 
                                  "character", "integer", "integer","character"))
    df <- rename(df, f1 = Y.1, f2 =Y.2, f3 = Y.3)  # I prefer the names f1, f2 for the objectives  
  } else{
    df <- read.table(paste0(fname,instance), stringsAsFactors = F, header = T,
                     colClasses=c("double", "double", "character", 
                                  "character", "integer", "integer","character"))
    df <- rename(df, f1 = Y.1, f2 =Y.2)  # I prefer the names f1, f2 for the objectives
  }

  #------------------------------------------------------------------------------
  #  Creation of the nodes dataframe 
  #------------------------------------------------------------------------------
  
  # Get the start and end nodes of trajectories.
  start <- df %>%
    filter (df$Gen == 0)
  
  end <- df %>%
     filter (df$Gen == (nGen - 1))  # last generation as it starts coutning with 0
  
  if (fi >= 8) {
    #  Aggregate rows and count the number of solutions for each vector.
    s <- df %>%
      group_by(f1,f2,f3,Solution1, Vector) %>%
      summarise(n = n())
    
    # Reorder columns son Solution appears first. This requied for constrcting the network
    s <- s %>%
      select(Solution1,f1,f2,f3,Vector, n)
  } else{
    #  Aggregate rows and count the number of solutions for each vector.
    s <- df %>%
      group_by(f1,f2,Solution1, Vector) %>%
      summarise(n = n())
    
    # Reorder columns son Solution appears first. This requied for constrcting the network
    s <- s %>%
      select(Solution1, f1, f2,Vector, n)
  }
  
  # Remove duplicates based on Solution1 column
  # So the first representatinve is kept, so the objective values are those of the first appeared
  # this is equivalent to "randomly" selecting the fitness of representative solutions
  # An alternative can be to select a lower precision for the objective values.
 
 
  # Convert from long to wide, keeping a column for each Vector.
  # Fill in missing values with zero
  nodes <- s %>%
    pivot_wider(names_from = Vector, values_from = n, values_fill = 0)
  
  
  # Add a new columns to data frame: 
  # - Count contains the sum of Values for each vector.
  # - Shared: indicates how many vectors visited and node
  # - Vector: indicates Vector that visited the node: V1 - V5,   Shared, Pareto
  # - Type:  Indicates  locations at the Start, Medium and end of runs 
  # -Obj: Objective vector in string form for comparison against theoretical Pareto front
  
  if (fi >= 8) { # functions 8, 9 and 10 have 3 objectives
    ov_str <- paste(as.integer(round(nodes$f1,od)*10^od),
                    as.integer(round(nodes$f2, od)*10^od),
                    as.integer(round(nodes$f3, od)*10^od), sep = "_")
  } else {
    ov_str <- paste(as.integer(round(nodes$f1,od)*10^od),
                    as.integer(round(nodes$f2,od)*10^od), sep = "_")
  }
  
  nodes <- nodes %>%
    mutate(Count = V1+V2+V3+V4+V5,
           Shared = (V1>0) + (V2>0) + (V3>0) + (V4>0) + (V5>0),
           Vector = "Shared",
           Type = "Medium",
           Obj = "000_000",
    )
  nodes <- rename(nodes,  Solution =  Solution1)
  nodes$Obj <- ov_str 
  # This removes duplicate solutions
  nodes <- nodes[!duplicated(nodes$Solution, nodes$Shared), ]
  
  # Assign Vector of nodes -- There are 6 possible values
  # V1, V2, V3, V4, V5, Shared - Default is Shared
  nodes[nodes$Shared ==1 & nodes$V1 >0, ]$Vector <- "V1"
  nodes[nodes$Shared ==1 & nodes$V2 >0, ]$Vector <- "V2"
  nodes[nodes$Shared ==1 & nodes$V3 >0, ]$Vector <- "V3"
  nodes[nodes$Shared ==1 & nodes$V4 >0, ]$Vector <- "V4"
  nodes[nodes$Shared ==1 & nodes$V5 >0, ]$Vector <- "V5"
  # Check if objective vector is in the Pareto front
  nodes[nodes$Obj%in% pf_str, ]$Vector = "Pareto"
  print("Pareto Nodes:")
  print(which(nodes$Obj%in% pf_str))
  # Assign Type of nodes -- There are 4 possible values
  # Begin, End, Medium, Pareto - Default is Medium
  nodes[nodes$Solution %in% start$Solution1, ]$Type = "Begin"
  nodes[nodes$Solution %in% end$Solution1, ]$Type = "End"
  #
  #------------------------------------------------------------------------------
  #  Creation of the edges dataframe 
  #------------------------------------------------------------------------------
  
  # Discard the last generation for edges creation
  
  df <- df %>%
    filter(Gen < max(df$Gen))
  
  #  Aggregate rows and count the number of edges (sol1 -> sol2) for each vector.
  
  s <- df %>%
    group_by(Solution1, Solution2, Vector) %>%
    summarise(n = n())
  
  s <- s[!duplicated(s$Solution1, s$Solution2), ]  # Remove duplicate edges
  
  # Convert from long to wide, keeping a column for each Vector.
  # Fill in missing values with zero
  edges <- s %>%
    pivot_wider(names_from = Vector, values_from = n, values_fill = 0)
  
  # Add a new columns: Count the sum of values for each vector, 
  # Shared: indicates how many vectors visited and edge
  edges <- edges %>%
    mutate(Count = V1+V2+V3+V4+V5,
           Shared = (V1>0) + (V2>0) + (V3>0) + (V4>0) + (V5>0),
           Vector = "Shared")
  
  # Assign type of Edges -- There are 6 types of nodes
  # V1, V2, V3, V4, V5, Shared
  
  edges[edges$Shared ==1 & edges$V1 >0, ]$Vector <- "V1"
  edges[edges$Shared ==1 & edges$V2 >0, ]$Vector <- "V2"
  edges[edges$Shared ==1 & edges$V3 >0, ]$Vector <- "V3"
  edges[edges$Shared ==1 & edges$V4 >0, ]$Vector <- "V4"
  edges[edges$Shared ==1 & edges$V5 >0, ]$Vector <- "V5"
  
  #------------------------------------------------------------------------------
  #  Creation the STN model from the nodes and edges dataframe
  #------------------------------------------------------------------------------
  STN<- graph_from_data_frame(edges, directed=TRUE, vertices=nodes)
  STN <- igraph::simplify(STN, remove.multiple = F)   # Remove self-loops
  
  # Saving the STN object, but also the nodes and edges
  # As they can be useful to compute  metrics
  fname <- paste0("stns/", aux, ".RData")
  save(pf_size, nodes, edges, STN, file = fname)
  print(table(V(STN)$Type))

  return (nrow(nodes))  # Return  number of nodes, just to check it
}

# ---- Get all files in the given input folder -----------------------------

fname <-  paste0("data/")
data_files <- list.files(fname)  # filenames in folder

# Create STNs for all files in the folder
nnodes <- lapply(data_files, create_stn)
barplot(as.numeric(unlist(nnodes))) # Plot number of nodes as check


