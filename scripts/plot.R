#########################################################################
# Search Trajectory Networks for Multi-Objective Evolutionary Algorithms
# Gabriela Ochoa and Yuri Lavinas, June 2022
# STN Visualisation - Version 2
# Input:  STN graph objects
# Output: PDF files with STN images
#########################################################################
rm(list = ls(all = TRUE))
library(igraph)

#################################################################
# Triangle vertex shape: because igraph does not have a native
# triangle shape, a function is provided to have a triangle shape
mytriangle <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1 / 200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(
    x = coords[, 1],
    y = coords[, 2],
    bg = vertex.color,
    col = vertex.color,
    stars = cbind(vertex.size, vertex.size, vertex.size),
    add = TRUE,
    inches = FALSE
  )
}
add_shape("triangle", clip = shapes("circle")$clip, plot = mytriangle)

# Node Colors
best_ncol  <-  "red"    # Pareto OPtima solutions 
end_ncol <-    "gray15"  # End of trajectories for each run.
start_ncol <-  "gold"   # Start of trajectories
shared_col <-  "gray70" #  Visitied by more than one Vector

# Legend 
legend.shape <- c(15, 17, 16, 20, 20, 20, 20, 20, 20) 
vector.txt <- c("Shared","V1", "V2", "V3", "V4","V5")
legend.txt <- c("Start", "End", "PF optimal", vector.txt)
vector.col <- c("#fdbf6f", "#4daf4a", "#6a3d9a", "#fb9a99", "#1f78b4")
legend.col <- c(start_ncol, end_ncol, best_ncol, shared_col, vector.col)

# -----------------------------------------------------------------------------
# Function to plot a STN graph
# -----------------------------------------------------------------------------
plot_stn <- function(instance)  {
  load(paste0("stns/",instance), verbose = T)
  # Decorate nodes
  # Node color by Vector 
  V(STN)[V(STN)$Vector == "Shared"]$color = shared_col
  V(STN)[V(STN)$Vector == "V1"]$color = vector.col[1]  
  V(STN)[V(STN)$Vector == "V2"]$color = vector.col[2]  
  V(STN)[V(STN)$Vector == "V3"]$color = vector.col[3]  
  V(STN)[V(STN)$Vector == "V4"]$color = vector.col[4]  
  V(STN)[V(STN)$Vector == "V5"]$color = vector.col[5]  
  V(STN)[V(STN)$Vector == "Pareto"]$color = best_ncol
  # LNode color by location type
  # Take the IDS of the type of nodes for decoration
  start_nodes <- which(V(STN)$Type == "Begin")
  end_nodes  <-  which(V(STN)$Type == "End")
  best_nodes <-  which(V(STN)$Vector == "Pareto")
  print("Best nodes")
  print(best_nodes)
  V(STN)[start_nodes]$color = start_ncol  # Color of start nodes
  V(STN)[end_nodes]$color = end_ncol      # Color of end of runs nodes
  V(STN)[best_nodes]$color = best_ncol    # Color of  best nodes
  # Shape of nodes
  V(STN)$shape <- "circle"  # circle is the default shape
  V(STN)[start_nodes]$shape = "square"  # Square for start nodes
  V(STN)[end_nodes]$shape = "triangle"  # Triangle for start nodes
  # Frame colors are the same as node colors. White  frame for best nodes to highlight them
  V(STN)$frame.color <- V(STN)$color
  V(STN)[best_nodes]$frame.color <- "white"
  # Size of Nodes Proportional to  incoming degree, 
  V(STN)$size <- strength(STN, mode="in") + 0.5   # nodes with strength 0 have at least size 0.8 
  V(STN)[end_nodes]$size = V(STN)[end_nodes]$size + 2 # Increase a a bit size of end nodes
  V(STN)[start_nodes]$size = V(STN)[start_nodes]$size + 2 # Increase a a bit size of end nodes
  V(STN)[best_nodes]$size = V(STN)[best_nodes]$size + 2.2   # Increease a bit more the size of  best nodes
  
  # Edge color by Vector 
  E(STN)[E(STN)$Vector == "Shared"]$color = shared_col
  E(STN)[E(STN)$Vector == "V1"]$color = vector.col[1]  
  E(STN)[E(STN)$Vector == "V2"]$color = vector.col[2]  
  E(STN)[E(STN)$Vector == "V3"]$color = vector.col[3]  
  E(STN)[E(STN)$Vector == "V4"]$color = vector.col[4]  
  E(STN)[E(STN)$Vector == "V5"]$color = vector.col[5]  
  # width of edges proportional to their Count
  E(STN)$width <- E(STN)$Count
  
  title <- paste('Nodes:',vcount(STN), 'Edges:',ecount(STN), 
                 'Comp:', components(STN)$no)
  frlay <- layout.fruchterman.reingold(STN)
  kklay <- layout.kamada.kawai(STN)
  clay <- layout_components(STN)
  
  print(title)
  ofname <-  gsub('.{6}$', '', instance) # removes  (last 6characters) .RData from file to use as name
  ofname = paste0("plots/",ofname,'.pdf')
  pdf(ofname) 
  print(ofname)
  # Plots with 3 Layouts
  plot(STN, layout = frlay, vertex.label = "", main = paste("FR",title),
       edge.arrow.size = 0.2, edge.curved = 0.3)
  legend("topright", legend.txt, pch = legend.shape, col = legend.col, 
         pt.bg=legend.col, cex = 0.7, pt.cex=1, bty = "n")
  
  plot(STN, layout = kklay, vertex.label = "", main = paste("KK",title),
       edge.arrow.size = 0.2, edge.curved = 0.3)
  legend("topright", legend.txt, pch = legend.shape, col = legend.col, 
         pt.bg=legend.col, cex = 0.7, pt.cex=1, bty = "n")
  
  plot(STN, layout = clay, vertex.label = "", main = paste("C",title),
       edge.arrow.size = 0.2, edge.curved = 0.3)
  legend("topright", legend.txt, pch = legend.shape, col = legend.col, 
         pt.bg=legend.col, cex = 0.7, pt.cex=1, bty = "n")
  dev.off()
  return (vcount(STN))  # Return  number of nodes, just to check it
}

# ---- Get all files in the given input folder -----------------------------

data_files <- list.files("stns/")  # filenames in folder

# Create STNs for all files in the folder
nnodes <- lapply(data_files, plot_stn)
barplot(as.numeric(unlist(nnodes))) # Plot number of nodes as check


