#########################################################################
# Search Trajectory Networks for Multi-Objective Evolutionary Algorithms
# Gabriela Ochoa and Yuri Lavinas, May 2021
# STN construction
# Input:  Text file trace of runs
# Output: STN graph objects
#########################################################################
rm(list = ls(all = TRUE))
library(igraph)
library(ggplot2)
library(dplyr)

library('RColorBrewer')


output_path = "./plots/"
colour.col <-
  c("#fdbf6f", "#4daf4a", "#6a3d9a", "#fb9a99", "#1f78b4")

metrics = read.csv(paste0("metrics/UF_metrics.csv"))

for (i in 1:10) {
  fn = paste0("UF", i)
  nodes <- metrics %>% filter(instance == fn) %>% select(starts_with('v'))
  
  moead_nodes = data.frame(vector = paste0("V",1:5), y = t(nodes[1,]))
  colnames(moead_nodes) = c('vector', "y")
  
  nsga2_nodes = data.frame(vector = paste0("V",1:5), y = t(nodes[2,]))
  colnames(nsga2_nodes) = c('vector', "y")
  
  #### NODE COUNT
  #### MOEA/D
  ggplot(data = moead_nodes, aes(x = vector, y = y, fill = vector)) +
    geom_bar(stat = "identity") + theme_minimal() + ylim(0, 350) + xlab("Vectors") +
    ylab("Number of Nodes") +
    scale_fill_manual("legend", values = colour.col) + theme(
      legend.position = "none",
      axis.text = element_text(size = 30),
      axis.title =
        element_text(size = 32, face = "bold")
    )
  
  ggsave(
    filename = paste0(output_path, fn, "bar_graph_moead.png"),
    dpi = 300
  )
  
  #### NODE COUNT
  #### NSGA-II
  ggplot(data = nsga2_nodes, aes(x = vector, y = y, fill = vector)) +
    geom_bar(stat = "identity") + theme_minimal() + ylim(0, 350) + xlab("Vectors") +
    ylab("Number of Nodes") +
    scale_fill_manual("legend", values = colour.col) + theme(
      legend.position = "none",
      axis.text = element_text(size = 30),
      axis.title =
        element_text(size = 32, face = "bold")
    )
  
  ggsave(
    filename = paste0(output_path, fn, "bar_graph_nsga2.png"),
    dpi = 300
  )
  
  
}

