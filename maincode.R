library(ggplot2)
library(rgl)
library(igraph)
library(RColorBrewer)
library(gridExtra)
library(patchwork)

source("0functions.R")

#Definition of parameters

N <- 1000
alpha_a <- 10 / N
r_a <- 1 / N
alpha_m <- 5
r_m <- 1

#Creation of the network (no edges)

g <- sample_gnp(N,p=0,directed=FALSE)

#To display the network
#lay <- layout_with_fr(g)
#par(mfrow = c(1,1), bg = "white")
#plot(g, layout=lay, vertex.color="steelblue", vertex.label=NA, edge.curved=0.2, vertex.size=1, main="starting network")

# Study of the Degree Distribution
#
# NOTE : multiple_run output:
#    if study_lcc FALSE or not specified:
#        deg -> list with: k_mean (a value)
#                          deg_dist (a vector)
#                          deg_sd (a vector)
#        lcc -> a value 0
#
#
#    if study_lcc TRUE:
#        deg -> a value 0
#        lcc -> list with: timestep (a vector)
#                          S (a vector)
#                          S_sd (a vector)

















