library(ggplot2)
library(rgl)
library(igraph)
library(RColorBrewer)
library(gridExtra)
library(patchwork)

source("0pract_fun.R")




# Load data
stations <- read.csv("stations.csv")
edges <- read.csv("edges.csv")


# Optional display of the network
#nodes <- stations[, c("nodeID", "latitude", "longitude")]
#
#edges_coords <- merge(edges, nodes, by.x = "nodeID_from", by.y = "nodeID") %>%
#  rename(lat_from = latitude, lon_from = longitude)
#
#edges_coords <- merge(edges_coords, nodes, by.x = "nodeID_to", by.y = "nodeID") %>%
#  rename(lat_to = latitude, lon_to = longitude)
#
#edges_plot <- data.frame(
#  x = edges_coords$lon_from,
#  y = edges_coords$lat_from,
#  xend = edges_coords$lon_to,
#  yend = edges_coords$lat_to
#)
#
#nodes$is_station <- !is.na(stations$nodeLabel)
#
#ggplot() +
#  geom_segment(data = edges_plot,
#               aes(x = x, y = y, xend = xend, yend = yend),
#               color = "grey60", size = 0.3) +
#
#  geom_point(data = nodes[!nodes$is_station, ],
#             aes(x = longitude, y = latitude),
#             color = "black", size = 0.5) +
#
#  geom_point(data = nodes[nodes$is_station, ],
#             aes(x = longitude, y = latitude),
#             color = "red", size = 1.2) +
#
#  coord_equal() +
#  theme_bw(base_size = 14) +
#  labs(title = "European Railway Network",
#       subtitle = "Red nodes = stations",
#       x = "Longitude", y = "Latitude")
#
# Save the plot to a PNG file
#ggsave("railway_network_FI_plot.png", width = 10, height = 8, dpi = 300)




colnames(edges) <- c("from", "to")
colnames(nodes)[1] <- "id"

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

V(g)$name <- as.character(V(g)$name)



# NOTE: overload -> input: network, alpha_min, alpha_max, alpha_step, modmod
#                          (modmod can be: 'rand' to begin with random failure
#                                          'deg' to begin with a degree based attack
#                                          'betw' to begin with a betweenness centrality based attack)
#                -> output: dataframe with column: 'alpha' (the sequence of alpha over which the study is done)
#                                                  'S' (the value of the size of the LCC for the chosen alphas)



# Single study
study <- overload(
  g = g,
  alpha_min = 0.1,
  alpha_max = 1.0,
  alpha_step = 0.1,
  modmod = "rand"             #possible "rand", "deg", "betw"
)

# Plot S over alpha
ggplot(study, aes(x = alpha, y = S)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Overload under Degree based Attack",
    x = expression(alpha),
    y = "S_lcc"
  ) +
  theme_bw(base_size = 14)

# Save the plot to a PNG file
#ggsave("lcc_study.png", width = 10, height = 8, dpi = 300)


# Study over multiple run (exclusively for 'rand')

study <- multiple(
  g = g,
  alpha_min = 0.1,
  alpha_max = 1.0,
  alpha_step = 0.1,
  iterations = 2
)


ggplot(study, aes(x = alpha, y = S)) +
  geom_ribbon(aes(ymin = S - S_sd, ymax = S + S_sd), fill = "steelblue", alpha = 0.3) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Network Robustness Under Overload",
    x = expression(alpha),
    y = "Mean Size of Largest Connected Component (S)"
  ) +
  theme_bw(base_size = 14)

# Save the plot to a PNG file
#ggsave("lcc_study_rand_multiple.png", width = 10, height = 8, dpi = 300)



























