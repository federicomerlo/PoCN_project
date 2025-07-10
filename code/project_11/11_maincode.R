library(ggplot2)
library(rgl)
library(igraph)
library(RColorBrewer)
library(gridExtra)
library(patchwork)

source("11_functions.R")

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



# NOTE : multiple_run output:
#    if study_lcc = FALSE or not specified:
#        deg -> list with: k_mean (a value)
#                          deg_dist (a vector)
#                          deg_sd (a vector)
#        lcc -> a value 0
#
#
#    if study_lcc = TRUE:
#        deg -> a value 0
#        lcc -> list with: timestep (a vector)
#                          S (a vector)
#                          S_sd (a vector)


#
#
#
# Study of the Degree Distribution
#
#
#

study_a <- multiple_run(g, artime, realizations = 20, timesteps = 5000, alpha = alpha_a, r = r_a)
study_m <- multiple_run(g, moore, realizations = 20, timesteps = 5000, alpha = alpha_m, r = r_m)

deg_info_a <- study_a$deg

x_a <- 0:25
y_a <- deg_info_a$deg_dist
err_a <- deg_info_a$deg_sd

k_mean_a <- deg_info_a$k_mean

deg_info_m <- study_m$deg

x_m <- 0:25
y_m <- deg_info_m$deg_dist
err_m <- deg_info_m$deg_sd

k_mean_m <- deg_info_m$k_mean


df1 <- data.frame(x = x_a, y = y_a, err = err_a)
df2 <- data.frame(x = x_m, y = y_m, err = err_m)

x_vals <- 0:25
funfun <- data.frame(
  x = rep(x_vals, 2),
  y = c(
    artime_function(x_vals, alpha_a, r_a, 5000),  #Artime
    moore_function(x_vals, alpha_m)  # Moore
  ),
  formula = rep(c("Artime", "Moore"), each = length(x_vals))
)


df1$source <- "Artime"
df2$source <- "Moore"
funfun$source <- funfun$formula

vlines <- data.frame(
  x = c(k_mean_a, k_mean_m),
  type = c("Mean Artime", "Mean Moore"),
  color = c("Mean Artime", "Mean Moore")
)

ggplot() +
  # Data
  geom_point(data = df1, aes(x = x, y = y, color = source), size = 3) +
  geom_errorbar(data = df1, aes(x = x, ymin = y - err, ymax = y + err, color = source), width = 0.1) +
  geom_point(data = df2, aes(x = x, y = y, color = source), size = 3) +
  geom_errorbar(data = df2, aes(x = x, ymin = y - err, ymax = y + err, color = source), width = 0.1) +

  # Theoretical curves
  geom_line(data = funfun, aes(x = x, y = y, color = source, linetype = source), linewidth = 0.5) +

  # Means
  geom_vline(data = vlines, aes(xintercept = x, color = type, linetype = type), linewidth = 1) +

  scale_color_manual(
    values = c(
      "Artime" = "blue",
      "Moore" = "red",
      "Mean Artime" = "#14bfc5",
      "Mean Moore" = "#ff5506"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Artime" = "solid",
      "Moore" = "solid",
      "Mean Artime" = "dotdash",
      "Mean Moore" = "dotdash"
    )
  ) +

  labs(
    title = "Degree study",
    x = "Degree",
    y = "P",
    color = "Legend",
    linetype = "Legend"
  ) +

  theme_bw(base_size = 14)

#To save the figure
#ggsave("degree_distribution.png", width = 8, height = 6, dpi = 300)


#
#
#
#Study of the size of the LCC
#
#
#

study_lcc_a <- multiple_run(g, artime, realizations = 20, timesteps = 1000, alpha = alpha_a, r = r_a, study_lcc = TRUE)
study_lcc_m <- multiple_run(g, moore, realizations = 20, timesteps = 1000, alpha = alpha_m, r = r_m, study_lcc = TRUE)

df1 <- as.data.frame(study_lcc_a$lcc)
df2 <- as.data.frame(study_lcc_m$lcc)

ggplot() +
  # Ribbon dataset 1
  geom_ribbon(data = df1, aes(x = timestep, ymin = S - S_sd, ymax = S + S_sd, fill = "Artime"),
              alpha = 0.2) +

  # Line dataset 1
  geom_line(data = df1, aes(x = timestep, y = S, color = "Artime"), 
            linewidth = 1.2, linetype = "solid") +

  # Ribbon dataset 2
  geom_ribbon(data = df2, aes(x = timestep, ymin = S - S_sd, ymax = S + S_sd, fill = "Moore"),
              alpha = 0.2) +

  # Line dataset 2
  geom_line(data = df2, aes(x = timestep, y = S, color = "Moore"),
            linewidth = 1.2, linetype = "solid") +

  scale_color_manual(values = c("Artime" = "blue", "Moore" = "red")) +
  scale_fill_manual(values = c("Artime" = "blue", "Moore" = "red")) +

  guides(fill = "none") +

  # Legend
  labs(
    title = "Study of LCC",
    x = "Timestep",
    y = "S",
    color = "Simulations"
  ) +
  theme_bw(base_size = 14)

#To save the figure
#ggsave("Size_lcc.png", width = 8, height = 6, dpi = 300)












