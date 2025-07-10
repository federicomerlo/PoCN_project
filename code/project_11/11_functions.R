library(igraph)
library(compiler)


edge_key <- function(i, j) {
  paste(sort(c(i, j)), collapse = "--")
}

#Artime degree distribution

c_t <- function(t, a, r) {
  (a / r) * (1 - exp(-r * t))
}

artime_function <- function(x, a, r, t) {
  c_val <- c_t(t, a, r)
  Q_val <- pgamma(c_val, shape = x + 1, lower.tail = FALSE)
  last_term <- exp(-c_val - r * t) * (c_val^x) / factorial(x)
  p_val <- (r / a) * (1 - Q_val) + last_term
  return(p_val)
}

#Moore degree distribution

gamma_upper <- function(a, x) {
  pgamma(x, shape = a, lower.tail = FALSE) * gamma(a)
}

moore_function <- function(x, c) {
  gamma_c1 <- gamma(c + 1)
  gamma_c1_c <- gamma_upper(c + 1, c)

  gamma_x1 <- gamma(x + 1)
  gamma_x1_c <- gamma_upper(x + 1, c)

  y <- ifelse(
    x < c,
    (gamma_c1 - gamma_c1_c) * gamma_x1_c / gamma_x1,
    gamma_c1_c * (1 - gamma_x1_c / gamma_x1)
  )
  return((exp(c) / c^(c + 1)) * y)
}


#General function to study multiple iterations on the same network

multiple_run_row <- function(network, fun, realizations, timesteps, alpha, r, study_lcc = FALSE) {

  k_mean_sum <- 0
  deg_matrix <- matrix(0, nrow = realizations, ncol = 26)
  s_count <- seq(0, timesteps, by = (timesteps * 0.1))
  lcc_matrix <- matrix(0, nrow = realizations, ncol = length(s_count))

  for (i in 1:realizations) {
    study <- fun(network, timesteps, alpha, r, study_lcc)

    # Study the degree distribution
    if (!study_lcc) {
      g <- study$network

      k_mean_sum <- k_mean_sum + (sum(degree(g)) / length(V(g)))

      deg_dist_real <- degree_distribution(g)
      l <- length(deg_dist_real)

      if (l < 26) {
        deg_dist_real <- c(deg_dist_real, rep(0, (26 - l)))
      }
      if (l > 26) {
        deg_dist_real <- deg_dist_real[1:26]
      }

      deg_matrix[i, ] <- deg_dist_real
    }

    # Study the size of the lcc
    if (study_lcc) {
      lcc_matrix[i, ] <- study$lcc$S
    }
  }

  if (study_lcc) {

    lcc_mean <- colMeans(lcc_matrix)
    lcc_dist_sd <- apply(lcc_matrix, 2, sd)

    degree_study <- 0

    lcc <- list(
      timestep = study$lcc$timestep,
      S = lcc_mean,
      S_sd = lcc_dist_sd
    )
  }
  if (!study_lcc) {

    deg_dist_mean <- colMeans(deg_matrix)
    deg_dist_sd <- apply(deg_matrix, 2, sd)

    degree_study <- list(
      k_mean = k_mean_sum / realizations,
      deg_dist = deg_dist_mean,
      deg_sd = deg_dist_sd
    )
    lcc <- 0
  }

  return(list(deg = degree_study, lcc = lcc))

}
multiple_run <- cmpfun(multiple_run_row)





#Implementation of Artime's approach 

artime_row <- function(network, timesteps, alpha, r, study_lcc = FALSE) {

  n <- length(V(network))

  n_add <- round(alpha * n / 2, digits = 0)
  if (n_add < 1) {
    print("adding 0 edges")
  }


  n_del <- round(r * n, digits = 0)
  if (n_del < 1) {
    print("resetting 0 nodes")
  }

  g <- network

  s_count <- seq(0, timesteps, by = (timesteps * 0.1))
  s_count[1] <- 1
  count <- 1
  lcc_set <- c()

  for (o in 1:timesteps) {

    # Keep track of the size of the lcc
    if (study_lcc) {
      if (o %in% s_count) {
        lcc_set[count] <- max(components(g)$csize) / n
        count <- count + 1
      }
    }

    if (n_add != 0) {
      # Find the existing edges
      existing_edges_set <- sapply(E(g), function(e) {
        v <- ends(g, e)
        edge_key(v[1], v[2])
      })

      new_edges_set <- character(0)
      new_edges_list <- list()

      # Create new edges
      while (length(new_edges_set) < n_add) {
        i <- sample(1:n, 1)
        j <- sample(setdiff(1:n, i), 1)

        key <- edge_key(i, j)

        if (!(key %in% existing_edges_set) && !(key %in% new_edges_set)) {
          new_edges_set <- c(new_edges_set, key)
          new_edges_list[[length(new_edges_list) + 1]] <- c(i, j)
        }
      }

      # Add the new edges
      for (edge in new_edges_list) {
        g <- add_edges(g, edge)
      }
    }

    # Reset some vertices' edges
    if (n_del != 0) {
      v_del <- sample(1:n, n_del)
      edges_to_del <- incident_edges(g, v_del)
      edges_combined <- do.call(c, edges_to_del)

      g <- delete_edges(g, edges_combined)
    }
  }

  if (study_lcc) {
    lcc <- data.frame(timestep = s_count[seq_along(lcc_set)], S = lcc_set)
  }
  if (!study_lcc) {
    lcc <- 0
  }

  return(list(network = g, lcc = lcc))

}
artime <- cmpfun(artime_row)







#Implementation of Moore's approach

moore_row <- function(network, timesteps, alpha, r = 1, study_lcc = FALSE) {

  if (r != 1) {
    stop("r must be 1")
  }

  if (alpha < 1) {
    stop("alpha must be greater then or equal to 1")
  }

  n <- length(V(network))

  g <- network

  s_count <- seq(0, timesteps, by = (timesteps * 0.1))
  s_count[1] <- 1
  count <- 1
  lcc_set <- c()

  for (o in 1:timesteps) {

    # Keep track of the size of the lcc
    if (study_lcc) {
      if (o %in% s_count) {
        lcc_set[count] <- max(components(g)$csize) / n
        count <- count + 1
      }
    }

    # Delete all adges of 1 node

    v_del <- sample(1:n, 1)
    edges_to_del <- incident_edges(g, v_del)
    edges_combined <- do.call(c, edges_to_del)

    if (length(edges_combined) > 0) {
      g <- delete_edges(g, edges_combined)
    }

    new_edges_set <- character(0)
    new_edges_list <- list()

    # Create new edges for that node

    while (length(new_edges_set) < alpha) {
      j <- sample(setdiff(1:n, v_del), 1)

      key <- edge_key(v_del, j)

      if (!(key %in% new_edges_set)) {
        new_edges_set <- c(new_edges_set, key)
        new_edges_list[[length(new_edges_list) + 1]] <- c(v_del, j)
      }
    }

    # Add the new edges
    for (edge in new_edges_list) {
      g <- add_edges(g, edge)
    }

  }

  if (study_lcc) {
    lcc <- data.frame(timestep = s_count[seq_along(lcc_set)], S = lcc_set)
  }
  if (!study_lcc) {
    lcc <- 0
  }

  return(list(network = g, lcc = lcc))

}
moore <- cmpfun(moore_row)












