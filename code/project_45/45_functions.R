library(ggplot2)
library(rgl)
library(igraph)
library(RColorBrewer)
library(gridExtra)
library(patchwork)
library(compiler)








overload_row <- function(g, alpha_min, alpha_max, alpha_step, modmod) {

  if (abs(((alpha_max - alpha_min) / alpha_step) - round((alpha_max - alpha_min) / alpha_step)) > .Machine$double.eps^0.5) {
    stop("'interval/step' not integer")
  }

  # Total number of nodes in the original graph
  N <- vcount(g)

  # Ensure nodes have names
  if (is.null(V(g)$name)) {
    V(g)$name <- as.character(seq_along(V(g)))
  }

  steps <- seq(alpha_min, alpha_max, by = alpha_step)
  study <- data.frame(alpha = steps, S = NA_real_)

  # Compute original betweenness for threshold reference
  bet_zero_full <- betweenness(g, normalized = TRUE)
  names(bet_zero_full) <- V(g)$name

  # Initial removal of one node
  g_start <- g
  if (modmod == "rand") {
    to_delete <- sample(V(g_start)$name, 1)
  } else if (modmod == "deg") {
    to_delete <- names(which.max(degree(g_start)))
  } else if (modmod == "betw") {
    to_delete <- names(which.max(bet_zero_full))
  } else {
    stop("modmod must be 'rand', 'deg', or 'betw'")
  }

  g_start <- delete_vertices(g_start, to_delete)
  bet_zero <- bet_zero_full[!names(bet_zero_full) %in% to_delete]

  # Run overload process for each alpha on same g_start
  for (j in seq_along(steps)) {
    a <- steps[j]

    g2 <- g_start

    C <- (1 + a) * bet_zero

    while (length(V(g2)) > 0) {

      bet_new <- betweenness(g2, normalized = TRUE)
      names(bet_new) <- V(g2)$name

      common_nodes <- intersect(names(bet_new), names(C))
      to_delete_ids <- names(bet_new[common_nodes][bet_new[common_nodes] > C[common_nodes]])

      if (length(to_delete_ids) == 0) break

      g2 <- delete_vertices(g2, to_delete_ids)
      C <- C[!names(C) %in% to_delete_ids]
    }

    # Compute normalized S
    S_raw <- if (vcount(g2) > 0) max(components(g2)$csize) else 0
    study$S[j] <- S_raw / N

  }

  colnames(study) <- c("alpha", "S")
  return(study)
}
overload <- cmpfun(overload_row)








multiple_row <- function(g, alpha_min, alpha_max, alpha_step, iterations) {
  if (iterations < 1 || iterations != as.integer(iterations)) {
    stop("'iterations' must be a positive integer")
  }

  modmod <- "rand"

  steps <- seq(alpha_min, alpha_max, by = alpha_step)
  n_steps <- length(steps)
  all_results <- matrix(NA_real_, nrow = iterations, ncol = n_steps)

  for (i in seq_len(iterations)) {
    result <- overload(g, alpha_min, alpha_max, alpha_step, modmod)
    all_results[i, ] <- result$S
  }

  S_mean <- colMeans(all_results)
  S_sd <- apply(all_results, 2, sd)

  final_result <- data.frame(
    alpha = steps,
    S = S_mean,
    S_sd = S_sd
  )

  return(final_result)
}
multiple <- cmpfun(multiple_row)
