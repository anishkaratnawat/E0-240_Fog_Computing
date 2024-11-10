library(queueing)
library(GenSA)

# Define function to calculate model and return ROk
calculate_model_rok <- function(mu_es, mu_ps, mu_ds, mu_os, mu_cs, mu_fs, d, t, k, n, sla_time, m_ps, m_fs, m_cs) {
  
  es_service_rate <- mu_es * 2
  ps_service_rate <- mu_ps
  ds_service_rate <- mu_ds * 3.5
  os_service_rate <- mu_os * 3.5
  cs_service_rate <- mu_cs
  fs_service_rate <- mu_fs

  node_es <- NewInput.MM1(lambda = 0, mu = es_service_rate, n = 0)
  node_ps <- NewInput.MMC(lambda = 0, mu = ps_service_rate, c = m_ps, n = 0)
  node_ds <- NewInput.MM1(lambda = 0, mu = ds_service_rate, n = 0)
  node_os <- NewInput.MM1(lambda = 0, mu = os_service_rate, n = 0)
  node_cs <- NewInput.MMC(lambda = 0, mu = cs_service_rate, c = m_cs, n = 0)
  node_fs <- NewInput.MMC(lambda = 0, mu = fs_service_rate, c = m_fs, n = 0)

  prob <- matrix(
    data = c(
      0, 0.5 * 0.5, 0.5, 0.5 * 0.5, 0, 0,
      0, 0.5 * 0.5, 0.5, 0.5 * 0.5, 0, 0,
      0, 0.5, 0, 0.5, 0, 0,
      0, 0, 0, 0, 1, 0,
      0, 0, 0, 0, 0, 1,
      0.5, 0, 0, 0, 0.5, 0
    ),
    nrow = 6, ncol = 6, byrow = TRUE
  )

  cjn <- NewInput.CJN(prob, n, 0, FALSE, 0, 0.001, node_es, node_ps, node_ds, node_os, node_cs, node_fs)
  model <- QueueingModel(cjn)
  return(model$ROk)
}

# Define the objective function to maximize average utilization across nodes
objective_function <- function(params) {

  mu_es <- params[1]
  mu_ps <- params[2]
  mu_ds <- params[3]
  mu_os <- params[4]
  mu_cs <- params[5]
  mu_fs <- params[6]
  m_ps <- round(params[7])
  m_fs <- round(params[8])
  m_cs <- round(params[9])
  
  d <- 0.5
  t <- 0.5
  k <- 0.5

  jobs <- 100
  sla_time <- 4
  
  rok_values <- calculate_model_rok(
    mu_es = mu_es, mu_ps = mu_ps, mu_ds = mu_ds, mu_os = mu_os, mu_cs = mu_cs, mu_fs = mu_fs,
    d = d, t = t, k = k,
    n = jobs, sla_time = sla_time, m_ps = m_ps, m_fs = m_fs, m_cs = m_cs
  )
  
  # Check if any ROk values are negative. If yes, penalize this configuration heavily.
  if (any(rok_values < 0) || any(rok_values > 1)) {
    cat("One or more ROk values are beyong range, penalizing this configuration.\n")
    return(Inf)
  }
  
  # Replace NaN values with 0 and calculate the average
  rok_values[is.na(rok_values)] <- 0
  avg_utilization <- mean(rok_values, na.rm = TRUE)
  
  cat("Average Utilization (ROk) for current parameters:", avg_utilization, "\n")

  return(-avg_utilization)
}

# Define parameter bounds for simulated annealing
lower_bounds <- c(
  0.1, 0.1, 0.1, 0.1, 0.1, 0.1, # Lower bounds for mu_es, mu_ps, mu_ds, mu_os, mu_cs, mu_fs
  1, 1, 1                        # Lower bounds for m_ps, m_fs, m_cs (server counts)
)

upper_bounds <- c(
  10, 10, 10, 10, 10, 10,              # Upper bounds for mu_es, mu_ps, mu_ds, mu_os, mu_cs, mu_fs
  10, 10, 10                     # Upper bounds for m_ps, m_fs, m_cs (server counts)
)

# Run simulated annealing
result <- GenSA(
  par = c(5, 5, 5, 5, 5, 5, 3, 3, 3), # Initial parameter values
  fn = objective_function,
  lower = lower_bounds,
  upper = upper_bounds,
  control = list(max.call = 5000, temperature = 1000) # Control settings for annealing process
)

# Print the optimized parameters and result
optimized_params <- list(
  mu_es = result$par[1],
  mu_ps = result$par[2],
  mu_ds = result$par[3],
  mu_os = result$par[4],
  mu_cs = result$par[5],
  mu_fs = result$par[6],
  m_ps = round(result$par[7]),
  m_fs = round(result$par[8]),
  m_cs = round(result$par[9])
)

average_utilization <- -result$value

cat("Optimized Parameters:\n")
print(optimized_params)
cat("\nMaximized Average Node Utilization (ROk):", average_utilization, "\n")
