if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork)

set.seed(2025)

simulate_once <- function(n, val_frac, misclass_rate = 0.15) {
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  p <- plogis(X1 + 0.5 * X2)
  D <- rbinom(n, 1, p)
  D_star <- ifelse(runif(n) < misclass_rate, 1 - D, D)
  
  # Define potential outcomes
  Y0 <- exp(X1) / 4 + sin(2 * X2) + rnorm(n)
  Y1 <- 2 + exp(X1) / 4 + sin(2 * X2) + rnorm(n)  # add ATE = 2
  
  # Reveal observed outcome
  Y <- ifelse(D == 1, Y1, Y0)
  
  # Validation indicator
  S <- rep(0, n)
  S[sample(1:n, size = n * val_frac)] <- 1
  
  df <- data.frame(Y, D, D_star, X1, X2, S)
  
  tau_val <- coef(lm(Y ~ D + X1 + X2, data = df |> filter(S == 1)))["D"]
  tau_ep_main <- coef(lm(Y ~ D_star + X1 + X2, data = df))["D_star"]
  tau_ep_val <- coef(lm(Y ~ D_star + X1 + X2, data = df |> filter(S == 1)))["D_star"]
  
  setNames(c(tau_val, tau_ep_main, tau_ep_val),
           c("tau_val", "tau_ep_main", "tau_ep_val"))
}

# Settings
n <- 1000
val_fracs <- seq(0.1, 0.5, by = 0.1)
n_sim <- 1000
true_tau <- 2

# Run and collect full simulation results
all_sims <- lapply(val_fracs, function(vf) {
  sims <- replicate(n_sim, simulate_once(n, vf), simplify = "matrix")
  
  tau_val_vec <- sims["tau_val", ]
  tau_ep_main_vec <- sims["tau_ep_main", ]
  tau_ep_val_vec <- sims["tau_ep_val", ]
  control_variate <- tau_ep_val_vec - tau_ep_main_vec
  b_hat <- cov(tau_val_vec, control_variate) / var(control_variate)
  tau_cv_vec <- tau_val_vec - b_hat * control_variate
  
  data.frame(
    val_frac = vf,
    validation_only = tau_val_vec,
    error_prone_main = tau_ep_main_vec,
    control_variates = tau_cv_vec
  )
})

# Reshape to long format
all_sims_df <- bind_rows(all_sims) |>
  pivot_longer(cols = c("validation_only", "error_prone_main", "control_variates"),
               names_to = "estimator", values_to = "estimate") |>
  mutate(bias = estimate - true_tau)

bias_summary <- all_sims_df |>
  group_by(val_frac, estimator) |>
  summarise(bias = mean(bias), variance = var(estimate), .groups = "drop") |> 
  mutate(RMSE = sqrt(bias^2 + variance)) |> 
  mutate(estimator = factor(estimator, levels = c("control_variates", "validation_only", "error_prone_main"))) |>
  mutate(estimator = recode(estimator, 
                            validation_only = "Validation Only",
                            error_prone_main = "Error-Prone Main",
                            control_variates = "Control Variates"))


# Create plots
p1 <- bias_summary |> 
  ggplot(aes(x = val_frac, y = bias, color = estimator, shape = estimator)) +
  geom_line(size = .5) +
  geom_point(size = 3) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Validation sample fraction", y = "Bias", color = NULL, shape = NULL)

p2 <- ggplot(bias_summary, aes(x = val_frac, y = variance, color = estimator, shape = estimator)) +
  geom_line(size = .5) +
  geom_point(size = 3) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Validation sample fraction", y = "Variance", color = NULL, shape = NULL)

# Combine with shared legend
p <- (p1 | p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "top")

pdf("./images/sim_cv.pdf", width = 8, height = 4)  
print(p)                                
dev.off()                               

ggsave(file = "./images/sim_cv.svg", plot = p, width = 8, height = 4)