if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork, Cairo)

set.seed(2025)

# Parameters
N <- 1000
n_val <- 200
B <- 1000
tau <- 2
gamma <- 1
alpha <- 0
delta_0 <- 0
delta_1 <- 1.5  # Strength of confounding (X -> D)

# Simulate data
X <- rnorm(N)
prob_D <- plogis(delta_0 + delta_1 * X)  # logistic(X) for treatment probability
D <- rbinom(N, 1, prob_D)                # True treatment influenced by X
Y <- alpha + tau * D + gamma * X + rnorm(N)

# Introduce 30% random flipping (misclassification)
D_star <- ifelse(runif(N) < 0.3, 1 - D, D)

# Validation indicator
S <- rep(0, N)
val_idx <- sample(1:N, n_val)
S[val_idx] <- 1

# Bootstrap storage
est_naive <- numeric(B)
est_rc <- numeric(B)

for (b in 1:B) {
  idx <- sample(1:N, N, replace = TRUE)
  
  X_b <- X[idx]
  D_b <- D[idx]
  D_star_b <- D_star[idx]
  Y_b <- Y[idx]
  S_b <- S[idx]
  
  # Naive regression
  fit_naive <- lm(Y_b ~ D_star_b + X_b)
  est_naive[b] <- coef(fit_naive)["D_star_b"]
  
  # Logistic regression calibration in validation sample
  calib_fit <- glm(D_b ~ D_star_b + X_b, subset = S_b == 1, family = binomial)
  D_hat_b <- predict(calib_fit, newdata = data.frame(D_star_b = D_star_b, X_b = X_b), type = "response")
  
  # RC regression with imputed treatment probability
  fit_rc <- lm(Y_b ~ D_hat_b + X_b)
  est_rc[b] <- coef(fit_rc)["D_hat_b"]
}

# Combine and plot
df_plot <- data.frame(
  estimate = c(est_naive, est_rc),
  method = rep(c("Naive (D*)", "Regression Calibration"), each = B)
)

# Plot
p <- ggplot(df_plot, aes(x = estimate, fill = method)) +
  geom_histogram(colour = "white", bins = 40, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = tau, linetype = 3, size = 1.5, color = "black") +
  facet_wrap(~ method, scales = "free") +
  labs(x = "Estimated Treatment Effect", y = "Count") +
  scale_x_continuous(limits = c(0, 3.5)) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")

pdf("./images/sim_reg_calibration.pdf", width = 8, height = 4)  
print(p)                                
dev.off()                               

ggsave(file = "./images/sim_reg_calibration.svg", plot = p, width = 8, height = 4)