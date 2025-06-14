if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork)

set.seed(2025)
n <- 1000
tau <- 1
gamma <- 2

simulate_case <- function(cor_DX, label) {
  D <- rnorm(n)
  X <- cor_DX * D + sqrt(1 - cor_DX^2) * rnorm(n)
  X_star <- X + rnorm(n, sd = 1)
  Y <- tau * D + gamma * X + rnorm(n)
  
  # Residualize D w.r.t. X and X*
  D_resid <- residuals(lm(D ~ X))
  D_resid_star <- residuals(lm(D ~ X_star))
  
  data.frame(D, X, X_star, Y, label, D_resid, D_resid_star)
}

# Simulate and combine
df_all <- bind_rows(
  simulate_case(0.7, "Corr(D,X) = +0.7"),
  simulate_case(-0.7, "Corr(D,X) = -0.7")
)

# Prediction line generator (uses D residualized on true X for plotting)
get_lines_df <- function(df, label) {
  fit_true <- lm(Y ~ D + X, data = df)
  fit_star <- lm(Y ~ D + X_star, data = df)
  
  d_seq <- seq(min(df$D), max(df$D), length.out = 200)
  x_fixed <- mean(df$X)
  x_star_fixed <- mean(df$X_star)
  
  pred_true <- predict(fit_true, newdata = data.frame(D = d_seq, X = x_fixed))
  pred_star <- predict(fit_star, newdata = data.frame(D = d_seq, X_star = x_star_fixed))
  
  d_resid_seq <- d_seq - mean(df$D)
  
  data.frame(
    D_resid = rep(d_resid_seq, 2),
    Y = c(pred_true, pred_star),
    Model = rep(c("True X", "Measured X*"), each = length(d_seq)),
    label = label
  )
}

# Combine prediction lines
df_lines <- bind_rows(
  get_lines_df(df_all |> filter(label == "Corr(D,X) = +0.7"), "Corr(D,X) = +0.7"),
  get_lines_df(df_all |> filter(label == "Corr(D,X) = -0.7"), "Corr(D,X) = -0.7")
)

# Plot: black = D residualized on X; red = D residualized on X*
p <- ggplot() +
  geom_point(data = df_all, aes(x = D_resid, y = Y), alpha = 0.1, color = "black") +
  geom_point(data = df_all, aes(x = D_resid_star, y = Y), alpha = 0.1, color = "red") +
  geom_line(data = df_lines, aes(x = D_resid, y = Y, color = Model, linetype = Model), size = 2) +
  facet_wrap(~ label) +
  scale_color_manual(values = c("True X" = "black", "Measured X*" = "red")) +
  scale_linetype_manual(values = c("True X" = "solid", "Measured X*" = "solid")) +
  labs(
    x = "Residualized D", y = "Y", color = NULL, linetype = NULL
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(-3, 3)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-10, 10)) +
  coord_cartesian(xlim = c(-2.5, 2.5 )) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")

pdf("./images/sim_confounder.pdf", width = 6, height = 6)
print(p)
dev.off()

ggsave(file = "./images/sim_confounder.svg", plot = p, width = 6, height = 6)