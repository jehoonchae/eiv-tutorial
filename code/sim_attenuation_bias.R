if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork)

set.seed(2025)

n <- 1000
tau_true <- 2
sigma_d <- 1
sigma_u <- 1
sigma_eps <- 1

# Data generation
D <- rnorm(n, 0, sigma_d)
u <- rnorm(n, 0, sigma_u)
D_star <- D + u
eps <- rnorm(n, 0, sigma_eps)
Y <- tau_true * D + eps

# Create long-format data for ggplot
df <- data.frame(
  Y = Y,
  D_true = D,
  D_star = D_star
)

# Regression fits
fit_true <- lm(Y ~ D_true, data = df)
fit_star <- lm(Y ~ D_star, data = df)

# Predictions for plotting regression lines
d_seq <- seq(-5, 5, length.out = 200)
pred_true <- predict(fit_true, newdata = data.frame(D_true = d_seq))
pred_star <- predict(fit_star, newdata = data.frame(D_star = d_seq))

df_lines <- data.frame(
  D = d_seq,
  Y_true = pred_true,
  Y_star = pred_star
)

# Plot
p <- ggplot() +
  geom_point(aes(x = D, y = Y), alpha = 0.2, color = "black") +
  geom_point(aes(x = D_star, y = Y), alpha = 0.2, color = "red") +
  geom_line(data = df_lines, aes(x = D, y = Y_true), color = "black", size = 2) +
  geom_line(data = df_lines, aes(x = D, y = Y_star), color = "red", size = 2) +
  labs(
    x = "D (or D*)",
    y = "Y"
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(-5, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-10, 10),
                     breaks = c(-10, -7.5, -5, -2.5, 0, 2.5, 5, 7.5, 10)) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank())

pdf("./images/sim_bias.pdf", width = 4, height = 6)
print(p)
dev.off()

ggsave(file = "./images/sim_bias.svg", plot = p, width = 4, height = 6)