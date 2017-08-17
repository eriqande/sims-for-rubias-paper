library(tidyverse)
load("data/cjfas_simulation_results.Rdata")

OUTDIR <- "plots_for_paper"
dir.create(OUTDIR)

################################# Figure 2 (Coalescent Simulation Cross-Validation)

rho_data <- coal_rho_data

rho_data$repunit <- rep(c("RU~1~~(K[r]==2)", "RU~2~~(K[r]==3)","RU~3~~(K[r]==12)"), 100)
rho_data$method <- rep(c("Posterior~Mean", "Bootstrap~Corrected"), each = 150)
rho_data$method <- as.factor(rho_data$method)
rho_data$method <- factor(rho_data$method, levels = levels(rho_data$method)[2:1])

fig.2 <- ggplot2::ggplot(rho_data, ggplot2::aes(x = true_rho, y = Estimate, colour = repunit)) +
  ggplot2::geom_point() +
  ggplot2::facet_grid(repunit ~ method, labeller = label_parsed) +
  ggplot2::labs(x = 'True (Simulated) Mixing Proportion', y = 'Estimated Mixing Proportion') +
  ggplot2::guides(color = F) +
  scale_color_brewer(palette = "Set1") +
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_bw()

ggsave(fig.2, filename = file.path(OUTDIR, "figure_2.pdf"), width = 4.6, height = 6)



######################## Figure 3 (Effects of Kr/K on the residuals)
coal_bias_data <- coal_rho_data
coal_bias_data$prop_diff <- (coal_bias_data$Estimate - coal_bias_data$true_rho) / coal_bias_data$true_rho
coal_bias_data$diff <- (coal_bias_data$Estimate - coal_bias_data$true_rho)
coal_bias_data$Np_C <- rep(c(2/17, 3/17, 12/17), 100)
coal_bias_data$Np_diff <- coal_bias_data$Np_C - coal_bias_data$true_rho
coal_bias_data <- coal_bias_data %>%
  mutate(method = factor(c("Posterior Mean", "Bootstrap Corrected")[method], levels = c(c("Posterior Mean", "Bootstrap Corrected")) ))

fig.3 <- ggplot2::ggplot(coal_bias_data, aes(x = Np_diff, y = diff)) +
  geom_point() +
  theme(axis.title.x = element_text(margin = margin(t = 2, b = 0)),
        axis.title.y = element_text(margin = margin(t = 0, b = 2)),
        plot.margin = margin(t = 6, r = 6, b = 0, l = 6)) +
  facet_grid(method ~ .) +
  geom_abline(intercept = 0, slope = 0) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm", colour = "red", linetype = "dashed", level = 0) +
  labs(x = expression(over(K[C],K)~~-~rho[r]^{sim}), y = expression(Residual~(tilde(rho)[r]-rho[r]^{sim}))) +
  scale_color_brewer(palette = "Set1") +
  theme_bw()


ggsave(fig.3, filename = file.path(OUTDIR, "figure_3.pdf"), width = 4.14, height = 4.53)
## Exported as PDF, dimensions 4.14 x 4.53 inches
