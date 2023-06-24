library(ggplot2)
library(gridExtra)
library(purrr)

types <- c("smooth", "jumps", "sinusoidal")

plot_fun <- function(low, high) {
  plts <- map(1:3, function(i) {
    valid_loss_rand <- rand_res[[i]][[1]][[3]]
    valid_loss_samp <- samp_res[[i]][[1]][[3]]
    plt_df_rand <- data.frame(loss = valid_loss_rand, iter = seq(1, length(valid_loss_rand)))
    plt_df_samp <- data.frame(loss = valid_loss_samp, iter = seq(1, length(valid_loss_samp)))
    plt_df_rand <- plt_df_rand[!duplicated(plt_df_rand$loss), ]
    plt_df_samp <- plt_df_samp[!duplicated(plt_df_samp$loss), ]
    plt_df_rand <- rbind(plt_df_rand, data.frame(loss = min(valid_loss_rand), iter = length(valid_loss_rand)))
    plt_df_samp <- rbind(plt_df_samp, data.frame(loss = min(valid_loss_samp), iter = length(valid_loss_samp)))

    p <- ggplot(plt_df_rand, aes(x = iter, y = loss)) +
      geom_step(color = "blue", lwd = 1) +
      geom_step(data = plt_df_samp, color = "red", lwd = 1) +
      scale_x_continuous(breaks = seq(0, length(valid_loss_rand), by = 20),
                         limits = c(0, 100)) +
      scale_y_continuous(breaks = seq(low, high, by = 0.05),
                         limits = c(low, high)) +
      labs(title = types[i],
           x = "Iteration",
           y = "Best validation loss") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            legend.position = "none")
    return(p)
  })

  return(plts)
}

rand_res <- readRDS("simulation/062123_rand_vs_samp_var_5_n_1000/out/rand_var_5_n_1000.RDS")
samp_res <- readRDS("simulation/062123_rand_vs_samp_var_5_n_1000/out/samp_var_5_n_1000_double_weight_v3.RDS")
n_1000_plts <- plot_fun(low = 0.75, high = 1.2)
n_1000_plt <- grid.arrange(grobs = n_1000_plts, ncol = 3)
ggsave("simulation/062123_rand_vs_samp_var_5_n_1000/var_5_n_1000_rand_vs_samp.pdf", plot = n_1000_plt, device = "pdf",
       width = 10, height = 4, dpi = 300, units = "in")

rand_res <- readRDS("simulation/062123_rand_vs_samp_var_5_n_1M/out/rand_var_5_n_1M.RDS")
samp_res <- readRDS("simulation/062123_rand_vs_samp_var_5_n_1M/out/samp_var_5_n_1M_double_weight_v3.RDS")
n_1M_plts <- plot_fun(low = 0.75, high = 1.2)
n_1M_plt <- grid.arrange(grobs = n_1M_plts, ncol = 3)
ggsave("simulation/062123_rand_vs_samp_var_5_n_1M/var_5_n_1M_rand_vs_samp.pdf", plot = n_1M_plt, device = "pdf",
       width = 10, height = 4, dpi = 300, units = "in")


