library(gridExtra)

get_plot <- function(fname, y_low, y_high, by) {
  prop_losses <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  rdts <- "benchmark/" %+% fname %+% "/out/" %+% fname %+% "_double_weight_v3_" %+% prop_losses %+% ".RDS"

  plots_prop_loss <- map(seq(1, length(rdts)), function(i) {
    rdt <- readRDS(rdts[i])
    p <- plot_valid_loss(rdt[[6]], y_low, y_high, by, fname %+% ", " %+% prop_losses[i])
    return(p)
  })

  # TODO: plot test loss by prop_loss

  return(plots_prop_loss)
}

get_prop_loss_plot <- function(fname, y_low, y_high, by) {
  prop_losses <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  rdts <- "benchmark/" %+% fname %+% "/out/" %+% fname %+% "_double_weight_v3_" %+% prop_losses %+% ".RDS"
  test_losses <- unlist(map(seq(1, length(rdts)), function(i) {
    rdt <- readRDS(rdts[i])
    return(rdt[[3]])
  }))

  p <- plot_test_loss(test_losses, y_low, y_high, by, fname)

  return(p)
}

cpu <- get_plot("cpu", 10, 30, 5)
cpu[[12]] <- get_prop_loss_plot("cpu", 30, 45, 5)
laheart <- get_plot("laheart", 0, 0.5, 0.1)
laheart[[12]] <- get_prop_loss_plot("laheart", 0.6, 1.5, 0.2)
pima <- get_plot("pima", 0.1, 0.4, 0.1)
pima[[12]] <- get_prop_loss_plot("pima", 0.4, 0.7, 0.1)
oecdpanel <- get_plot("oecdpanel", 0.2, 0.45, 0.05)
oecdpanel[[12]] <- get_prop_loss_plot("oecdpanel", 0.5, 0.6, 0.05)
fev <- get_plot("fev", 0.25, 0.4, 0.05)
fev[[12]] <- get_prop_loss_plot("fev", 0.345, 0.36, 0.005)

cpu_plts <- grid.arrange(grobs = cpu, ncol = 4)
laheart_plts <- grid.arrange(grobs = laheart, ncol = 4)
pima_plts <- grid.arrange(grobs = pima, ncol = 4)
oecdpanel_plts <- grid.arrange(grobs = oecdpanel, ncol = 4)
fev_plts <- grid.arrange(grobs = fev, ncol = 4)

ggsave("benchmark/plots/cpu.pdf", plot = cpu_plts, device = "pdf",
       width = 10, height = 6, dpi = 300, units = "in")
ggsave("benchmark/plots/laheart.pdf", plot = laheart_plts, device = "pdf",
       width = 10, height = 6, dpi = 300, units = "in")
ggsave("benchmark/plots/pima.pdf", plot = pima_plts, device = "pdf",
       width = 10, height = 6, dpi = 300, units = "in")
ggsave("benchmark/plots/oecdpanel.pdf", plot = oecdpanel_plts, device = "pdf",
       width = 10, height = 6, dpi = 300, units = "in")
ggsave("benchmark/plots/fev.pdf", plot = fev_plts, device = "pdf",
       width = 10, height = 6, dpi = 300, units = "in")
