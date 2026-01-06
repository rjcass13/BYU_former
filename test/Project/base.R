# The theory comes primarily from this report: https://arxiv.org/pdf/0803.0217
# Code help comes from: https://rajeshrinet.github.io/blog/2014/ising-model/

################################
# Base Simulation Study #
################################
source('functions.R')
set.seed(666)
grid_dim <- c(5, 5) # (width, height)
temps <- c(seq(1, 1.9, by = .05), seq(1.91, 2.5, by = .01), seq(2.51, 4, by = .05))
eq_steps <- 100
mc_steps <- 250
p_up <- .5
J <- 1
kb <- 1
res <- ising_sim(grid_dim, temps, eq_steps, mc_steps, p_up, J, kb, plots = TRUE)


################################
# Lattice Plots Throughout Sim #
################################
source('functions.R')
set.seed(666)
grid_dim <- c(32, 32) # (width, height)
sim_steps <- 150
plot_steps <- c(c(1:10), seq(11, 49, by = 3), seq(50, 150, by = 5))
temp <- 2
plots <- ising_sim_plots(grid_dim, temp, sim_steps, plot_steps, save_plot_list = TRUE)

###### Optional: Can run the following to convert those plots into a gif. Ended up not needing this
# library(magick) # For the animation
# library(gtools) # For the ordering of the files
# setwd("c:\\Users\\RJ\\Documents\\GitRepos\\stat624\\project\\images")
# for (i in seq_along(plots)) {
#   ggsave(filename = paste0("plot_frame_", i, ".png"), plot = plots[[i]])
# }
# img_list <- image_read(mixedsort(list.files(pattern = "plot_frame_")))
# animation <- image_animate(img_list, fps = 4)
# image_write(animation, "animation.gif")
# setwd("c:\\Users\\RJ\\Documents\\GitRepos\\stat624\\project")



##############################
# Magnetization Phase Change #
##############################
source('functions.R')
set.seed(666) # 624 also gave an interesting one at temp = 1.5
grid_dim <- c(5, 5) # (width, height)
sim_steps <- 1000
temp <- 2.2
ising_sim_mag(grid_dim, temp, sim_steps)


# To Do:
# Check numeric stability