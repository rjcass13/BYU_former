library(reshape2)
library(ggplot2)
library(coda)

# Generate a starting array. 
# Will randomly assign ups and downs where an up has prob p_up
create_array <- function(rows = 4, cols = 4, p_up = .5) {
  x <- rep(1:rows, each = cols)
  y <- rep(1:cols, rows)
  spin = sample(c(-1, 1), size = rows*cols, replace = TRUE, prob = c(1-p_up, p_up))
  data.frame(x, y, spin)
}

# Generate a plot with one color for the up spin, another for the down
plot_ising <- function(lattice, step = 1, col_up = 'green', col_down = 'black') {
  ggplot(lattice, aes(x = x, y = y, fill = factor(spin))) +
    geom_tile(color = NA) + # Add white borders to squares
    scale_fill_manual(values = c('1' = col_up, '-1' = col_down)) + # Assign colors
    labs(title = paste("Ising Model Plot: Step", step),
          x = "X",
          y = "Y ",
          fill = "Spin") +
    theme_minimal() +
    coord_fixed(ratio = 1) # Ensure squares are indeed square

}

# Calculate the Hamiltonian. Eq 2
# The sum of the product of the point spin with the spins of the nearest neighbors
# Multiplied by -J where J is +1 for Ferromagnetic, and -1 for anti-Ferromagnetic, materials
calc_ham <- function(lattice, pos_x = 1, pos_y = 1, J = 1) {
  width <- max(lattice$x)
  height <- max(lattice$y)
  pos_spin <- lattice$spin[which((lattice$x == pos_x) & (lattice$y == pos_y))]

  # Define movements to nearest neighbors (Up, Down, Left, Right)
  x_adj <- c(0, 0, -1, 1)
  y_adj <- c(1, -1, 0, 0)
  H <- 0

  # Find the positions, and thus spins, of the nearest neighbors. Add to Hessian
  for (i in 1:4) {
    #### Find the X
    # If current position on the left bound, and looking for left neighbor, loop to right side
    if ((pos_x == 1) & (x_adj[i] == -1)) {
      x <- width
      # If current position on the right bound, and looking for right neighbor, loop to left side
    } else if ((pos_x == width) & (x_adj[i] == 1)) {
      x <- 1
      # Otherwise, just add the x adjustment
    } else {
      x <- pos_x + x_adj[i]
    }
    #### Find the Y
    # If current position on the left bound, and looking for left neighbor, loop to right side
    if ((pos_y == 1) & (y_adj[i] == -1)) {
      y <- height
      # If current position on the right bound, and looking for right neighbor, loop to left side
    } else if ((pos_y == height) & (y_adj[i] == 1)) {
      y <- 1
      # Otherwise, just add the x adjustment
    } else {
      y <- pos_y + y_adj[i]
    }

    # Find spin value of that neighbor, multiply by position spin. Add to sum
    neighbor_spin <- lattice$spin[which((lattice$x == x) & (lattice$y == y))]
    H <- H + neighbor_spin
  }

  # Return the final Hamiltonian value
  -J * H * pos_spin
}

# Calculate the total energy of the state
calc_energy <- function(lattice, J = 1) {

  pre_latticed_ham_calc <- function(pos_x, pos_y) {
    calc_ham(lattice, pos_x, pos_y, J)
  }

  .5 * sum(unlist(mapply(pre_latticed_ham_calc, pos_x = lattice$x, pos_y = lattice$y)))
}

# Calculate the magnetization of the state
calc_mag <- function(lattice, J = 1) {
  sum(lattice$spin)
}

# Perform one step in the Monte Carlo simulation (iterate over all points, decide whether to flip or not)
mc_step <- function(lattice, beta, J = 1) {
  width <- max(lattice$x)
  height <- max(lattice$y)

  for (x in 1:width) {
    for (y in 1:height) {
      index <- which((lattice$x == x) & (lattice$y == y))
      # Get spin at the current location
      spin <- lattice$spin[index]
      # Calculate the cost of changing spin signs
      cost <- -2 * calc_ham(lattice, pos_x = x, pos_y = y, J)
      # If cost is < 0, change spin. Otherwise, if random number less than energy cost, change spin
      if (cost < 0) { lattice$spin[index] <- spin * -1}
      else if (runif(1) < exp(-cost * beta)) { lattice$spin[index] <- spin * -1}
    }
  }
  # Return the final state of the lattice in this step
  lattice
}

# Run the simulation
ising_sim <- function(grid_dim, temps, eq_steps, mc_steps, p_up = 0.5, J = 1, kb = 1, plots = TRUE) {
  width <- grid_dim[1]
  height <- grid_dim[2]
  n_temps <- length(temps)
  

  # Derived values
  n1 <- 1 / (mc_steps * width * height)
  n2 <- 1 / (mc_steps^2 * width * height)

  # Vectors for the values at each step
  step_eng = vector(mode = "list", length = mc_steps)
  step_mag = vector(mode = "list", length = mc_steps)

  # Vectors for the agg. values at each temperature
  energy         = vector(mode = "list", length = n_temps)
  magnetization  = vector(mode = "list", length = n_temps)
  specific_heat  = vector(mode = "list", length = n_temps)
  susceptibility = vector(mode = "list", length = n_temps)
  mce_energy     = vector(mode = "list", length = n_temps)
  mce_mag        = vector(mode = "list", length = n_temps)

  # For each temperature value, run a simulation and record the overall metrics
  for (ind in 1:n_temps) {
    # Initialize lattice grid and the final metrics
    lattice <- create_array(width, height, p_up)
    E1 <- E2 <- M1 <- M2 <- 0
    
    # Define values calculated for each temperature
    beta <- 1/(kb * temps[ind])
    beta2 <- beta^2

    # Bring it to equilibrium
    for (i in 1:eq_steps) {
      mc_step(lattice, beta, J)
    }

    for (i in 1:mc_steps) {
      lattice <- mc_step(lattice, beta, J)
      step_eng[i] <- calc_energy(lattice, J)
      step_mag[i] <- calc_mag(lattice, J)
    }

    step_eng <- unlist(step_eng)
    step_mag <- unlist(step_mag)
    E1 <- sum(step_eng)
    E2 <- sum(step_eng^2)
    M1 <- sum(step_mag)
    M2 <- sum(step_mag^2)
    # Use the effective sample size!
    step_eng_scale <- n1 * step_eng
    step_mag_scale <- n1 * step_mag
    eng_ess <- length(step_eng_scale) * var(step_eng_scale) / spectrum0.ar(step_eng_scale)$spec
    mag_ess <- length(step_mag_scale) * var(step_mag_scale) / spectrum0.ar(step_mag_scale)$spec

    energy[ind] <- n1*E1
    magnetization[ind] <- n1*M1
    specific_heat[ind] <- (n1*E2 - n2*E1*E1)*beta2
    susceptibility[ind] <- (n1*M2 - n2*M1*M1)*beta
    mce_energy[ind] <- sqrt(var(step_eng_scale)/eng_ess)
    mce_mag[ind] <- sqrt(var(step_mag_scale)/mag_ess)

    cat('\r', 'Finished Temperature Index:', ind)
  }

  if (plots) {
    title_text <- paste('Ising Model:', width, 'x', height, '| EQ Steps:', eq_steps, '| MC Steps:', mc_steps)
    plot(temps, energy, main = title_text, xlab = 'Temperature')
    plot(temps, magnetization, main = title_text, xlab = 'Temperature')
    plot(temps, specific_heat, main = title_text, xlab = 'Temperature')
    plot(temps, susceptibility, main = title_text, xlab = 'Temperature')
    plot(temps, mce_energy, main = title_text, xlab = 'Temperature')
    plot(temps, mce_mag, main = title_text, xlab = 'Temperature')
  }

  data.frame(temps = unlist(temps), energy = unlist(energy), 
    magnetization = unlist(magnetization), specific_heat = unlist(specific_heat), 
    susceptibility = unlist(susceptibility), mce_energy = unlist(mce_energy), mce_mag = unlist(mce_mag))
}

# Plot the ising model output every plot_interval steps of a simulation
ising_sim_plots <- function(grid_dim, temp, sim_steps, plot_steps = c(1:10), save_plot_list = TRUE, colors = c('green', 'black'), p_up = 0.5, J = 1, kb = 1) {
  # Initialize grid
  width <- grid_dim[1]
  height <- grid_dim[2]
  lattice <- create_array(width, height, p_up)
  
  # Determine calculated values
  num_plots <- length(plot_steps) + 1
  beta <- 1/(kb * temp)
  beta2 <- beta^2

  # If a list of plots is requested, initialize the list
  if (save_plot_list) {
    plot_list <- vector(mode = "list", length = num_plots)
    plot_num <- 1
  }
  
  # Plot initial grid layout
  p <- plot_ising(lattice, step = 0, col_up = colors[1], col_down = colors[2])
  if (save_plot_list) {
    plot_list[[plot_num]] <- p
    plot_num <- plot_num + 1
  } else {
    print(p)
  }
  
  # Run through the designated number of simulation steps
  for (i in 1:sim_steps) {
    lattice <- mc_step(lattice, beta, J)
    # Create a graph at each step indicated
    if (i %in% plot_steps) {
      p <- plot_ising(lattice, step = i, col_up = colors[1], col_down = colors[2])
      if (save_plot_list) {
        plot_list[[plot_num]] <- p
        plot_num <- plot_num + 1
      } else {
        print(p)
      }
    }
  }

  # If requested, return the saved list of plots
  if (save_plot_list) {
    plot_list
  }
}


# Plot the ising model output every plot_interval steps of a simulation
ising_sim_mag <- function(grid_dim, temp, sim_steps, p_up = 0.5, J = 1, kb = 1) {
  # Initialize grid
  width <- grid_dim[1]
  height <- grid_dim[2]
  lattice <- create_array(width, height, p_up)
  
  # Determine calculated values
  beta <- 1/(kb * temp)
  beta2 <- beta^2
  n1 <- 1 / (width * height)

  # If a list of plots is requested, initialize the list
  mag_list <- vector(mode = "list", length = sim_steps)

  # Run through the designated number of simulation steps
  for (i in 1:sim_steps) {
    lattice <- mc_step(lattice, beta, J)
    mag_list[i] <- n1 * calc_mag(lattice, J)
  }

  # If requested, return the saved list of plots
  plot(1:sim_steps, test, main = 'Phase Changes: 5x5 | T = 2.2', xlab = 'Simulation Step', ylab = 'Magnetization')
}