source("dvonmises.R")
source("rvonmises.R")
source("rejection-sampler.R")
source("importance-resampler.R")
source("show-vonmises.R")

# Unimodal
show_vonmises(200, pi / 1.3, pi, 2, 2, 1)

# Unimodal
show_vonmises(200, pi / 1.3, pi, 2, 2, 1, use_sir = TRUE)

# Bimodal (positive lambda)
show_vonmises(200, 0, sqrt(2.5), 2, 2, 3)

# Bimodal (negative lambda)
show_vonmises(200, 0, 0, 1, 3, -4)

# Very concentrated
show_vonmises(200, 0, pi, 30, 10, -8)

# Only contours
show_vonmises(0, 0, 0, 2, 2, 1)

# Only samples
show_vonmises(300, 0, 0, 2, 2, 1, show_contours = FALSE)

