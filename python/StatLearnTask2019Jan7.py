# mixture of normal distributions in 2D
# Distributions should not be too far away each other not be too close each other

# Performance of methods depends on (i) locations/variance of normal distributions and (ii) sample size

# Write the codes to generate a sample set first, and
# Visualize the sample records in 2D and grab the difficuty of the task at a glance

# Number of normal distributions :
ndist = 3

# Sample size :
nsample = 100

# Centers of normal distributions
import numpy as np
ctrs = np.arrays([[0.0,0],[1,0],[0.7,0.4]])

# Sigma matrix
sigmas = np.arrays([[],[]],[[],[]],[[],[]])
