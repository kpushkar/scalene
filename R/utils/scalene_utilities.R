# This R script will host different utility functions
# later on if we want to split it up into multiple R scripts, we will do so

sample_normal_df_with_range <- function(lower_bound, upper_bound, mean, sd, n, seed)
{
  # refer to the following page for instructions
  # http://r.789695.n4.nabble.com/how-to-generate-a-normal-distribution-with-mean-1-min-0-2-max-0-8-td3481450.html

  p_L <- pnorm(lower_bound, mean = mean, sd = sd)
  p_U <- pnorm(upper_bound, mean = mean, sd = sd)

  set.seed(seed = seed)
  x <- qnorm(runif(n,p_L, p_U),mean = mean, sd = sd)
  x
}

###############
# Function calculates the space elasticity values for given parameters
# returns list of values for all space values
###############
space_elasticity_values <- function (elasticity_params, space_values)
{
  x <- sapply(space_values, space_curve_value, elasticity_params = elasticity_params)
  # for some reason, x is list with name ALPHA
  # creating a matrix
  x <- matrix(x,ncol = 1,dimnames = list(space_values))
  x
}

###############
# Function calculates the growth curve formula
# This implementation is Gompertz curve
# returns curve value for given space
###############
space_curve_value <- function(space, elasticity_params)
{
  # make sure there are 4 parameters
  if(length(elasticity_params) != 4)
    "ERROR"
  alpha = elasticity_params[1]
  beta = elasticity_params[2]
  gamma = elasticity_params[3]
  delta = elasticity_params[4]

  alpha + (beta - alpha)*exp(-exp(gamma*(space-delta)))
}
