# Space Elasticity Transformations
#
source("conf/spaceopt_config.R")
source("R/utils/scalene_utilities.R")

elasticity_transformation <- function(curve_values, modifying_values, transformType=TX_PARAMS$ADD)
{
  # check if the length of curve_values and modifying_values is same or not
  if(length(curve_values) != length(modifying_values))
    return(ERROR_MSG$TX_MISMATCH_LENGTH)

  # call transformation function based on transformType
  if(transformType == TX_PARAMS$ADD)
    elasticity_transformation_additive(curve_values, modifying_values)
  else if(transformType == TX_PARAMS$MUL)
    elasticity_transformation_multiplicative(curve_values,modifying_values)
  else if(transformType == TX_PARAMS$EXP)
    elasticity_transformation_exponential(curve_values,modifying_values)
  else if(transformType == TX_PARAMS$LOG)
    elasticity_transformation_logarithmic(curve_values,modifying_values)
  else
    return(ERROR_MSG$TX_INVALID_TRANSFORM)
}

elasticity_transformation_additive <- function(curve_values, modifying_values)
{
  # make sure that the space elasticity curve is increasing curve
  modified_curve <- (curve_values + modifying_values)
  return(space_elasticity_curve_increasing(modified_curve))
}


elasticity_transformation_multiplicative <- function(curve_values, modifying_values)
{
  # make sure that the space elasticity curve is increasing curve
  modified_curve <- (curve_values * modifying_values)
  return(space_elasticity_curve_increasing(modified_curve))
}

elasticity_transformation_exponential <- function(curve_values, modifying_values)
{
  # make sure that the space elasticity curve is increasing curve
  modified_curve <- (curve_values * exp(modifying_values))
  return(space_elasticity_curve_increasing(modified_curve))
}

elasticity_transformation_logarithmic <- function(curve_values, modifying_values)
{
  # make sure that the space elasticity curve is increasing curve
  modified_curve <- (curve_values * log(modifying_values))
  return(space_elasticity_curve_increasing(modified_curve))
}

# ensure that space elasticity curve is increasing function
space_elasticity_curve_increasing <- function(curve_value)
{
  # assumes that the curve values are for increasing order of space
  # find the row with maximum curve value
  # for curve values beyond this row, return max of curve value or maximum value
  num_rows <- length(curve_value)
  max_curve_value <- max(curve_value)
  max_curve_row <- which(curve_value == max_curve_value)[1]
  increasing_curve_value <- curve_value
  # print(num_rows)
  # print(max_curve_row)
  increasing_curve_value[1:max_curve_row] <- curve_value[1:max_curve_row]
  if(max_curve_row < num_rows)
  {
    # increasing_curve_value[max_curve_row_1:num_rows] <- rep(x = max_curve_value,times=num_rows-max_curve_row)
    # dont know why, but the above row does not work
    # length(increasing_curve_value[max_curve_row+1:num_rows]) is coming out as length(increasing_curve_value)
    next_row = max_curve_row + 1
    increasing_curve_value[next_row:num_rows] <- rep(x = max_curve_value,times=num_rows-max_curve_row)
  }
  return(increasing_curve_value)
}
