# commonly used functions for Space Optimisation

# created this file so that there is no direct link between input files and core models

source("conf/spaceopt_config.R")

# returns the STORE_DB dataframe
store_db <- function()
{
  read.csv(file = STORE_DB.f,
           stringsAsFactors = F)
}

# returns total space of a store
total_space <- function(store_id)
{
  STORE_DB <- store_db()
  STORE_DB$STORE_SPACE[which(STORE_DB$STORE_ID==store_id)][1]
}

# returns space elasticity parameters for all stores
space_elasticity_params_db <- function()
{
  read.csv(file = SPACE_ELASTICITY_PARAMS_DB.f,
           stringsAsFactors = F)
}

# returns space elasticity parameters for a given store
space_elasticity_params_store <- function(store_id)
{
  # at the moment there is only one store cluster
  space_elasticity_params_db()
}

# read min and max space configuration file
min_max_space <- function()
{
  read.csv(file = MIN_MAX_SPACE_CONFIG_DB.f,
           stringsAsFactors = F)
}

# read min and max space for a given store
min_max_space_store <- function (store_id)
{
  MIN_MAX_SPACE <- min_max_space()
  match_row <- which(MIN_MAX_SPACE$STORE_ID==store_id)
  MIN_MAX_SPACE[match_row,]
}
