# space allocation for a store
# by creating a data frame for the store that has product id, mods and KPI value
#

# function: optimised_space_allocation
# input: store_id
# output: optimised space allocation object

source("R/spaceopt/common/spaceopt_common.R")
source("R/utils/scalene_utilities.R")
library("reshape2")
library("dplyr")

optimised_space_allocation <- function (store_id)
{

  # store_id = 1206
  # Step 1: Get the store characteristics - total space of the store
  total_space <- total_space(store_id)
  # Step 2: Get the space elasticity parameters for the store
  SPACE_ELASTICITY_PARAMS <- space_elasticity_params_store(store_id)
  # Step 3: Get the min and max space for each product id for the store
  MIN_MAX_SPACE <- min_max_space_store(store_id)

  # Step 4: Get the base curve for each product id and space count
  SPACE_ELASTICITY.BASE_CURVE <- base_curve(elasticity_params = SPACE_ELASTICITY_PARAMS,
                                            min_space = 1,
                                            max_space = 20)
  # print("BASE CURVE")
  # print(names(SPACE_ELASTICITY.BASE_CURVE))
  # PRODUCT_ID, CLUSTER_ID, ALPHA, BETA, GAMMA, DELTA, SPACE, CUM_KPI, SPACE.n
  # Step 5: Transform space elasticity curve
  # ignore for now
  SPACE_ELASTICITY.TX <- SPACE_ELASTICITY.BASE_CURVE

  # Step 6: Get incremental kpi
  SPACE_ELASTICITY.INC <- incremental_kpi(SPACE_ELASTICITY.TX)
  # print("INCREMENTAL KPI")
  # print(names(SPACE_ELASTICITY.INC))
  # PRODUCT_ID, CLUSTER_ID, ALPHA, BETA, GAMMA, DELTA, SPACE, CUM_KPI, SPACE.n, CUM_KPI_LAG

  # Step 7: Use MIN_MAX
  SPACE_ELASTICITY.MIN_MAX <- min_max_adjusted(SPACE_ELASTICITY.INC, MIN_MAX_SPACE)
  # print(names(SPACE_ELASTICITY.MIN_MAX))
  # STORE_ID, PRODUCT_ID, SPACE, SPACE.n, CUM_KPI, CUM_KPI_LAG

  # Step 8: Calculate incremental KPI
  SPACE_ELASTICITY.MIN_MAX$INC_KPI <- SPACE_ELASTICITY.MIN_MAX$CUM_KPI - ifelse(is.na(SPACE_ELASTICITY.MIN_MAX$CUM_KPI_LAG),
                                                                                0,SPACE_ELASTICITY.MIN_MAX$CUM_KPI_LAG)

  # Step 9: Allocate minimum space for each product id
  SPACE_ALLOCATION <- allocate_min_space(store_id, MIN_MAX_SPACE, total_space)

  # Step 10: Allocate remainder of space
  # space_after_minimum <- sum(MIN_MAX_SPACE$MIN_SPACE, na.rm = T) + 1
  # get Elasticity Allocation results in decreasing order of Incremental KPI
  SPACE_ELASTICITY_ALLOCATION <- SPACE_ELASTICITY.MIN_MAX[order(SPACE_ELASTICITY.MIN_MAX$INC_KPI,
                                                                decreasing = T),]
  # now lets figure out how much of space will be filled after the minimum space is covered
  space_to_fill <- seq(sum(MIN_MAX_SPACE$MIN_SPACE,na.rm=T)+1,total_space)
  SPACE_ALLOCATION[space_to_fill,c("PRODUCT_ID","INCREMENTAL_KPI")] <- SPACE_ELASTICITY_ALLOCATION[1:length(space_to_fill),c("PRODUCT_ID","INC_KPI")]
  SPACE_ALLOCATION
}

# calculates base space elasticity curve
base_curve <- function(elasticity_params, min_space=1, max_space=20)
{
  possible_space_count <- seq(min_space, max_space)
  SPACE_ELASTICITY <- data.frame(matrix(data = vector(),
                                        nrow = nrow(elasticity_params),
                                        ncol = ncol(elasticity_params)+length(possible_space_count)))
  colnames(SPACE_ELASTICITY) <- c(colnames(elasticity_params),
                                  sprintf("S%04d",possible_space_count))
  SPACE_ELASTICITY[,c(1:ncol(elasticity_params))] <- elasticity_params
  SPACE_ELASTICITY <- melt(data = SPACE_ELASTICITY,
                           id.vars = c("PRODUCT_ID","CLUSTER_ID","ALPHA","BETA","GAMMA","DELTA"),
                           variable.name = "SPACE",
                           value.name = "CUM_KPI")
  SPACE_ELASTICITY$SPACE.n <- as.numeric(gsub(pattern = "S([0-9]*)",
                                              replacement = "\\1",
                                              x = SPACE_ELASTICITY$SPACE))
  SPACE_ELASTICITY$CUM_KPI <- apply(SPACE_ELASTICITY[,c(3:6,9)],
                                    function(x){
                                      space_curve_value2(space = x[5],
                                                        alpha = x[1],
                                                        beta = x[2],
                                                        gamma = x[3],
                                                        delta = x[4])
                                    },
                                    MARGIN = 1)

  SPACE_ELASTICITY
}

# function to adjust space elasticity curve for min and max space
min_max_adjusted <- function (space_elasticity, min_max_db)
{
  temp.df <- merge(x = space_elasticity,
                   y = min_max_db,
                   by = c("PRODUCT_ID"),
                   all.x = T)
  temp.df$valid_space <- apply(temp.df[,c("SPACE.n","MIN_SPACE","MAX_SPACE")],
                               function(x) {
                                 if((x[1] > x[2]) & (x[1] <= x[3]))
                                   1
                                 else
                                   0
                               },
                               MARGIN = 1)
  temp.df <- subset(temp.df, valid_space == 1)
  # temp.df[,-c("MIN_SPACE","MAX_SPACE","valid_space")]
  # temp.df$MIN_SPACE <- NULL
  # temp.df$MAX_SPACE <- NULL
  # temp.df$valid_space <- NULL
  # temp.df$ALPHA <- NULL
  # temp.df
  temp.df[,c("STORE_ID","PRODUCT_ID","SPACE","SPACE.n","CUM_KPI","CUM_KPI_LAG")]
}

# function to calculate incremental KPI from cumulative KPI
# we will use dplyr::lag function to calculate the lagged cumulative KPI
# and then we will subtract
incremental_kpi <- function (space_elasticity_curve)
{
  #space_elasticity_curve dataframe will have following columns
  #STORE_ID
  #PRODUCT_ID
  #SPACE
  #SPACE.n
  #CUM_KPI
  # by_product_id <- space_elasticity_curve %>%
  #                       group_by(PRODUCT_ID)
  space_elasticity_curve <- space_elasticity_curve %>%
                            group_by(PRODUCT_ID) %>%
                              mutate(CUM_KPI_LAG = lag(CUM_KPI, order_by = SPACE.n)) %>%
                                arrange(PRODUCT_ID, SPACE.n)
  space_elasticity_curve
}

# function to allocate space to minimum number of space for each product id
allocate_min_space <- function(store_id, min_max_space_db, total_space)
{
  # create the data frame for space allocation output
  SPACE_ALLOCATION <- data.frame(STORE_ID = rep(store_id, times=length(total_space)),
                                 SPACE = seq(1,total_space),
                                 PRODUCT_ID=NA,
                                 INCREMENTAL_KPI=NA)
  cumulative_min_space <- sum(min_max_space_db$MIN_SPACE,na.rm=T)
  SPACE_ALLOCATION$PRODUCT_ID[1:cumulative_min_space] <- unlist(apply(min_max_space_db[,c(2,3)],
                                                                function(x){
                                                                  product_id = x[1]
                                                                  repeat_times = as.numeric(x[2])
                                                                  rep(product_id, times=repeat_times)
                                                                },
                                                                MARGIN = 1))
  SPACE_ALLOCATION
}


