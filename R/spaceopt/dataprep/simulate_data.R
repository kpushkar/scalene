# this script will create simulated data
#
# there are three types of data
# STORE_DB <- list of stores with following details for every store
#
# ----
# STORE_ID STORE_NAME STATE LOCATION POSTCODE GEO_LAT GEO_LONG
# ----
# ----
# SPACE_ALLOCATED_DB <- amount of space allocated to product hierarchy 3 for every store
# ----
# ----
# STORE_ID PRODUCT_ID PRODUCT_HIERARCHY SPACE_HIERARCHY ALLOCATED_SPACE SPACE_UOM EFFECTIVE_FROM EFFECTIVE_TO
# ----
#
# ANNUAL_SALES_DB <- annual sales by store and product
#
# ----
# STORE_ID PRODUCT_ID SALES MARGIN
#
# SPACE_ELASITICITY_PARAMS_DB <- this has the elasticity curve parameters for all product_ids
#
#
# SPACE_ELASTICITY_VALUES_DB <- this has the calculated values (e.g. Sales) for all product_ids and possible space

source("R/utils/scalene_utilities.R")
# now creating some reference levels from which values will be selected randomly
source("conf/spaceopt_config.R")
library("reshape2")
# lets now create STORE_DB
# read the base store file
STORE_DB <- read.csv("data/spaceopt/input/store_names.csv",stringsAsFactors = F)
colnames(STORE_DB) <- c("STORE_ID","STORE_NAME")

random_state_levels <- sample(1:length(STORE_STATE_LEVELS),
                              nrow(STORE_DB),replace = T)
random_location_levels <- sample(1:length(STORE_LOCATION_LEVELS),
                                 nrow(STORE_DB), replace = T)
random_states <- sapply(random_state_levels, function(x){STORE_STATE_LEVELS[x]})
random_locations <- sapply(random_location_levels, function(x){STORE_LOCATION_LEVELS[x]})

STORE_DB$STATE <- random_states
STORE_DB$LOCATION <- random_locations

# check distribution
table(STORE_DB$STATE)
table(STORE_DB$LOCATION)

# cleanup
rm(random_location_levels)
rm(random_locations)
rm(random_state_levels)
rm(random_states)

# now lets create SPACE_ALLOCATED_DB

# I want to create a narrow frame with STORE_ID, PRODUCT_ID and ALLOCATED_SPACE columns
# However, I am struggling to find an elegant way of doing it
# For now, creating a wide data frame and then melting to a narrow data frame

# first create an empty data frame that has following columns
# STORE_ID
# PRODUCT_ID_1 (replace with actual product id with a X in beginning)
PRODUCT_DB <- read.csv("data/spaceopt/input/product_lvl3_db.csv",
                       stringsAsFactors = F,
                       colClasses = c("character","character")) # did this so that product id is read as character
product_ids <- sapply(PRODUCT_DB$product_id,
                      function(x)
                      {
                        #paste X in beginning
                        paste("X",x,sep = "")
                      })
store_ids <- unique(STORE_DB$STORE_ID)
# now create an empty data frame
SPACE_DB_WIDE <- data.frame(matrix(vector(),length(store_ids),1+length(product_ids),
                            dimnames=list(c(),c("STORE_ID",product_ids))),
                            stringsAsFactors = F)
SPACE_DB_WIDE$STORE_ID <- store_ids

# simulate allocations for all combinations of store_ids and product_ids
random_space_allocation_levels <- sample(1:length(ALLOCATED_SPACE_LEVELS),
                                         length(store_ids) * length(product_ids),
                                         replace = T)
random_space_allocation_matrix <- matrix(random_space_allocation_levels,
                                         ncol = length(product_ids))
random_space_allocation <- apply(random_space_allocation_matrix,
                                 function(x)
                                   {
                                   ALLOCATED_SPACE_LEVELS[x]
                                 },
                                 MARGIN = c(1,2))
# now put this matrix into SPACE_DB_WIDE
SPACE_DB_WIDE[,2:ncol(SPACE_DB_WIDE)] <- random_space_allocation

# cleanup
rm(random_space_allocation)
rm(random_space_allocation_levels)
rm(random_space_allocation_matrix)

# now use reshape to melt this dataframe into narrow format
# install.packages("reshape")

# Lets now create the SPACE_ELASTICITY_CURVE_DB dataset
# we will use the Gompertz model for Space Elasticity
# Gompertz model has 4 parameters
# alpha
# beta
# gamma
# delta
#
# for every product id and cluster id, we will create the model parameters

# we will first create possible solution space for each of the parameters
# and then randomly select a solution for a product_id

# possible solution space
# alpha : this is the saturation level of sales in a store for a product id
#         we expect saturation level of sales will be normal distributed
#         with values between $1,000 and $1,000,000
ALPHA_LEVEL_LB <- 1000
ALPHA_LEVEL_UB <- 1000000
ALPHA_LEVEL_MEAN <- 50000
ALPHA_LEVEL_SD <- 10000

seed_value <- 207
random_alpha_values <- sample_normal_df_with_range(lower_bound = ALPHA_LEVEL_LB,
                                                   upper_bound = ALPHA_LEVEL_UB,
                                                   mean = ALPHA_LEVEL_MEAN,
                                                   sd = ALPHA_LEVEL_SD,
                                                   n = length(product_ids),
                                                   seed = seed_value)
random_alpha_values <- round(random_alpha_values,2)
hist(random_alpha_values)

# beta: this is the minimum value possible. Keeping this as 0
BETA_LEVELS <- c(0)

random_beta_values <- rep(x = 0, times = length(product_ids))

# Gamma: I think this is the slope or ramp-up rate during growth phase
#        dont know how it should behave.
#        for now, keeping it to be normal distributed with values between 0 and 1
GAMMA_LEVELS_LB <- 0.01
GAMMA_LEVELS_UB <- 0.2
GAMMA_LEVELS_MEAN <- mean(GAMMA_LEVELS_LB, GAMMA_LEVELS_UB)
GAMMA_LEVELS_SD <- 1

random_gamma_values <- sample_normal_df_with_range(lower_bound = GAMMA_LEVELS_LB,
                                                   upper_bound = GAMMA_LEVELS_UB,
                                                   mean = GAMMA_LEVELS_MEAN,
                                                   sd = GAMMA_LEVELS_SD,
                                                   n = length(product_ids),
                                                   seed = seed_value)
random_gamma_values <- round(random_gamma_values,5)
hist(random_gamma_values)

# DELTA: this is the upper shoulder
#        it should be a function of number of items in the category
#        so that category with more items can have the upper shoulder at a higher number of mods
#        reason being that space for more items should create additional sales while space for more facing will saturate sales
#        assuming number of items is a uniform distribution, DELTA values will also be uniform distribution
#        with some lower bound and upper bound
STORE_MOD_COUNT_QUANTILES <- quantile(apply(SPACE_DB_WIDE[,-1],FUN=sum, MARGIN=1),probs = c(0,0.25,0.5,0.75,1))
DELTA_LEVELS_LB <- floor(STORE_MOD_COUNT_QUANTILES[2])
DELTA_LEVELS_UB <- ceiling(STORE_MOD_COUNT_QUANTILES[4])

# above values of delta are too high
# keeping DELTA values to be close to 0
# i think my understanding of delta is incorrect
DELTA_LEVELS_LB = 0
DELTA_LEVELS_UB = 1
random_delta_values <- sample(seq(DELTA_LEVELS_LB,DELTA_LEVELS_UB),length(product_ids), replace = T)
hist(random_delta_values)

SPACE_ELASTICITY_PARAMS_DB <- data.frame(PRODUCT_ID=product_ids,
                                  CLUSTER_ID=rep("C1",length(product_ids)),
                                  ALPHA = random_alpha_values,
                                  BETA = random_beta_values,
                                  GAMMA = random_gamma_values,
                                  DELTA = random_delta_values)

# And now add the total space allocated in a store to the STORE_DB
STORE_MODS <- data.frame(STORE_ID = SPACE_DB_WIDE$STORE_ID,
                         STORE_SPACE = apply(SPACE_DB_WIDE[,-1], FUN = sum, MARGIN = 1))
STORE_DB <- merge(x = STORE_DB,
                  y = STORE_MODS,
                  all = F)

# cleanup
rm(ALPHA_LEVEL_LB)
rm(ALPHA_LEVEL_UB)
rm(ALPHA_LEVEL_MEAN)
rm(ALPHA_LEVEL_SD)
rm(random_alpha_values)
rm(BETA_LEVELS)
rm(random_beta_values)
rm(DELTA_LEVELS_UB)
rm(DELTA_LEVELS_LB)
rm(random_delta_values)
rm(GAMMA_LEVELS_LB)
rm(GAMMA_LEVELS_UB)
rm(GAMMA_LEVELS_MEAN)
rm(GAMMA_LEVELS_SD)
rm(random_gamma_values)
rm(product_ids)
rm(store_ids)

# create SPACE_ELASTICITY_VALUES_DB

# first lets have the possible space values
POSSIBLE_SPACE_LB = 1
POSSIBLE_SPACE_UB = STORE_MOD_COUNT_QUANTILES[4]+1
possible_space_values <- seq(POSSIBLE_SPACE_LB, POSSIBLE_SPACE_UB)

# need to do transpose so that product_ids are in rows
possible_space_elasticity_values <- t(apply(SPACE_ELASTICITY_PARAMS_DB[,3:6],
                                          FUN = space_elasticity_values,
                                          MARGIN = 1,
                                          space_values = possible_space_values
                                          ))
# final dataframe will have
# Product_Id
# Cluster_Id
# All possible space values (Sxxxx, where xxxx is the possible space values)
SPACE_ELASTICITY_VALUES_DB <- data.frame(matrix(vector(),nrow(SPACE_ELASTICITY_PARAMS_DB),length(possible_space_values)+2))
colnames(SPACE_ELASTICITY_VALUES_DB) <- c("PRODUCT_ID", "CLUSTER_ID",
                                          sprintf("S%04d",possible_space_values))
SPACE_ELASTICITY_VALUES_DB$PRODUCT_ID <- SPACE_ELASTICITY_PARAMS_DB$PRODUCT_ID
SPACE_ELASTICITY_VALUES_DB$CLUSTER_ID <- SPACE_ELASTICITY_PARAMS_DB$CLUSTER_ID
SPACE_ELASTICITY_VALUES_DB[,3:ncol(SPACE_ELASTICITY_VALUES_DB)] <- possible_space_elasticity_values

rm(STORE_MODS)
rm(STORE_MOD_COUNT_QUANTILES)
rm(ALLOCATED_SPACE_LEVELS)
rm(seed_value)
rm(STORE_LOCATION_LEVELS)
rm(STORE_STATE_LEVELS)
rm(POSSIBLE_SPACE_UB)
rm(POSSIBLE_SPACE_LB)
rm(possible_space_elasticity_values)
rm(possible_space_values)



# now create ANNUAL_SALES_DB
# idea is to use the allocated space and space elasticity curve to estimate annual sales
# on top of it we will add random noise (while ensuring that the annual number is > 10000)

# read the current space allocation db
SPACE_DB_WIDE <- read.csv("data/spaceopt/input/Current_Space_Allocation.csv",
                                        stringsAsFactors = F)
names(SPACE_DB_WIDE)
# melt the dataframe into narrow format
SPACE_DB <- melt(SPACE_DB_WIDE,
                 id.vars = "STORE_ID")
names(SPACE_DB)
colnames(SPACE_DB) <- c("STORE_ID","PRODUCT_ID","ALLOCATED_SPACE")
names(SPACE_DB)

# function to calculate space elasticity curve values for product and allocated space
current_kpi_simulated <- function(product_id, current_space)
{
  # get the space elasticity params for the product_id
  match_row = which(SPACE_ELASTICITY_PARAMS_DB$PRODUCT_ID==product_id)[1]
  # make sure we take the first match row ... hopefully there will be no duplicates
  elasticity_params <- SPACE_ELASTICITY_PARAMS_DB[match_row,3:6]
  space_curve_value(current_space,elasticity_params = elasticity_params)
}

# now for each store-product id combination, run the space_elasticity_curve
ANNUAL_SALES_DB <- SPACE_DB
# ANNUAL_SALES_DB$CURRENT_KPI <- apply(SPACE_DB[,2:3],
#                                       function(x){
#                                         current_kpi_simulated(as.character(x[1]),as.numeric(x[2]))
#                                       },
#                                       MARGIN=1)

# get the space_elasticity_params
ANNUAL_SALES_DB <- merge(x=SPACE_DB,
                         y=SPACE_ELASTICITY_PARAMS_DB,
                         by = "PRODUCT_ID",
                         all.x = T)

names(ANNUAL_SALES_DB)
ANNUAL_SALES_DB$CURRENT_KPI <- apply(ANNUAL_SALES_DB[,c(3,5:8)],
                                     function(x){
                                       space_curve_value(space = x[1],
                                                         elasticity_params = x[2:5])
                                     },MARGIN = 1)
# now create random data to add to current KPI
random_error_data_kpi <- rnorm(nrow(ANNUAL_SALES_DB),mean = 0, sd = 100)
ANNUAL_SALES_DB$CURRENT_KPI <- ANNUAL_SALES_DB$CURRENT_KPI + random_error_data_kpi
# make sure ANNUAL SALES is minimum set to 10
ANNUAL_SALES_DB$CURRENT_KPI <- ifelse(ANNUAL_SALES_DB$CURRENT_KPI >= 10,
                                      ANNUAL_SALES_DB$CURRENT_KPI,
                                      10)
# remove unnecessary columns
ANNUAL_SALES_DB <- ANNUAL_SALES_DB[,c(1,2,9)]
ANNUAL_SALES_AGG_DB <- aggregate(ANNUAL_SALES_DB$CURRENT_KPI,by = list(STORE_ID=ANNUAL_SALES_DB$STORE_ID),
                                FUN = sum)
colnames(ANNUAL_SALES_AGG_DB) <- c("STORE_ID","CURRENT_KPI")

# write all the dataframes into csv
write.csv(x = SPACE_DB_WIDE, file = "data/spaceopt/input/Current_Space_Allocation.csv", row.names = F)
write.csv(x = SPACE_ELASTICITY_PARAMS_DB, file="data/spaceopt/output/Space_Elasticity_Params_DB.csv", row.names = F)
write.csv(x=SPACE_ELASTICITY_VALUES_DB, file="data/spaceopt/output/Space_Elasticity_Values_DB.csv", row.names = F)
write.csv(x = STORE_DB, file = "data/spaceopt/input/Store_DB.csv", row.names = F)
write.csv(x = ANNUAL_SALES_AGG_DB, file = "data/spaceopt/input/Current_KPI_Stores_DB.csv", row.names = F)
write.csv(x = ANNUAL_SALES_DB, file="data/spaceopt/input/Current_KPI_DB.csv", row.names = F)

rm(SPACE_ELASTICITY_VALUES_DB)
rm(SPACE_ELASTICITY_PARAMS_DB)
rm(SPACE_DB_WIDE)
rm(STORE_DB)
rm(PRODUCT_DB)
rm(ANNUAL_SALES_AGG_DB)
rm(ANNUAL_SALES_DB)
rm(random_error_data_kpi)
rm(SPACE_DB)
