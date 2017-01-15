# version 2 of space allocation
# this time using a completely simplistic approach of space allocation
# and then compare with the non-linear optimisation answer

# logic is as follows
# while space left to allocate
#   allocate space to product with greatest incremental sales
#   update next incremental sales value for the product
# end

source("R/utils/scalene_utilities.R")
# SPACE_ELASTICITY_VALUES_DB <- read.csv("data/spaceopt/output/Space_Elasticity_Values_DB.csv",
                                       # stringsAsFactors = F)
SPACE_ELASTICITY_PARAMS_DB <- read.csv("data/spaceopt/output/Space_Elasticity_Params_DB.csv",
                                       stringsAsFactors = F)

# lets have only few product ids at the moment
# SPACE_ELASTICITY_PARAMS_DB <- SPACE_ELASTICITY_PARAMS_DB[5:7,]
# SPACE_ELASTICITY_VALUES_DB <- SPACE_ELASTICITY_VALUES_DB[5:7,]
#

# TOTAL_SPACE = 10

initialize_space_allocation <- function(total_space)
{
  data.frame(SPACE_UNIT=seq(1,total_space,1),
                              PRODUCT_ID=NA,
                              INCREMENTAL_KPI=NA)
}


# function to check if all space is allocated
# returns TRUE if all space is allocated
# else FALSE
all_space_allocated <- function()
{
  if(length(which(is.na(ALLOCATED_SPACE$PRODUCT_ID))) > 0)
    FALSE
  else
    TRUE
}

# update the incremental sales for a product id and currently allocated space
# retrieve elasticity params from elasticity_params_db
get_incremental_kpi <- function(product_id, allocated_space,increment=1)
{
  # get the elasticity curve parameters for product_id
  match_row = which(SPACE_ELASTICITY_PARAMS_DB$PRODUCT_ID==product_id)
  # print(sprintf("Product id found in row: %d", match_row))
  a = SPACE_ELASTICITY_PARAMS_DB$ALPHA[match_row]
  b = SPACE_ELASTICITY_PARAMS_DB$BETA[match_row]
  g = SPACE_ELASTICITY_PARAMS_DB$GAMMA[match_row]
  d = SPACE_ELASTICITY_PARAMS_DB$DELTA[match_row]
  # if allocated_space = NA, then make it 0
  if(is.na(allocated_space))
    allocated_space = 0
  # find KPI with current space allocation
  if(allocated_space==0)
    current_kpi = 0
  else
    current_kpi = space_curve_value(allocated_space,c(a,b,g,d))
  # doing this if-else because space_curve_value is non-zero for x=0
  next_kpi = space_curve_value(allocated_space+increment,c(a,b,g,d))
  # print(sprintf("Current kpi: %s and next kpi is %s",current_kpi,next_kpi))
  # if for some reason next_kpi is less than current_kpi, then we will keep incremental to 0
  max(next_kpi - current_kpi,0)
}

initialize_incremental_kpi <- function()
{
  INCREMENTAL_KPI_DF <- data.frame(PRODUCT_ID=SPACE_ELASTICITY_PARAMS_DB$PRODUCT_ID,
                                ALLOCATED_SPACE_COUNT=rep(x = 0, times = nrow(SPACE_ELASTICITY_PARAMS_DB)),
                                INCREMENTAL_KPI=sapply(SPACE_ELASTICITY_PARAMS_DB$PRODUCT_ID,
                                                       get_incremental_kpi,
                                                       allocated_space = 0),
                                stringsAsFactors = F)
  INCREMENTAL_KPI_DF
}


# initialize the incremental KPI dataframe
ALLOCATED_SPACE <- initialize_space_allocation(TOTAL_SPACE)
INCREMENTAL_KPI <- initialize_incremental_kpi()

# lets start to allocate
while(!all_space_allocated())
{
  space_to_allocate <- min(which(is.na(ALLOCATED_SPACE$PRODUCT_ID)))
  # print(sprintf("We are allocating space unit %d",space_to_allocate))

  # find the product_id with maximum incremental sales
  max_incremental_kpi <- max(INCREMENTAL_KPI$INCREMENTAL_KPI)
  # if more than 1 match, then pick the first one
  max_row <- which(INCREMENTAL_KPI$INCREMENTAL_KPI==max_incremental_kpi)[1]

  # TODO: need some error checking
  max_pid <- INCREMENTAL_KPI$PRODUCT_ID[max_row]

  # allocate space to this product id
  ALLOCATED_SPACE$PRODUCT_ID[space_to_allocate] = max_pid
  ALLOCATED_SPACE$INCREMENTAL_KPI[space_to_allocate] = max_incremental_kpi
  # update the incremental KPI for this product
  current_space_allocation <- INCREMENTAL_KPI$ALLOCATED_SPACE_COUNT[max_row]+1
  next_incremental_kpi <- get_incremental_kpi(max_pid,current_space_allocation)
  INCREMENTAL_KPI$ALLOCATED_SPACE_COUNT[max_row] = current_space_allocation
  INCREMENTAL_KPI$INCREMENTAL_KPI[max_row] = next_incremental_kpi
}
print(sprintf("Maximum possible KPI is $ %s from %d space",
              format(sum(ALLOCATED_SPACE$INCREMENTAL_KPI),big.mark = ",",scientific = F),TOTAL_SPACE))

# print("Space allocation is as follows")
# print(table(ALLOCATED_SPACE$PRODUCT_ID))

### cleanups
rm(current_space_allocation)
rm(max_incremental_kpi)
rm(max_pid)
rm(max_row)
rm(next_incremental_kpi)
rm(space_to_allocate)

