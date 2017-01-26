# configuration file to create min and max range of space for STORE-PRODUCT combination
library("reshape2")
# load the store space allocation db
CURRENT_SPACE_ALLOCATION_DB <- read.csv(file="data/spaceopt/input/Current_Space_Allocation.csv",
                                        stringsAsFactors = F)
names(CURRENT_SPACE_ALLOCATION_DB)

# get minimum space criteria based on current allocation
min_space <- function(current_allocation, min_factor=0.5)
{
  return(max(0,round(current_allocation*min_factor,0)))
}

# get maximum space criteria based on current allocation
max_space <- function(current_allocation, max_factor=1.25)
{
  return(round(current_allocation*max_factor,0))
}

MIN_SPACE_CONFIG_DB <- as.data.frame(
                        apply(CURRENT_SPACE_ALLOCATION_DB[,-1],min_space,MARGIN = c(1,2))
                        )
names(MIN_SPACE_CONFIG_DB)
MIN_SPACE_CONFIG_DB$STORE_ID <- CURRENT_SPACE_ALLOCATION_DB$STORE_ID

hist(MIN_SPACE_CONFIG_DB$X075)

MIN_SPACE_CONFIG_DB <- melt(data = MIN_SPACE_CONFIG_DB,
                            id.vars = "STORE_ID",
                            variable.name = "PRODUCT_ID",
                            value.name = "MIN_SPACE",factorsAsStrings = T)
names(MIN_SPACE_CONFIG_DB)

MAX_SPACE_CONFIG_DB <- as.data.frame(
                        apply(CURRENT_SPACE_ALLOCATION_DB[,-1],max_space, MARGIN = c(1,2))
                        )
names(MAX_SPACE_CONFIG_DB)
MAX_SPACE_CONFIG_DB$STORE_ID <- CURRENT_SPACE_ALLOCATION_DB$STORE_ID

# now put it in narrow format
MAX_SPACE_CONFIG_DB <- melt(data = MAX_SPACE_CONFIG_DB,
                            id.vars = "STORE_ID",
                            variable.name = "PRODUCT_ID",
                            value.name = "MAX_SPACE", factorsAsStrings = T)
names(MAX_SPACE_CONFIG_DB)

MIN_MAX_SPACE_CONFIG_DB <- merge(x = MIN_SPACE_CONFIG_DB,
                                 y = MAX_SPACE_CONFIG_DB,
                                 by = c("STORE_ID","PRODUCT_ID"),
                                 all = T)
names(MIN_MAX_SPACE_CONFIG_DB)

# lets check that MAX_SPACE is >= MIN_SPACE
max_check <- ifelse(MIN_MAX_SPACE_CONFIG_DB$MAX_SPACE >= MIN_MAX_SPACE_CONFIG_DB$MIN_SPACE,
                    1,0)
length(which(max_check==0))
# all good

# write out the min and max space configuration file
write.csv(x = MIN_MAX_SPACE_CONFIG_DB,
          file = "data/spaceopt/input/MIN_MAX_SPACE_CONFIG_DB.csv",
          row.names = F)
print("Min Max Space Config file written")

# cleanup the environment
rm(CURRENT_SPACE_ALLOCATION_DB)
rm(MAX_SPACE_CONFIG_DB)
rm(MIN_SPACE_CONFIG_DB)
rm(MIN_MAX_SPACE_CONFIG_DB)
rm(max_check)
rm(max_space)
rm(min_space)
