# check simulated data to see if there are any issues
SPACE_ELASTICITY_PARAMS_DB <- read.csv("data/spaceopt/output/Space_Elasticity_Params_DB.csv",
                                       stringsAsFactors = F)
SPACE_ELASTICITY_VALUES_DB <- read.csv("data/spaceopt/output/Space_Elasticity_Values_DB.csv",
                                       stringsAsFactors = F)

library(reshape2)

# melt SPACE_ELASTICITY_VALUES into narrow format
SPACE_ELASTICITY_VALUES.m <- melt(SPACE_ELASTICITY_VALUES_DB,
                                  id.vars = c("PRODUCT_ID","CLUSTER_ID"))
names(SPACE_ELASTICITY_VALUES.m)
colnames(SPACE_ELASTICITY_VALUES.m) <- c("PRODUCT_ID",
                                         "CLUSTER_ID",
                                         "SPACE",
                                         "KPI")
# change class for the columns
sapply(1:ncol(SPACE_ELASTICITY_VALUES.m),class)
SPACE_ELASTICITY_VALUES.m$PRODUCT_ID <- as.character(SPACE_ELASTICITY_VALUES.m$PRODUCT_ID)
SPACE_ELASTICITY_VALUES.m$CLUSTER_ID <- (as.character(SPACE_ELASTICITY_VALUES.m$CLUSTER_ID))
SPACE_ELASTICITY_VALUES.m$SPACE_COUNT <- as.numeric(gsub(pattern = "S",
                                                         replacement = "",
                                                         x = SPACE_ELASTICITY_VALUES.m$SPACE))



library(ggplot2)
ggplot(data=SPACE_ELASTICITY_VALUES.m[which(SPACE_ELASTICITY_VALUES.m$SPACE_COUNT<=100),]) + geom_line(aes(x=SPACE_COUNT,
                                                       y=KPI,
                                                       color=PRODUCT_ID)) + theme(legend.position="none")
