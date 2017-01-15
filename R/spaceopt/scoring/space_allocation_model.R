# now lets run the space_allocation_loop for all possible space
# and then use it for our graphical interface

source("R/utils/scalene_utilities.R")
source("R/spaceopt/scoring/space_allocation_loop_FN.R")

# let's read the store space info
STORE_DB <- read.csv("data/spaceopt/input/Store_DB.csv",
                     stringsAsFactors = F)
names(STORE_DB)
fivenum(STORE_DB$STORE_SPACE)

POSSIBLE_SPACE_LB = min(STORE_DB$STORE_SPACE)-5
POSSIBLE_SPACE_UB = max(STORE_DB$STORE_SPACE)+10

STORE_CURRENT_KPI <- read.csv("data/spaceopt/input/Current_KPI_Stores_DB.csv",
                              stringsAsFactors = F)
names(STORE_CURRENT_KPI)


# for each of the possible scenarios create the most efficient space allocation
MAX_POSSIBLE_KPI <- data.frame(SPACE_COUNT=seq(POSSIBLE_SPACE_LB,POSSIBLE_SPACE_UB))


MAX_POSSIBLE_KPI$OPT_KPI <- unlist(sapply(MAX_POSSIBLE_KPI$SPACE_COUNT,
                                          optimise_kpi)[1,])
names(MAX_POSSIBLE_KPI)

STORE_KPI_COMP <- merge(x = STORE_DB,
                        y = STORE_CURRENT_KPI)
names(STORE_KPI_COMP)

STORE_KPI_COMP <- merge(x = STORE_KPI_COMP,
                        y = MAX_POSSIBLE_KPI,
                        by.x = "STORE_SPACE",
                        by.y = "SPACE_COUNT",
                        all.x = T)
names(STORE_KPI_COMP)


# lets create some graphs
#
library("ggplot2")

# compare CURRENT_KPI and OPT_KPI for each store
p<-ggplot(data = STORE_KPI_COMP) + geom_point(aes(x=CURRENT_KPI,y=OPT_KPI))+
  ggtitle("Current vs Optimal KPI for stores")

ggsave(filename = "data/spaceopt/output/Compare_Optimised_KPI_1.png",
       plot = p)

library("reshape2")
graph.data <- melt(STORE_KPI_COMP[,c(1,2,4,6,7)], id.vars = c("STORE_SPACE",
                                                              "STORE_ID",
                                                              "STATE"))
names(graph.data)
colnames(graph.data) <- c("STORE_SPACE","STORE_ID","STATE","KPI_TYPE","KPI")

p <- ggplot(data = graph.data) + geom_bar(aes(x=STORE_ID,y=KPI,fill=KPI_TYPE),
                                     stat="identity",position="dodge") + facet_grid(STATE~.) +
  ggtitle("Current vs Optimal KPI by State")

ggsave(filename = "data/spaceopt/output/Compare_Optimised_KPI.png",
       plot = p)

p <- ggplot(data = MAX_POSSIBLE_KPI) + geom_bar(aes(x=SPACE_COUNT,y=OPT_KPI),stat="identity") +
  ggtitle("Optimised KPI vs Space Count")

ggsave(filename = "data/spaceopt/output/Opt_KPI_Space.png",
       plot = p)

# write the csvs
write.csv(x = MAX_POSSIBLE_KPI,file = "data/spaceopt/output/Optimised_KPI_DB.csv", row.names = F)
write.csv(x = STORE_KPI_COMP, file = "data/spaceopt/output/Optimised_KPI_Store_DB.csv", row.names = F)

rm(graph.data)
rm(MAX_POSSIBLE_KPI)
rm(STORE_CURRENT_KPI)
rm(STORE_DB)
rm(STORE_KPI_COMP)
rm(p)
rm(POSSIBLE_SPACE_UB)
rm(POSSIBLE_SPACE_LB)
