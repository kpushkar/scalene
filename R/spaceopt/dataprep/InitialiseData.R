# load the Layout Sales csv
LayoutSales <- read.csv("data/spaceopt/input/SalesByLayout.csv", stringsAsFactors = F)
# select a Layout ID and cluster
# and get the relevant sales data
LAYOUT_ID <- 5 # Cook In Sauce
CLUSTER_ID <- "MIDDLE" # Middle Cluster
# MyLayout <- subset(LayoutSales,
#                    LayoutID == LAYOUT_ID &
#                      CLUSTER == CLUSTER_ID)
# dim(MyLayout)
# 477*9

Sales_Space <- read.csv("data/spaceopt/input/LayoutSalesSpace.csv",stringsAsFactors = F)
MyLayout <- subset(Sales_Space, LayoutID == LAYOUT_ID & Cluster == CLUSTER_ID)
# create random Space allocation table

# lets just have some fun with data
library(ggplot2, lib.loc = "/usr/lib/R/library")

my_plot <- ggplot(MyLayout) + geom_point(aes(x=normSpace, y=normSales))
my_plot

my_plot <- ggplot(MyLayout) + geom_point(aes(x=normSpace,y=normFifth))
my_plot


my_eda <- function(x)
{
  # calculate name of the file
  layout_name <- as.character(x)
  # subset data
  layout_data <- subset(Sales_Space,LayoutName == x)
  layout_data[,10] <- as.factor(layout_data[,10])
  saveBiVariatePlot(data = layout_data,
                    xCol = 10,
                    yCol = 6,
                    FUN = mean,
                    filename = paste(layout_name,"-mean-Sales",sep=""))
  0
}

sapply(unique(Sales_Space$LayoutName),my_eda)


install.packages("grofit")
library("grofit")
