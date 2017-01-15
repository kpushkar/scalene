library("lpSolve")


prefix_pk <- function(x,prefix)
{
  paste(prefix,x,sep="")
}

matrix_to_array_pk <- function(x)
{
  as.vector(t(x))
}


segment_constraint <- function(x,ncol,nrow)
{
  if(x>nrow)
   stop("PK: row index out of bound")
  else
  {
    # create a matrix with all 0
    y <- matrix(rep(0,ncol*nrow),ncol=ncol)
    # for the x row, make it all 1
    y[x,c(1:ncol)] <- 1
    matrix_to_array_pk(y)
  }
}

# NUM_OFFERS <- 4
# NUM_SEGMENTS <- 2

offer_recommendation <- function(NUM_OFFERS,NUM_SEGMENTS,MIN_PROFIT)
{
  
  OFFERS <- unlist(lapply(seq(1,NUM_OFFERS),prefix_pk,prefix="O"))
  SEGMENTS <- unlist(lapply(seq(1,NUM_SEGMENTS),prefix_pk,prefix="S"))
  
  print("Creating dummy coefficient matrices ...")
  # create coefficient for sales, profit and participation rate
  sales_coeff_matrix <- matrix(sample(1000:100,NUM_OFFERS*NUM_SEGMENTS,replace=T),nrow=NUM_SEGMENTS)
  profit_coeff_matrix <- matrix(sample(10:100,NUM_OFFERS*NUM_SEGMENTS,replace=T),nrow=NUM_SEGMENTS)
  participation_coeff_matrix <- matrix(sample(0:1,NUM_OFFERS*NUM_SEGMENTS,replace=T),nrow=NUM_SEGMENTS)
  print("Coefficient matrices created!")
  
  # lets now create the integer programming model
  #
  # objective function
  # objective 
  
  #   MIN_PROFIT <- 100
  
  
  print("Creating optimisation model ...")
  objective_fn_coeff <- matrix_to_array_pk(sales_coeff_matrix)
  
  ## lets make the constraint matrix
  # each row of the matrix is for 1 constraint
  # we will have 1 row each for each of the customer segment
  # and then 1 row for the Profit constraint
  
  constraint_fn_coeff <- matrix(data=NA,nrow=NUM_SEGMENTS+1,ncol=NUM_SEGMENTS*NUM_OFFERS)
  
  constraint_fn_coeff[1,]<-matrix_to_array_pk(profit_coeff_matrix)
  # now lets create 1 row each for each of the customer segments
  for(i in 1:NUM_SEGMENTS)
  {
    print(paste("Creating constraint for:",i))
    constraint_fn_coeff[i+1,] <- segment_constraint(i,ncol=NUM_OFFERS,nrow=NUM_SEGMENTS)
  }
  
  constraint_fn_value <- c(MIN_PROFIT,rep(1,NUM_SEGMENTS))
  constraint_direction <- c(">=",rep("=",NUM_SEGMENTS))
  
  print("Optimising model created!")
  
  print("Running optimisation now ...")
  
  offer_optimised <- lp(direction="max",objective.in=objective_fn_coeff,const.mat=constraint_fn_coeff,const.dir=constraint_direction,const.rhs=constraint_fn_value,all.bin=TRUE)
  
  print("Optimisation achieved!")
  
  print("Writing the output file ...")
  
  # now write the solution
  offer_recommendation <- data.frame(data=matrix(offer_optimised$solution,ncol=NUM_OFFERS,byrow=TRUE))
  colnames(offer_recommendation) <- OFFERS
  offer_recommendation <- cbind(Segment=SEGMENTS,offer_recommendation)
  
  write.csv(x=offer_recommendation,file="Offer Recommendation.csv",row.names=FALSE)
  
  sales_uplift_df <- data.frame(data=sales_coeff_matrix)
  colnames(sales_uplift_df) <- OFFERS
  sales_uplift_df <- cbind(Segment=SEGMENTS,sales_uplift_df)
  write.csv(x=sales_uplift_df,file="Sales Uplift Coefficient.csv",row.names=FALSE)
  
  profit_uplift_df <- data.frame(data=profit_coeff_matrix)
  colnames(profit_uplift_df) <- OFFERS
  profit_uplift_df <- cbind(Segment=SEGMENTS,profit_uplift_df)
  write.csv(x=profit_uplift_df,file="Profit Uplift Coefficient.csv",row.names=FALSE)
  
  print("All done!")
  print("=================")
  print("Output files:")
  print("Offer Recommendation.csv")
  print("Sales Uplift Coefficient.csv")
  print("Profit Uplift Coefficient.csv")
  print("=================")
}