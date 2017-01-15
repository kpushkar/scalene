# this is a temporary file
# to test out how the space allocation model will work

# load the space elasiticity db
SPACE_ELASTICITY_DB <- read.csv("data/spaceopt/output/Space_Elasticity_Params_DB.csv",
                                stringsAsFactors = F)
names(SPACE_ELASTICITY_DB)

my_func <- function(x, extra_variable)
{
  sprintf("Class is: %s", class(x))
  sprintf("Length is: %d", length(x))
  x[1] * x[2] + x[4] + extra_variable
}

apply(SPACE_ELASTICITY_DB[1,3:6],FUN = my_func, MARGIN = 1, extra_variable=20)

# this is great. I now know how to use apply function!!! :)

sales_function <- function (space, alpha, beta, gamma, delta)
{
  alpha + (beta - alpha)*exp(-exp(gamma*(space-delta)))
}

x = seq(0,100,1)
y = sapply(x, sales_function, alpha = 10, beta = 5, gamma = 0.08, delta = 40)
plot(y=y, x=x)

plot(y=sapply(x,sales_function, alpha=10,beta=5,gamma=-0.08,delta=40),x=x)

possible_sales <- function(model_params)
{
  # make sure there are 4 parameters
  if(length(model_params) != 4)
    "ERROR"
  a = model_params[1]
  b = model_params[2]
  g = model_params[3]
  d = model_params[4]

  # sprintf("A: %s B: %s G: %s D: %s",a,b,g,d)
  # sales_function(500,a,b,g,d)
  spaces = seq(400,600,1)
  sapply(spaces,sales_function,alpha = a, beta = b, gamma = g, delta = d)
}

plot(y=possible_sales(SPACE_ELASTICITY_DB[1,3:6]),x=seq(400,600,1))

possible_sales(SPACE_ELASTICITY_DB[1,3:6])

sales_function(500,46859,0,0.51,494)


x <- apply(SPACE_ELASTICITY_DB[,3:6], FUN = possible_sales, MARGIN = 1)

plot(y=x[,25],x=seq(400,600,1))
abline(x[,3])
