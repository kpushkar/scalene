# v1 of space allocation given sales elasticity curve
# we will use nloptr package and solve it as a non-linear optimisation
# issue is that it will provide non-integer results.

library("nloptr")
# lets load up the space elasticity curve dataset
SPACE_ELASTICITY_PARAMS_DB <- read.csv("data/spaceopt/output/Space_Elasticity_Params_DB.csv",
                                      stringsAsFactors = F)
# lets restrict to only 5 product ids
# SPACE_ELASTICITY_PARAMS_DB <- SPACE_ELASTICITY_PARAMS_DB[5:7,]

alpha <- SPACE_ELASTICITY_PARAMS_DB$ALPHA
beta <- SPACE_ELASTICITY_PARAMS_DB$BETA
gamma <- SPACE_ELASTICITY_PARAMS_DB$GAMMA
delta <- SPACE_ELASTICITY_PARAMS_DB$DELTA

###
# maximise f(x) = alpha + (beta - alpha)*exp(-exp(gamma*(x-delta))
#
###

TOTAL_SPACE=550
TOTAL_SPACE=410

# lets check if the gompertz function works with list or not
set.seed(207)
x0 <- sample(10:20,size = length(alpha)-1, replace = T)
# x0 <- c(407,1)
x0 <- c(x0,TOTAL_SPACE-sum(x0))
x0
y0 <- alpha + (beta - alpha)*exp(-exp(gamma*(x0-delta)))
y0
sum(y0)
# lets compare with space_curve_Value
yh <- sapply(1:length(alpha),
             function(x){space_curve_value(x0[x],c(alpha[x],beta[x],gamma[x],delta[x]))}
)
yh
y0-yh
sum(y0)
# cool!! working with array works!

objective_fn <- function(x,alpha,beta,gamma,delta)
{
  -sum(alpha+(beta-alpha)*exp(-exp(gamma*(x-delta))))
}


# derivative: (beta-alpha)*(-1)*gamma*exp(-exp(gamma*(x-delta)))*exp(gamma*(x-delta))
gradient_fn <- function(x,alpha,beta,gamma,delta)
{
  rbind(
  (beta[1]-alpha[1]) * (1) * gamma[1] * exp(-exp(gamma[1] * (x[1]-delta[1]))) * exp(gamma[1]*(x[1]-delta[1])),
  (beta[2]-alpha[2]) * (1) * gamma[2] * exp(-exp(gamma[2] * (x[2]-delta[2]))) * exp(gamma[2]*(x[2]-delta[2])),
  (beta[3]-alpha[3]) * (1) * gamma[3] * exp(-exp(gamma[3] * (x[3]-delta[3]))) * exp(gamma[3]*(x[3]-delta[3]))
  )
}



constraint_space <- function(x,alpha,beta,gamma,delta)
{
  c(x[1]+x[2]+x[3]-TOTAL_SPACE)
}

jacobian_constraint <- function(x,alpha,beta,gamma,delta)
{
  c(
    1,
    1,
    1
  )
}

local_opts <- list("algorithm"="NLOPT_LD_MMA",
                   "xtol_rel"=1.0e-2)

opts <- list("algorithm"="NLOPT_LD_AUGLAG",
             "xtol_rel"=1.0e-2,
             "local_opts"=local_opts,
             "maxeval"=1000,
             "check_derivatives"=TRUE,
             "check_derivatives_print"="all")
             #"print_level"=2)

space_allocation <- nloptr(x0 = x0,
                           eval_f = objective_fn,
                           eval_grad_f = gradient_fn,
                           eval_g_eq = constraint_space,
                           eval_jac_g_eq = jacobian_constraint,
                           lb = c(0.0001,0.0001,0.0001),
                           ub = c(TOTAL_SPACE,TOTAL_SPACE,TOTAL_SPACE),
                           opts = opts,
                           alpha = alpha,
                           beta = beta,
                           gamma = gamma,
                           delta = delta)

# answer looks incorrect
# but right now trying to see whether array functions can be used
gradient_fn2 <- function(x,alpha,beta,gamma,delta)
{
  (beta-alpha) * (1) * gamma * exp(-exp(gamma*(x-delta))) * exp(gamma*(x-delta))
}

constraint_fn2 <- function(x,alpha,beta,gamma,delta)
{
  c(sum(x)-TOTAL_SPACE)
}

jacobian_constraint2 <- function(x,alpha,beta,gamma,delta)
{
  rep(x = 1,times = length(x))
}

x0 <- c(1,1,498)
x0 <- rep(round((TOTAL_SPACE/2)),length(alpha))
space_allocation_2 <- nloptr(x0 = x0,
                           eval_f = objective_fn,
                           eval_grad_f = gradient_fn2,
                           eval_g_eq = constraint_fn2,
                           eval_jac_g_eq = jacobian_constraint2,
                           lb = rep(x=0,times=length(x0)),
                           ub = rep(x=TOTAL_SPACE,times=length(x0)),
                           opts = opts,
                           alpha = alpha,
                           beta = beta,
                           gamma = gamma,
                           delta = delta)

space_allocation_2$solution
-space_allocation_2$objective
# compare space_allocation and space_allocation_2
# sapply(1:length(space_allocation),function(x){space_allocation[x]==space_allocation_2[x]})
# for(i in 1:length(space_allocation))
# {
#   print(space_allocation[i])
#   print(space_allocation_2[i])
# }
# good thing that both these results are exactly the same
# so that is good!!
# now we have to figure out why did this not work

# first off, check the gradient function
# worked ... the objective function is now negative of gompertz and that had to be accounted for in gradient
# still results not ok.
# lets put a really weird starting point
# trying x0 <- c(1,1,498)
# this gave some answer .. looks weird though
# answer became 0,0,550
# lets try with 5,5,5
x0 <- c(5,5,5)
# result: 550,0,0
x0 <- c(10,10,10)
TOTAL_SPACE=10

space_allocation_2 <- nloptr(x0 = x0,
                             eval_f = objective_fn,
                             eval_grad_f = gradient_fn2,
                             eval_g_eq = constraint_fn2,
                             eval_jac_g_eq = jacobian_constraint2,
                             lb = rep(x=0,times=length(x0)),
                             ub = rep(x=TOTAL_SPACE,times=length(x0)),
                             opts = opts,
                             alpha = alpha,
                             beta = beta,
                             gamma = gamma,
                             delta = delta)

print(space_allocation_2)
space_allocation_2$solution
-space_allocation_2$objective

# lets manually check the result
SPACE_ELASTICITY_VALUES_DB <- read.csv("data/spaceopt/output/Space_Elasticity_Values_DB.csv",
                                       stringsAsFactors = F)
# SPACE_ELASTICITY_VALUES_DB <- SPACE_ELASTICITY_VALUES_DB[5:7,]


# something is wrong with objective function
space_calc_fn2 <- function(x,alpha,beta,gamma,delta)
{
  (alpha+(beta-alpha)*exp(-exp(gamma*(x-delta))))
}

space_calc_fn2(407,alpha[1],beta[1],gamma[1],delta[1])
