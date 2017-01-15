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
objective_fn <- function(x,alpha,beta,gamma,delta)
{
  -sum(alpha+(beta-alpha)*exp(-exp(gamma*(x-delta))))
}

gradient_fn <- function(x,alpha,beta,gamma,delta)
{
  (beta-alpha) * (1) * gamma * exp(-exp(gamma*(x-delta))) * exp(gamma*(x-delta))
}

constraint_fn <- function(x,alpha,beta,gamma,delta)
{
  c(sum(x)-TOTAL_SPACE)
}

jacobian_constraint <- function(x,alpha,beta,gamma,delta)
{
  rep(x = 1,times = length(x))
}


local_opts <- list("algorithm"="NLOPT_LD_MMA",
                   "xtol_rel"=1.0e-2)

opts <- list("algorithm"="NLOPT_LD_AUGLAG",
             "xtol_rel"=1.0e-2,
             "local_opts"=local_opts,
             "maxeval"=1000,
             "check_derivatives"=TRUE
             )

x0 <- rep(round((TOTAL_SPACE/2)),length(alpha))

space_allocation <- nloptr(x0 = x0,
                           eval_f = objective_fn,
                           eval_grad_f = gradient_fn,
                           eval_g_eq = constraint_fn,
                           eval_jac_g_eq = jacobian_constraint,
                           lb = rep(x=0,times=length(x0)),
                           ub = rep(x=TOTAL_SPACE,times=length(x0)),
                           opts = opts,
                           alpha = alpha,
                           beta = beta,
                           gamma = gamma,
                           delta = delta)

print(sprintf("Space allocation completed: %s",space_allocation$message))
print(sprintf("Maximum possible KPI is $ %s with %d space",
              format(-space_allocation$objective,big.mark = ",",scientific = F),TOTAL_SPACE))
# print("#### SPACE ALLOCATION #####")
# print(space_allocation$solution)

### cleanups
rm(alpha)
rm(beta)
rm(delta)
rm(gamma)
rm(local_opts)
rm(opts)
rm(x0)
