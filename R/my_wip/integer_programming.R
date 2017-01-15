# this script is to figure out how integer programming works in R

# the space allocation problem is a non-linear problem as the objective function is a non-linear function
# hence we will use nloptr package and not lpSolve
# install.packages("lpSolve")
install.packages("nloptr")
library("nloptr")

# this is a trial program
# first we will try the demo example

# Minimize f(x) = 100*(x2 - x1^2)^2 + (1-x1)^2
# without any constraint
#
# to use non-linear optimisation, we also need to create the gradient function
# gradient function is the partial derivative of objective function w.r.t each variable
# i.e. in this case partial derivative of f(x) w.r.t x1 and x2
#
# partial derivative w.r.t. x1 = 400*x1^3 - 400*x1*x2 + 2*x1 - 2
# partial derivative w.r.t. x2 = 200*(x2 - x1^2)

objective_fn <- function(x)
{
  100*(x[2] - (x[1])^2)^2 + (1-x[1])^2
}

gradient_fn <- function(x)
{
  # c(
  #   (400*x[1]^3 - 400*x[1]*x[2] + 2*x[1] -2),
  #   (200*(x[2]-(x[1])^2))
  #   )
  # )
  c(
    (-400*x[1]*(x[2]-(x[1])^2)-2*(1-x[1])),
     (200*(x[2]-(x[1])^2))
     )
}

# set initial values
x0 <- c(-1.2,1)
# set optimisation options
alg_options <- list("algorithm"="NLOPT_LD_LBFGS",
                    "xtol_rel"=1.0e-8)

# solve the problem
res <- nloptr(x0 = x0,eval_f = objective_fn, eval_grad_f = gradient_fn, opts = alg_options)
print(res)


# slightly different result from the demo, but who cares :D

# and now with constraints
#
# Minimise f(x) = sqrt(x[2])
#
# when x[2] >= 0
# -x[2] + (a1*x[1] + b1)^3 <= 0
# -x[2] + (a2*x[1] + b2)^3 <= 0

objective_fn2 <- function(x,a,b)
{
  sqrt(x[2])
}

gradient_fn2 <- function(x,a,b)
{
  c(0,
    0.5/sqrt(x[2])
    )
}

constraint_fn <- function(x,a,b)
{
  (a*x[1] + b)^3 - x[2]
}

constratint_jacobian_fn <- function(x,a,b)
{
  rbind(
    c(3*a[1]*(a[1]*x[1]+b[1])^2,-1),
    c(3*a[2]*(a[2]*x[1]+b[2])^2,-1)
  )
}

a <- c(2,-1)
b <- c(0,1)

res2 <- nloptr(x0 = c(1.234, 5.678),
               eval_f = objective_fn2,
               eval_grad_f = gradient_fn2,
               lb = c(-Inf,0),
               ub = c(Inf,Inf),
               eval_g_ineq = constraint_fn,
               eval_jac_g_ineq = constratint_jacobian_fn,
               opts = list("algorithm"="NLOPT_LD_MMA",
                           "xtol_rel"=1.0e-8,
                           "print_level"=2,
                           "check_derivatives"=TRUE,
                           "check_derivatives_print"="all"),
               a=a,
               b=b)
print(res2)


# third example with equality constraint
# refer https://artax.karlin.mff.cuni.cz/r-help/library/nloptr/html/nloptr-package.html
#
# Minimise f(x) = x1*x4*(x1+x2+x3) + x3
# when
# 25 - x1*x2*x3*x4 <= 0
# 1 <= x1,x2,x3,x4 <= 5
# x1^2 + x2^2 + x3^2 + x4^2 = 40

objective_fn3 <- function(x)
{
  return(x[1]*x[4]*(x[1]+x[2]+x[3]) + x[3])
}

gradient_fn3 <- function(x)
{
  return(c(
    (2*x[1]*x[4]+x[4]*(x[2]+x[3])),
    x[1]*x[4],
    x[1]*x[4]+1,
    x[1]*(x[1]+x[2]+x[3])
  ))
}

constraint_fn3_ineq <- function(x)
{
  return(c(25 - x[1]*x[2]*x[3]*x[4]))
}

jacobian_fn3_ineq <- function(x)
{
  return(c(
    -x[2]*x[3]*x[4],
    -x[1]*x[3]*x[4],
    -x[1]*x[2]*x[4],
    -x[1]*x[2]*x[3]
  ))
}

constraint_fn3_eq <- function(x)
{
  return(c(x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40))
}

jacobian_fn3_eq <- function(x)
{
  return(c(
    2*x[1],
    2*x[2],
    2*x[3],
    2*x[4]
  ))
}

x0 <- c(1,5,5,1)

local_opts = list("algorithm"="NLOPT_LD_MMA",
                  "xtol_rel"=1.0e-7)

res3 <- nloptr(x0 = x0,
               eval_f = objective_fn3,
               eval_grad_f = gradient_fn3,
               eval_g_ineq = constraint_fn3_ineq,
               eval_jac_g_ineq = jacobian_fn3_ineq,
               eval_g_eq = constraint_fn3_eq,
               eval_jac_g_eq = jacobian_fn3_eq,
               lb = c(1,1,1,1),
               ub = c(5,5,5,5),
               opts = list("algorithm"="NLOPT_LD_AUGLAG",
                           "xtol_rel"=1.0e-7,
                           "local_opts"=local_opts,
                           "maxeval"=1000)
               )
print(res3)


# results are slightly different. figured out .. reason was number of iterations. set this option to 1000 and results matched
# copying the exact code from example to see how it works
eval_f <- function( x ) {
  return( list( "objective" = x[1]*x[4]*(x[1] + x[2] + x[3]) + x[3],
                "gradient" = c( x[1] * x[4] + x[4] * (x[1] + x[2] + x[3]),
                                x[1] * x[4],
                                x[1] * x[4] + 1.0,
                                x[1] * (x[1] + x[2] + x[3]) ) ) )
}

# constraint functions
# inequalities
eval_g_ineq <- function( x ) {
  constr <- c( 25 - x[1] * x[2] * x[3] * x[4] )

  grad   <- c( -x[2]*x[3]*x[4],
               -x[1]*x[3]*x[4],
               -x[1]*x[2]*x[4],
               -x[1]*x[2]*x[3] )
  return( list( "constraints"=constr, "jacobian"=grad ) )
}

# equalities
eval_g_eq <- function( x ) {
  constr <- c( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )

  grad   <- c(  2.0*x[1],
                2.0*x[2],
                2.0*x[3],
                2.0*x[4] )
  return( list( "constraints"=constr, "jacobian"=grad ) )
}

# initial values
x0 <- c( 1, 5, 5, 1 )

# lower and upper bounds of control
lb <- c( 1, 1, 1, 1 )
ub <- c( 5, 5, 5, 5 )


local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                    "xtol_rel"  = 1.0e-7 )
opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
              "xtol_rel"  = 1.0e-7,
              "maxeval"   = 1000,
              "local_opts" = local_opts )

res <- nloptr( x0=x0,
               eval_f=eval_f,
               lb=lb,
               ub=ub,
               eval_g_ineq=eval_g_ineq,
               eval_g_eq=eval_g_eq,
               opts=opts)
print( res )

# got it!
