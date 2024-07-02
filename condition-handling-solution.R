# CONDITION HANDLING -- SOLUTIONS


# Calculate the square root of the sum of two numbers
sqrt_sum <- function(x, y){
  sqrt(sum(x, y))
}
sqrt_sum(3, 6)


## Rewrite sqrt_sum() and use stop() to produce a fatal error any time
## the sum of the two numbers is negative
sqrt_sum <- function(x, y){
  if(sum(x, y) < 0) 
    stop("The sum of these numbers is negative.")
  sqrt(x + y)
}
sqrt_sum(3, -6)


## Rewrite sqrt_sum() to return a missing value (`NA`) any time
## the sum of the two numbers is negative and produce a warning message
## explaining why this happened
sqrt_sum <- function(x, y) {
  if(sum(x, y) < 0) {
    warning("The sum of these numbers is negative.")
    return(NA)
  }
  sqrt(x + y)
}
sqrt_sum(3,-6)
