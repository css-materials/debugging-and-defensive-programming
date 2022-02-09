# Calculate the square root of the sum of two numbers
sqrt_sum <- function(x, y){
  sqrt(sum(x, y))
}

sqrt_sum(3, 6)

## Rewrite sqrt_sum() to produce a fatal error any time
## the sum of the two numbers is negative
sqrt_sum <- function(x, y){
  if(sum(x, y) < 0) stop("The sum of these numbers is negative.")
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

# Write an iterative operation that safely calculates the maximum value
# for every column in rcfss::gun_deaths
library(tidyverse)
library(rcfss)

gun_deaths

## for loop
output <- vector("list", ncol(gun_deaths))
safe_max <- safely(max) # cannot pass na.rm parameter here

for(i in seq_along(gun_deaths)){
  output[[i]] <- safe_max(gun_deaths[[i]], na.rm = TRUE)
}

str(output)

## map
gun_deaths %>%
  map(safe_max, na.rm = TRUE)

# Perform the same operation as above, but this time replace
# error messages with a missing value (`NA`)

## for loop
output <- vector("list", ncol(gun_deaths))
possible_max <- possibly(max, NA)

for(i in seq_along(gun_deaths)){
  output[[i]] <- possible_max(gun_deaths[[i]], na.rm = TRUE)
}

str(output)

## map
gun_deaths %>%
  map(possible_max, na.rm = TRUE)

### same thing without predefining the adverb
gun_deaths %>%
  map(possibly(max, NA), na.rm = TRUE)
