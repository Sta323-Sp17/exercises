# Exercise 1
#
# Below is the list of primes between 2 and 100:
# 
# 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 
# 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
# 
# If you were given the vector x = c(3, 4, 12, 19, 23, 48, 50, 61, 63, 78), write out the R code 
# necessary to print only the values of x that are not prime (without using subsetting or the %in% operator).
# 
# Your code should use nested loops to iterate through the vector of primes and x.

primes = c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 
           43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
x = c(3, 4, 12, 19, 23, 48, 50, 61, 63, 78)

## double for loop

for (value in x)
{
  matched = FALSE
  for(p in primes)
  {
    if (value == p) {
      matched = TRUE
      break
    }
  }
  
  if (!matched)
    print(value)
}


## single for loop

for (value in x)
{
  if (!any(value == primes))
    print(value)
}


## no loops

x[!x %in% primes]
