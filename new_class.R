## New Class

## Class for numeric vectors that only contain 1s and 0s (DO NOT CHANGE)
setClass(
  Class = "binary_vector",
  contains = "numeric"
)

## Validity method for 'binary_vector' -- there should only be 0s and 1s
setValidity(
  Class = "binary_vector",
  method = function(object) {
    ## Complete this function
    if (all(object %in% c(0, 1))){
      TRUE
    } else{
      "There should only be 0s and 1s."
    }
  }
)

## Coerce method from character --> binary_vector
setAs(
  from = "character",
  to = "binary_vector",
  def = function(from) {
    ## Complete this function
    binary = as.numeric(from)
    new("binary_vector", binary)
  }
)

## Adding two binary_vectors using binary arithmetic
## 1 + 1 = 0
## 1 + 0 = 1
## 0 + 1 = 1
## 0 + 0 = 0

## Generic function for adding two vectors
setGeneric(
  "add_vector",
  ## Complete the rest of the generic function definition
  function(x, y) standardGeneric("add_vector")
)

## Specific method function for adding two binary vectors
setMethod(
  "add_vector",
  ## Complete the rest of the method function definition
  c("binary_vector", "binary_vector"),
  function(x, y){
    sum = (x + y) %% 2
    new("binary_vector", sum)
  }
)


################################################################################
## Some test cases

## Create a new vector
x <- new("binary_vector", c(0, 1, 1, 0, 0))

## This should fail with an error
x <- new("binary_vector", c(0, 1, 1, 0, 2))

## This should fail with an error
x <- new("binary_vector", c(0, 1, -1, 0, 0))

## Coerce from a character vector
x <- as(c("0", "1", "1", "0", "0", "1"), "binary_vector")
y <- new("binary_vector", c(1, 1, 0, 0, 1, 0))

## Add two binary vectors
z <- add_vector(x, y)
z  ## should be c(1, 0, 1, 0, 1, 1)
