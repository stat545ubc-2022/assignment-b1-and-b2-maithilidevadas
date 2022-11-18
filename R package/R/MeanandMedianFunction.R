library(devtools)

#The function here first recognizes whether the input is numeric or not. If it is not numeric, R returns with an error. If the input is numeric, the function computes its mean and median

#' Summary Statistics: Mean and Median
#' The function created above returns the mean and median of a numeric variable to the user.
#' @param data is the **numeric vector** input we will provide for the function to run on. If this input is not numeric, the function will produce an error.
#' @return the print command that returns the value of mean and median for the chosen numeric #' variable or if the input is not numeric, it will return an error as defined.
#' @examples
#' MeanandMedian(c(12,54,23,78,33))
#' @import palmerpenguins
#' @import testthat

#' To understand examples with a dataset, we can use the palmerpenguins. 
#' #install.packages("palmerpenguins")
#' library(palmerpenguins)
#' MeanandMedian(penguins$bill_length_mm)
#' MeanandMedian(penguins$island)

#' @export
MeanandMedian<- function(data) {
  if(!is.numeric(data)){
    stop("The following function cannot be executed as the input is not numeric")
  }
  mean<-mean(data,na.rm = TRUE)
  median<- median(data,na.rm = TRUE)
  print(c(mean,median))
}

 