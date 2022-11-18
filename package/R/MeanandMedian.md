Making a package
================
Maithili Devadas
2022-11-17

``` r
library(devtools)
```

    ## Loading required package: usethis

\#The function here first recognizes whether the input is numeric or
not. If it is not numeric, R returns with an error. If the input is
numeric, the function computes its mean and median

``` r
#' Summary Statistics: Mean and Mode
#' The function created above returns the mean and median of a numeric variable to the user.
#' @param data is the input we will provide for the function to run on. 
#' @param mean is the mean of the numeric variable.
#' @param median is the median of the numeric variable.
#' @return the print command that returns the value of mean and median for the chosen numeric #' variable or if the input is not numeric, it will return an error as defined.
#' @examples
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
```

.
