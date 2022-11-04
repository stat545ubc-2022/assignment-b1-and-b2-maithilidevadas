Assignment B1
================
Maithili Devadas
2022-11-04

``` r
#Loading packages 
library(devtools)
```

    ## Loading required package: usethis

``` r
library(usethis)
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:devtools':
    ## 
    ##     test_file

``` r
library(datateachr)
library(palmerpenguins)
```

\***Exercise 1 and 2: Making and documenting a function**

``` r
#For the cancer dataset I previously worked on, I repeated the commands for making summary statistic tables multiple times for different variables. At the time, I did not know how to create a function. Thus, I would like to create a function that calculates a few summary statistics for example, the mean and the median for variables in my dataset.

#The function here first recognizes whether the input is numeric or not. If it is not numeric, R #returns with an error. If the input is numeric, the function computes its mean and median

MeanandMedian<- function(data) {
  if(!is.numeric(data)){
    stop("The following function cannot be executed as the input is not numeric")
  }
 mean<-mean(data,na.rm = TRUE)
 median<- median(data,na.rm = TRUE)
 print(c(mean,median))
}

#Documenting the function using roxygen() syntax as instructed. 
#'Summary Statistics: Mean and Mode
#'The function created above returns the mean and median of a numeric variable to the user.
#'@param data is the input we will provide for the function to run on. 
#'@param mean is the mean of the numeric variable.
#'@param median is the median of the numeric variable.
#'@return the print command that returns the value of mean and median for the chosen numeric variable or if the input is not numeric, it will return an error as defined.
```

**Exercise 3: Include examples**

**Example 1**

``` r
#Choosing data from different datasets to check if our function is running. As stated in the function, the first value of the output is mean and the second is the median ie. print(c(mean,median))

#First dataset
vancouver_trees
```

    ## # A tibble: 146,611 × 20
    ##    tree_id civic_number std_st…¹ genus…² speci…³ culti…⁴ commo…⁵ assig…⁶ root_…⁷
    ##      <dbl>        <dbl> <chr>    <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1  149556          494 W 58TH … ULMUS   AMERIC… BRANDON BRANDO… N       N      
    ##  2  149563          450 W 58TH … ZELKOVA SERRATA <NA>    JAPANE… N       N      
    ##  3  149579         4994 WINDSOR… STYRAX  JAPONI… <NA>    JAPANE… N       N      
    ##  4  149590          858 E 39TH … FRAXIN… AMERIC… AUTUMN… AUTUMN… Y       N      
    ##  5  149604         5032 WINDSOR… ACER    CAMPES… <NA>    HEDGE … N       N      
    ##  6  149616          585 W 61ST … PYRUS   CALLER… CHANTI… CHANTI… N       N      
    ##  7  149617         4909 SHERBRO… ACER    PLATAN… COLUMN… COLUMN… N       N      
    ##  8  149618         4925 SHERBRO… ACER    PLATAN… COLUMN… COLUMN… N       N      
    ##  9  149619         4969 SHERBRO… ACER    PLATAN… COLUMN… COLUMN… N       N      
    ## 10  149625          720 E 39TH … FRAXIN… AMERIC… AUTUMN… AUTUMN… N       N      
    ## # … with 146,601 more rows, 11 more variables: plant_area <chr>,
    ## #   on_street_block <dbl>, on_street <chr>, neighbourhood_name <chr>,
    ## #   street_side_name <chr>, height_range_id <dbl>, diameter <dbl>, curb <chr>,
    ## #   date_planted <date>, longitude <dbl>, latitude <dbl>, and abbreviated
    ## #   variable names ¹​std_street, ²​genus_name, ³​species_name, ⁴​cultivar_name,
    ## #   ⁵​common_name, ⁶​assigned, ⁷​root_barrier

``` r
#Computing the mean and median of the variable 'diameter' using our function MeanandMedian
Example1 <- MeanandMedian(vancouver_trees$diameter)
```

    ## [1] 11.49016  9.00000

**Example 2**

``` r
##Second dataset 
apt_buildings
```

    ## # A tibble: 3,455 × 37
    ##       id air_c…¹ ameni…² balco…³ barri…⁴ bike_…⁵ exter…⁶ fire_…⁷ garba…⁸ heati…⁹
    ##    <dbl> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 10359 NONE    Outdoo… YES     YES     0 indo… NO      YES     YES     HOT WA…
    ##  2 10360 NONE    Outdoo… YES     NO      0 indo… NO      YES     YES     HOT WA…
    ##  3 10361 NONE    <NA>    YES     NO      Not Av… NO      YES     NO      HOT WA…
    ##  4 10362 NONE    <NA>    YES     YES     Not Av… YES     YES     NO      HOT WA…
    ##  5 10363 NONE    <NA>    NO      NO      12 ind… NO      YES     NO      HOT WA…
    ##  6 10364 NONE    <NA>    NO      NO      Not Av… <NA>    YES     NO      HOT WA…
    ##  7 10365 NONE    <NA>    NO      YES     Not Av… NO      YES     NO      HOT WA…
    ##  8 10366 CENTRA… Indoor… YES     NO      Not Av… NO      YES     YES     HOT WA…
    ##  9 10367 NONE    <NA>    YES     YES     0 indo… NO      YES     YES     ELECTR…
    ## 10 10368 NONE    Indoor… YES     YES     Not Av… NO      YES     NO      HOT WA…
    ## # … with 3,445 more rows, 27 more variables: intercom <chr>,
    ## #   laundry_room <chr>, locker_or_storage_room <chr>, no_of_elevators <dbl>,
    ## #   parking_type <chr>, pets_allowed <chr>, prop_management_company_name <chr>,
    ## #   property_type <chr>, rsn <dbl>, separate_gas_meters <chr>,
    ## #   separate_hydro_meters <chr>, separate_water_meters <chr>,
    ## #   site_address <chr>, sprinkler_system <chr>, visitor_parking <chr>,
    ## #   ward <chr>, window_type <chr>, year_built <dbl>, year_registered <dbl>, …

``` r
#Example2:Computing the mean and median of the variable 'no_of_elevators' using our function MeanandMedian
Example2 <- MeanandMedian(apt_buildings$no_of_elevators)
```

    ## [1] 1.208406 1.000000

**Example 3 and 4**

``` r
##Third dataset
penguins
```

    ## # A tibble: 344 × 8
    ##    species island    bill_length_mm bill_depth_mm flipper_…¹ body_…² sex    year
    ##    <fct>   <fct>              <dbl>         <dbl>      <int>   <int> <fct> <int>
    ##  1 Adelie  Torgersen           39.1          18.7        181    3750 male   2007
    ##  2 Adelie  Torgersen           39.5          17.4        186    3800 fema…  2007
    ##  3 Adelie  Torgersen           40.3          18          195    3250 fema…  2007
    ##  4 Adelie  Torgersen           NA            NA           NA      NA <NA>   2007
    ##  5 Adelie  Torgersen           36.7          19.3        193    3450 fema…  2007
    ##  6 Adelie  Torgersen           39.3          20.6        190    3650 male   2007
    ##  7 Adelie  Torgersen           38.9          17.8        181    3625 fema…  2007
    ##  8 Adelie  Torgersen           39.2          19.6        195    4675 male   2007
    ##  9 Adelie  Torgersen           34.1          18.1        193    3475 <NA>   2007
    ## 10 Adelie  Torgersen           42            20.2        190    4250 <NA>   2007
    ## # … with 334 more rows, and abbreviated variable names ¹​flipper_length_mm,
    ## #   ²​body_mass_g

``` r
#Example3:Computing the mean and median of the variable 'bill_length_mm' using our function MeanandMedian
Example3<- MeanandMedian(penguins$bill_length_mm)
```

    ## [1] 43.92193 44.45000

``` r
#Example4: Returning an error
#Writing as comment since the Rmd file was not knitting with the error.
#Example4<- MeanandMedian(penguins$island)
```

**Exercise4: Test the function**

``` r
#Using testthat() and expect_equal(), we expect the function to return the same output as Example3
test_that("mean and median of variable", {
expect_equal(Example3, MeanandMedian(penguins$bill_length_mm)) })
```

    ## [1] 43.92193 44.45000
    ## Test passed 😀

``` r
 #Using testthat() and expect_error() for an input other than numeric.
test_that("Return an error for non-numeric data ", {
expect_error (MeanandMedian(penguins$island))})
```

    ## Test passed 🎊

``` r
#Using testthat() and expect_error we check for vector of length 0.
test_that("Return an error for vector of length 0 ", {
expect_error (MeanandMedian(penguins$bill_length_mm, numeric(0)))})
```

    ## Test passed 🥳
