# Create Table One Describing Baseline Characteristics

Create a table summarizing continuous, categorical and dichotomous
variables, optionally stratified by one or more variables, while
performing adequate statistical tests.

## Usage

``` r
tOne(
  x, groups = NA, add.length = TRUE,
  colnames = NULL, vnames = NULL, total = TRUE,
  align = "\\l", FUN = NULL, TEST = NULL,
  intref = "high",
  fmt = list(abs = "abs.sty", num = "num.sty", per = "per.sty",
             pval = style(fmt = "*", naForm = "   "))
)

# S3 method for class 'tOne'
print(x, ...)

# S3 method for class 'tOne'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x:

  a tOne object to subset

- groups:

  the grouping variable

- add.length:

  logical. If set to `TRUE` (default), a row with the group sizes will
  be inserted as first row of the table.

- colnames:

  a vector of column names for the result table

- vnames:

  a vector of variable names to be placed in the first column instead of
  the real names

- total:

  logical (default `TRUE`), defines whether the results should also be
  displayed for the whole, ungrouped variable

- align:

  the character on whose position the strings will be aligned. Left
  alignment can be requested by setting `sep = "\l"`, right alignment by
  `"\r"` and center alignment by `"\c"`. Mind the backslashes, as if
  they are omitted, strings would be aligned to the **character** **l**,
  **r** or **c** respectively. Default value is `"\l"`, thus left
  alignment.

- FUN:

  the function to be used as location and dispersion measure for numeric
  (including integer) variables (`mean`/`sd` is default, alternatives as
  `median`/`IQR` are possible by defining a function). See examples.

- TEST:

  a list of functions to be used to test the variables. Must be named as
  `"num"`, `"cat"` and `"dich"` and be defined as function with
  arguments `(x, g)`, generating something similar to a p-value. Use
  `TEST=NA` to suppress test. (See examples.)

- intref:

  one out of `"high"` (default), `"low"` or `"both"`, defining which
  value of a dichotomous variable should be reported. Usually this will
  be `1` or `TRUE`. Setting it to `"low"` will report the lower value
  `0` or `FALSE`, `"both"` reports the variable as a categorical one
  with all its levels. Dichotomous factors are treated the same way,
  `"high"` reporting the last and `"low"` the first level.

- fmt:

  fm codes for absolute, numeric and percentage values, and for the
  p-values of the tests

- ...:

  further parameters (not used here)

- i:

  rowindex

- j:

  columnindex

- drop:

  drop the structure in case of total reduction

## Value

a character matrix of class `tOne`

## Details

In research the characteristics of study populations are often
characterised through some kind of a "Table 1", containing descriptives
of the used variables, as mean/standard deviation for continuous
variables, and proportions for categorical variables. In many cases, a
comparison is made between groups within the framework of the scientific
question.

![Table 1](figures/tOne.png)

Creating such a table can be very time consuming and there's a need for
a flexible function that helps us to solve the task. `tOne()` is
designed to be easily used with sensible defaults, and yet flexible
enough to allow free definition of the essential design elements.

This is done by breaking down the descriptive task to three types of
variables: quantitative (numeric, integer), qualitative (factor,
characters) and dichotomous variables (the latter having exactly two
values or levels). Depending on the variable type, the descriptives and
the according sensible tests are chosen. By default mean/sd are chosen
to describe numeric variables.


      FUN = function(x)
              gettextf("%s (%s)",
                       fm(mean(x, na.rm = TRUE), fmt = fmt$num),
                       fm(sd(x, na.rm = TRUE), fmt = fmt$num))

Their difference is tested with the Kruskal-Wallis test. For categorical
variables the absolute and relative frequencies are calculated and
tested with a chi-square test.  
The tests can be changed with the argument `TEST`. These must be
organised as list containing elements named `"num"`, `"cat"` and
`"dich"`. Each of them must be a function with arguments `(x, g)`,
returning something similar to a p-value.


      TEST = list( num = list(fun = function(x, g){
          summary(aov(x ~ g))\verb{[[1]][1, "Pr(>F)"]}}, lbl = "ANOVA"),
        cat = list(fun = function(x, g){
          chisq.test(table(x, g))$p.val}, lbl = "Chi-Square test"),
        dich = list(fun = function(x, g){
          fisher.test(table(x, g))$p.val}, lbl = "Fisher exact test")
      ) 

The legend text of the test, which is appended to the table together
with the significance codes, can be set with the variable `lbl`.

Great importance was attached to the free definition of the number fms.
By default, the optionally definable fm templates of **DescToolsX** are
used. Deviations from this can be freely passed as arguments to the
function. fms can be defined for integers, floating point numbers,
percentages and for the p-values of statistical tests. All options of
the function
[`fm()`](https://andrisignorell.github.io/pharos/reference/fm.html) are
available and can be provided as a list. See examples which show several
different implementations.


      fmt = list(abs  = "abs.sty",
                 num  = "num.sty",
                 per  = "per.sty",
                 pval = style(fmt = "*", naForm = "   ")
                 ) 

Several tables can be appended using
[`appendX()`](https://andrisignorell.github.io/bedrock/reference/appendX.html).
This can be useful, if e.g. the `mean/sd` AND `median/IQR` should be
displayed together. Another use case is to introduce a delimiter row.

The function returns a character matrix as result, which can easily be
subset or combined with other matrices. An interface for `toWrd()` is
available such that the matrix can be transferred to MS-Word. Both font
and alignment are freely selectable in the Word table.

## See also

[`appendX()`](https://andrisignorell.github.io/bedrock/reference/appendX.html)

Other frequency: [`expFreq()`](expFreq.md), [`freq()`](freq.md),
[`freq2D()`](freq2D.md), [`percTable()`](percTable.md)

## Examples

``` r

opt <- options(scipen = 8)

# define some special fms for count data, percentages and numeric results
# (those will be supported by tOne)
abs.sty <- style(digits = 0, bigMark = "'")   # counts
per.sty <- style(digits = 1, fmt = "%")        # percentages
num.sty <- style(digits = 1, bigMark = "'")   # numeric

tOne(x = Pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")],
  groups = Pizza$quality)
#> 
#> var                total              low                medium             high                                 
#> n                  1008               156 (15.5%)        356 (35.3%)        496 (49.2%)                          
#> temperature        47.937 (9.938)     32.874 (7.772)     45.640 (7.387)     53.604 (6.474)     *** ¹             
#> delivery_min       25.653 (10.843)    33.925 (11.742)    26.522 (10.113)    22.615 (9.497)     *** ¹             
#> driver                                                                                         *** ³             
#> Butcher            79 ( 7.9%)         10 ( 6.5%)         36 (10.1%)         33 ( 6.7%)                           
#> Carpenter          225 (22.4%)        59 (38.1%)         90 (25.4%)         76 (15.4%)                           
#> Carter             196 (19.5%)        11 ( 7.1%)         72 (20.3%)         113 (22.9%)                          
#> Farmer             94 ( 9.4%)         10 ( 6.5%)         26 ( 7.3%)         58 (11.7%)                           
#> Hunter             130 (12.9%)        8 ( 5.2%)          43 (12.1%)         79 (16.0%)                           
#> Miller             109 (10.9%)        16 (10.3%)         35 ( 9.9%)         58 (11.7%)                           
#> Taylor             171 (17.0%)        41 (26.5%)         53 (14.9%)         77 (15.6%)                           
#> wine_ordered (= 1) 161 (16.1%)        32 (20.8%)         63 (17.9%)         66 (13.4%)         .   ³             
#> ---
#> ¹) Kruskal-Wallis test, ²) Fisher exact test, ³) Chi-Square test
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#> 

# the same but no groups now...
tOne(x = Pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")])
#> 
#> var                total             
#> n                  1209              
#> temperature        47.937 (9.938)    
#> delivery_min       25.653 (10.843)   
#> driver                               
#> Butcher            96 ( 8.0%)        
#> Carpenter          272 (22.6%)       
#> Carter             234 (19.4%)       
#> Farmer             117 ( 9.7%)       
#> Hunter             156 (13.0%)       
#> Miller             125 (10.4%)       
#> Taylor             204 (16.9%)       
#> wine_ordered (= 1) 187 (15.6%)       
#> 

# define median/IQR as describing functions for the numeric variables
tOne(iris[, -5], iris[, 5],
  FUN = function(x) {
    gettextf("%s / %s",
      fm(median(x, na.rm = TRUE), digits = 1),
      fm(IQR(x, na.rm = TRUE), digits = 3))
  }
)
#> 
#> var          total        setosa       versicolor   virginica                
#> n            150          50 (33.3%)   50 (33.3%)   50 (33.3%)               
#> Sepal.Length 5.8 / 1.300  5.0 / 0.400  5.9 / 0.700  6.5 / 0.675  *** ¹       
#> Sepal.Width  3.0 / 0.500  3.4 / 0.475  2.8 / 0.475  3.0 / 0.375  *** ¹       
#> Petal.Length 4.4 / 3.500  1.5 / 0.175  4.4 / 0.600  5.6 / 0.775  *** ¹       
#> Petal.Width  1.3 / 1.500  0.2 / 0.100  1.3 / 0.300  2.0 / 0.500  *** ¹       
#> ---
#> ¹) Kruskal-Wallis test, ²) Fisher exact test, ³) Chi-Square test
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#> 

# replace kruskal.test by ANOVA and report the p.value
# Change tests for all the types
tOne(x = iris[, -5], groups = iris[, 5],
     FUN = function(x) gettextf("%s / %s",
            fm(mean(x, na.rm = TRUE), digits = 1),
            fm(sd(x, na.rm = TRUE), digits = 3)),

     TEST = list(
       num = list(fun = function(x, g){summary(aov(x ~ g))[[1]][1, "Pr(>F)"]},
                        lbl = "ANOVA"),
               cat = list(fun = function(x, g){chisq.test(table(x, g))$p.val},
                        lbl = "Chi-Square test"),
               dich = list(fun = function(x, g){fisher.test(table(x, g))$p.val},
                         lbl = "Fisher exact test")),
       fmt = list(abs = "abs.sty", num  = "num.sty", per = "per.sty",
                pval = style(fmt = "*", naForm = "   "))
)
#> 
#> var          total        setosa       versicolor   virginica                
#> n            150          50 (33.3%)   50 (33.3%)   50 (33.3%)               
#> Sepal.Length 5.8 / 0.828  5.0 / 0.352  5.9 / 0.516  6.6 / 0.636  *** ¹       
#> Sepal.Width  3.1 / 0.436  3.4 / 0.379  2.8 / 0.314  3.0 / 0.322  *** ¹       
#> Petal.Length 3.8 / 1.765  1.5 / 0.174  4.3 / 0.470  5.6 / 0.552  *** ¹       
#> Petal.Width  1.2 / 0.762  0.2 / 0.105  1.3 / 0.198  2.0 / 0.275  *** ¹       
#> ---
#> ¹) ANOVA, ²) Fisher exact test, ³) Chi-Square test
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#> 

t1 <- tOne(x     = Pizza[,c("temperature", "driver", "rebate")],
           groups   = Pizza$area,
           align = " ",
           total = FALSE,

           FUN = function(x) gettextf("%s / %s (%s)",
                                      fm(mean(x, na.rm = TRUE), digits = 1),
                                      fm(sd(x, na.rm = TRUE), digits = 3),
                                      fm(median(x, na.rm = TRUE), digits = 1)),

           TEST = NA,

           fmt = list(abs  = style(bigMark = " ", digits=0),
                      num  = style(bigMark = " ", digits=1),
                      per  = style(fmt=function(x)
                          strPad(fm(x, fmt="%", digits=1), width=5, adj = "r")),
                      pval = style(fmt = "*", naForm = "   "))
)
# add a userdefined legend
attr(t1, "legend") <- "numeric: mean / sd (median)), factor: n (n%)"

t1
#> 
#> var                      Brent                    Camden                   Westminster             
#> n                        474 (39.5%)              344 (28.7%)              381 (31.8%)             
#> temperature              51.1 / 8.734 (53.4)      47.4 / 10.111 (50.3)     44.3 / 9.836 (45.9)     
#> driver                                                                                             
#> Butcher                  72 (15.2%)               1 ( 0.3%)                22 ( 5.8%)              
#> Carpenter                29 ( 6.1%)               19 ( 5.6%)               221 (58.2%)             
#> Carter                   177 (37.4%)              47 (13.8%)               5 ( 1.3%)               
#> Farmer                   19 ( 4.0%)               87 (25.5%)               11 ( 2.9%)              
#> Hunter                   128 (27.1%)              4 ( 1.2%)                24 ( 6.3%)              
#> Miller                   6 ( 1.3%)                41 (12.0%)               77 (20.3%)              
#> Taylor                   42 ( 8.9%)               142 (41.6%)              20 ( 5.3%)              
#> rebate (= TRUE)          235 (50.3%)              172 (50.3%)              184 (48.7%)             
#> ---
#> numeric: mean / sd (median)), factor: n (n%) 
#> 


# dichotomous integer or logical values can be reported by the high or low value
set.seed(1)
x <- sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE)
y <- sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE) == 1
z <- factor(sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE))
g <- sample(x = letters[1:4], size = 100, replace = TRUE)
d.set <- data.frame(x = x, y = y, z = z, g = g)

tOne(d.set[1:3], d.set$g, intref = "low")
#> Warning: Chi-squared approximation may be incorrect
#> 
#> var         total       a           b           c           d                      
#> n           100         30 (30.0%)  21 (21.0%)  25 (25.0%)  24 (24.0%)             
#> x (= 0)     32 (32.0%)  10 (33.3%)  9 (42.9%)   7 (28.0%)   6 (25.0%)   ³          
#> y (= FALSE) 30 (30.0%)  8 (26.7%)   7 (33.3%)   10 (40.0%)  5 (20.8%)   ³          
#> z (= 0)     21 (21.0%)  7 (23.3%)   4 (19.0%)   6 (24.0%)   4 (16.7%)   ³          
#> ---
#> ¹) Kruskal-Wallis test, ²) Fisher exact test, ³) Chi-Square test
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#> 

tOne(d.set[1:3], d.set$g, intref = "high")
#> Warning: Chi-squared approximation may be incorrect
#> 
#> var        total      a          b          c          d                    
#> n          100        30 (30.0%) 21 (21.0%) 25 (25.0%) 24 (24.0%)           
#> x (= 1)    68 (68.0%) 20 (66.7%) 12 (57.1%) 18 (72.0%) 18 (75.0%) ³         
#> y (= TRUE) 70 (70.0%) 22 (73.3%) 14 (66.7%) 15 (60.0%) 19 (79.2%) ³         
#> z (= 1)    79 (79.0%) 23 (76.7%) 17 (81.0%) 19 (76.0%) 20 (83.3%) ³         
#> ---
#> ¹) Kruskal-Wallis test, ²) Fisher exact test, ³) Chi-Square test
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#> 

# report both levels of the factor
tOne(data.frame(z = z), g, intref = "both")
#> Warning: Chi-squared approximation may be incorrect
#> 
#> var        total      a          b          c          d                    
#> n          100        30 (30.0%) 21 (21.0%) 25 (25.0%) 24 (24.0%)           
#> z                                                                 ³         
#> 0          21 (21.0%) 7 (23.3%)  4 (19.0%)  6 (24.0%)  4 (16.7%)            
#> 1          79 (79.0%) 23 (76.7%) 17 (81.0%) 19 (76.0%) 20 (83.3%)           
#> ---
#> ¹) Kruskal-Wallis test, ²) Fisher exact test, ³) Chi-Square test
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#> 

options(opt)


if (FALSE) { # \dontrun{

# Send the whole stuff to Word
wrd <- getNewWrd()
toWrd(
  tOne(x   = Pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")],
       groups = Pizza$quality,
       fmt = list(num=style(digits=1))
       ),
  font = list(name="Arial narrow", size=8),
  align = c("l","r")      # this will be recycled: left-right-left-right ...
)
} # }


```
