# Bivariate (Two-Dimensional) Frequency Distribution

Calculate a frequency distribution for two continuous variables.

## Usage

``` r
freq2D(x, ...)

# S3 method for class 'formula'
freq2D(x, data, subset, na.action, n = 20, pad = 0, dnn = NULL, ...)

# Default S3 method
freq2D(x, y, n = 20, pad = 0, dnn = NULL, ...)
```

## Arguments

- x:

  a vector of x values, or a data frame whose first two columns contain
  the x and y values

- ...:

  named arguments passed to the default method

- data:

  a `data.frame`, `matrix`, or `list` from which the variables in
  `formula` should be taken

- subset:

  an optional vector specifying a subset of observations to be used

- na.action:

  a function which indicates what should happen when the data contain
  missing values. Defaults to `getOption("na.action")`.

- n:

  the desired number of bins for the output, a scalar or a vector of
  length 2

- pad:

  number of rows and columns to add to each margin, containing only
  zeros

- dnn:

  names for the dimensions in the result

- y:

  a vector of y values

- formula:

  a [`formula`](https://rdrr.io/r/stats/formula.html), such as `y ~ x`

## Value

a frequency matrix whose rows represent the y bins in descending order
and whose columns represent the x bins

## Details

The exact number of bins is determined by the
[`pretty`](https://rdrr.io/r/base/pretty.html) function, based on the
value of `n`.

Padding the margins with zeros can be helpful for subsequent analysis,
such as smoothing.

## Note

Based on code by Arni Magnusson, adapted to conform to package
standards.

## See also

[`cut`](https://rdrr.io/r/base/cut.html),
[`table`](https://rdrr.io/r/base/table.html), and
[`print.table`](https://rdrr.io/r/base/print.html) are the basic
underlying functions.

Other frequency: [`expFreq()`](expFreq.md), [`freq()`](freq.md),
[`percTable()`](percTable.md), [`tOne()`](tOne.md)

## Examples

``` r

freq2D(quakes$long, quakes$lat, dnn="")
#>      
#>       166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183
#>   -11  13   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#>   -12  16  11   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#>   -13   4  12   0   3   4   0   1   1   0   0   0   0   0   0   0   0   0   0
#>   -14   3  14   0   0   1   1   2   1   0   0   0   0   0   0   0   0   0   0
#>   -15   1  14   3   0   0   2   0   0   0   0   0   0   0   0   0   1   1   1
#>   -16   0  16   9   0   0   0   0   0   0   0   0   0   1   0   0   1   0   3
#>   -17   0   0   1   1   0   0   0   0   0   0   0   0   0   0   1   8   3   4
#>   -18   0   0   1   3   0   0   0   0   0   0   0   1   0   0   2  49  42   4
#>   -19   0   0   0  21   3   0   0   0   0   0   0   0   0   0   0   3  15   8
#>   -20   0   0   0   7   2   0   0   0   1   0   0   0   0   1   1   7  33   5
#>   -21   0   0   0   0   5   0   0   1   1   0   0   0   0   0   0  49  26   2
#>   -22   0   0   0   0   3   7   4   0   0   0   0   0   0   0  16  15   5   6
#>   -23   0   0   0   0   2   3   1   3   0   0   0   0   0   4  18   3   0   5
#>   -24   0   0   0   0   0   0   0   0   0   0   0   0   0   5  39   2   1   8
#>   -25   0   0   0   0   0   0   0   0   0   0   0   0   2   1  16   5   0   6
#>   -26   0   0   0   0   0   0   0   0   0   0   0   0   6   3   7   1   7   2
#>   -27   0   0   0   0   0   0   0   0   0   0   0   0   2   1   0   2  12   7
#>   -28   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  14   7
#>   -29   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   3   4
#>   -30   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   8   7   0
#>   -31   0   0   0   0   0   0   0   0   0   0   0   0   0   0   2   4   3   0
#>   -32   0   0   0   0   0   0   0   0   0   0   0   0   0   0   4   4   0   0
#>   -33   0   0   0   0   0   0   0   0   0   0   0   0   0   0   2   4   5   0
#>   -34   0   0   0   0   0   0   0   0   0   0   0   0   0   1   2   1   2   0
#>   -35   0   0   0   0   0   0   0   0   0   0   0   0   0   1   2   1   0   0
#>   -36   0   0   0   0   0   0   0   0   0   0   0   0   0   1   1   0   0   0
#>   -37   0   0   0   0   0   0   0   0   0   0   0   1   2   0   0   0   0   0
#>   -38   0   0   0   0   0   0   0   0   0   0   1   2   0   0   0   0   0   0
#>   -39   0   0   0   0   0   0   0   0   0   0   1   0   0   0   0   0   0   0
#>      
#>       184 185 186 187 188
#>   -11   0   0   0   0   0
#>   -12   0   0   0   0   0
#>   -13   0   0   0   0   0
#>   -14   0   0   0   0   0
#>   -15   1   7  10   7   1
#>   -16   4  15   8   8   2
#>   -17   0   4   8   5   1
#>   -18   1   8   0   3   1
#>   -19  12   5   4   3   1
#>   -20  19   6   5   2   0
#>   -21   7   8   8   0   0
#>   -22   8   8   1   0   0
#>   -23   7   6   1   0   0
#>   -24   7   5   0   0   0
#>   -25   0   0   0   0   0
#>   -26   2   0   0   0   0
#>   -27   4   0   0   0   0
#>   -28   1   0   0   0   0
#>   -29   1   0   0   0   0
#>   -30   0   0   0   0   0
#>   -31   0   0   0   0   0
#>   -32   0   0   0   0   0
#>   -33   0   0   0   0   0
#>   -34   0   0   0   0   0
#>   -35   0   0   0   0   0
#>   -36   0   0   0   0   0
#>   -37   0   0   0   0   0
#>   -38   0   0   0   0   0
#>   -39   0   0   0   0   0
freq2D(lat ~ long, quakes, n=c(10, 20), pad=1)
#>      long
#> lat   164 166 168 170 172 174 176 178 180 182 184 186 188 190
#>   -10   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#>   -11   0  14   2   0   0   0   0   0   0   0   0   0   0   0
#>   -12   0  20   7   0   0   0   0   0   0   0   0   0   0   0
#>   -13   0   9   7   7   2   0   0   0   0   0   0   0   0   0
#>   -14   0   9   8   2   2   1   0   0   0   0   0   0   0   0
#>   -15   0   4  14   0   2   0   0   0   0   2   5  20   2   0
#>   -16   0   8  17   0   0   0   0   1   1   3   8  23   6   0
#>   -17   0   0   2   0   0   0   0   0   2  14   1  15   2   0
#>   -18   0   0   3   1   0   0   0   1  12  81   8   7   2   0
#>   -19   0   0   2  22   0   0   0   0   2  22  17   9   1   0
#>   -20   0   0   4   5   0   1   0   1   3  41  24  10   0   0
#>   -21   0   0   0   5   0   2   0   0  17  59  12  12   0   0
#>   -22   0   0   0   5   9   0   0   0  29   9  15   6   0   0
#>   -23   0   0   0   5   3   1   0   0  24   3  16   1   0   0
#>   -24   0   0   0   0   0   0   0   1  45   8  13   0   0   0
#>   -25   0   0   0   0   0   0   0   2  21   5   2   0   0   0
#>   -26   0   0   0   0   0   0   0   7   9  10   2   0   0   0
#>   -27   0   0   0   0   0   0   0   3   1  17   7   0   0   0
#>   -28   0   0   0   0   0   0   0   0   0  17   5   0   0   0
#>   -29   0   0   0   0   0   0   0   0   0   6   3   0   0   0
#>   -30   0   0   0   0   0   0   0   0   3  12   0   0   0   0
#>   -31   0   0   0   0   0   0   0   0   4   5   0   0   0   0
#>   -32   0   0   0   0   0   0   0   0   6   2   0   0   0   0
#>   -33   0   0   0   0   0   0   0   0   4   7   0   0   0   0
#>   -34   0   0   0   0   0   0   0   0   4   2   0   0   0   0
#>   -35   0   0   0   0   0   0   0   0   4   0   0   0   0   0
#>   -36   0   0   0   0   0   0   0   1   1   0   0   0   0   0
#>   -37   0   0   0   0   0   0   1   2   0   0   0   0   0   0
#>   -38   0   0   0   0   0   0   1   2   0   0   0   0   0   0
#>   -39   0   0   0   0   0   0   1   0   0   0   0   0   0   0
#>   -40   0   0   0   0   0   0   0   0   0   0   0   0   0   0
#> attr(,"data.name")
#> [1] "lat ~ long"
```
