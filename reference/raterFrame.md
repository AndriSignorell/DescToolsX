# Create a Data.frame for Interrater Agreement

Creates a `data.frame` for a formula `rating ~ subjects | raters` with
the subjects in rows and the raters in columns as base structure for
interrater agreement (IRA) functions.

## Usage

``` r
raterFrame(formula, data, subset, na.action, dropSubj = FALSE)
```

## Arguments

- formula:

  something like `rating ~ subjects | raters`

- data:

  the data

- subset:

  potential subset, evaluated in the long-format data

- na.action:

  what should happen with missings, applied per subject (i.e. to the
  rows of the wide result)

- dropSubj:

  logical; whether to drop the subject column (default `FALSE`)

## Value

a `data.frame` of class `"raterFrame"` with subjects in rows and raters
in columns. The name of the subject column is kept in the `"subject"`
attribute (`NA` if it was dropped).

## Details

Assessments made by raters are typically - and appropriately - stored
and organized in databases. Data originating from databases are usually
in long format. Converting this long format into a wide format suitable
for analysis is a complex task, as it is not just a matter of simply
displaying consecutive values in a new column (as in usual
"from-long-to-wide" operations), but of assigning the values to the
correct subjects, which requires a sequential join.  
The present function supports this process by converting long-format
data into a wide format that can be used by subsequent inter-rater
agreement functions. Missing values are marked as `NA`s.

`na.action` is applied to the *wide* frame, i.e. per subject: `na.omit`
removes subjects with at least one missing rating. The `"na.action"`
attribute of the result carries a `"values"` attribute with the
identifiers of the omitted subjects; it is absent if nothing was
omitted.

## See also

[bedrock::resolveFormula](https://andrisignorell.github.io/bedrock/reference/resolveFormula.html)

Other agreement: [`blandAltmanData()`](blandAltmanData.md),
[`normalizeToConfusion()`](normalizeToConfusion.md)

## Examples

``` r

d.long <- data.frame(
     expand.grid(subj=as.character(1:5), rater=LETTERS[1:3]),
     rating = c(1, 4, 5, 7, 2, 2, 5, 6, 7, 1, 1, 4, 6, 6, 2))

# default rater frame
raterFrame(rating ~ subj | rater, data=d.long)
#>   subj A B C
#> 1    1 1 2 1
#> 2    2 4 5 4
#> 3    3 5 6 6
#> 4    4 7 7 6
#> 5    5 2 1 2

# introduce some NAs
raterFrame(rating ~ subj | rater, data=d.long[-c(3, 6), ])
#>   subj  A  B C
#> 1    1  1 NA 1
#> 2    2  4  5 4
#> 3    3 NA  6 6
#> 4    4  7  7 6
#> 5    5  2  1 2

# omit cases containing NAs
raterFrame(rating ~ subj | rater, data=d.long[-c(3, 6), ],
           na.action=na.omit)
#>   subj A B C
#> 2    2 4 5 4
#> 4    4 7 7 6
#> 5    5 2 1 2

# omit the subject column
raterFrame(rating ~ subj | rater, data=d.long, dropSubj=TRUE)
#>   A B C
#> 1 1 2 1
#> 2 4 5 4
#> 3 5 6 6
#> 4 7 7 6
#> 5 2 1 2
```
