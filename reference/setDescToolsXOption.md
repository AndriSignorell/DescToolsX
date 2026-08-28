# Set DescToolsX Options

Set one or more package options. Each supplied name is stored with the
`"DescToolsX."` prefix and can be read with
[`getOption()`](https://rdrr.io/r/base/options.html).

## Usage

``` r
setDescToolsXOption(...)
```

## Arguments

- ...:

  named option values

## Value

a named list containing the previous values, invisibly

## Details

Options used by descriptive methods include:

- `plotit`:

  whether descriptive functions create plots by default

- `footnote`:

  characters used as footnote markers

- `lang`:

  language used for month and weekday names

## See also

[`options`](https://rdrr.io/r/base/options.html),
[`getOption`](https://rdrr.io/r/base/options.html)

## Examples

``` r
old <- getOption("DescToolsX.plotit")
setDescToolsXOption(plotit = FALSE)
getOption("DescToolsX.plotit")
#> [1] FALSE
options(DescToolsX.plotit = old)
```
