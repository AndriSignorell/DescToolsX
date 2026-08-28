# Expected Frequencies

Calculate the expected frequencies of an n-way table assuming
independence.

## Usage

``` r
expFreq(x, freq = c("abs", "rel"))
```

## Arguments

- x:

  a table

- freq:

  whether absolute or relative frequencies are computed. Must be `"abs"`
  or `"rel"`; partial matching is supported.

## Value

a table with either the absolute or the relative expected frequencies,
of the same dimension and dimnames as `x`

## Details

Expected values for a 2-dimensional table can be easily calculated with
res \<- [`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)`(x)` and
accessed by `res$expected`. This approach fails for higher dimensional
tables, for which there is no support. `expFreq()` also returns expected
frequencies for higher dimensional tables.

## Note

Based on code by David Meyer previously published as
`independence_table` in vcd, adapted to conform to package standards.

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)

Other frequency: [`freq()`](freq.md), [`freq2D()`](freq2D.md),
[`percTable()`](percTable.md), [`tOne()`](tOne.md)

## Examples

``` r

expFreq(Titanic)
#> , , Age = Child, Survived = No
#> 
#>       Sex
#> Class         Male      Female
#>   1st    8.5690577   2.3266650
#>   2nd    7.5144045   2.0403062
#>   3rd   18.6146300   5.0542323
#>   Crew  23.3342033   6.3356878
#> 
#> , , Age = Adult, Survived = No
#> 
#>       Sex
#> Class         Male      Female
#>   1st  164.4630158  44.6548916
#>   2nd  144.2214139  39.1589050
#>   3rd  357.2642744  97.0041646
#>   Crew 447.8454431 121.5987049
#> 
#> , , Age = Child, Survived = Yes
#> 
#>       Sex
#> Class         Male      Female
#>   1st    4.0889933   1.1102408
#>   2nd    3.5857326   0.9735958
#>   3rd    8.8825516   2.4117847
#>   Crew  11.1346433   3.0232711
#> 
#> , , Age = Adult, Survived = Yes
#> 
#>       Sex
#> Class         Male      Female
#>   1st   78.4786606  21.3084751
#>   2nd   68.8197485  18.6858936
#>   3rd  170.4797980  46.2885645
#>   Crew 213.7034295  58.0246169
#> 

expFreq(UCBAdmissions, freq="r")
#> , , Dept = A
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted 0.04752573 0.03240792
#>   Rejected 0.07503920 0.05116943
#> 
#> , , Dept = B
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted 0.02979909 0.02032008
#>   Rejected 0.04705030 0.03208373
#> 
#> , , Dept = C
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted 0.04676165 0.03188689
#>   Rejected 0.07383279 0.05034677
#> 
#> , , Dept = D
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted 0.04034339 0.02751026
#>   Rejected 0.06369887 0.04343643
#> 
#> , , Dept = E
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted 0.02974815 0.02028534
#>   Rejected 0.04696988 0.03202888
#> 
#> , , Dept = F
#> 
#>           Gender
#> Admit            Male     Female
#>   Admitted 0.03637017 0.02480092
#>   Rejected 0.05742550 0.03915860
#> 

```
