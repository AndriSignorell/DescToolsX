
DigitSum <- function(x)
  # calculates the digit sum of a number: DigitSum(124) = 7
  sapply(x, function(z)
    sum(floor(z / 10^(0L:(nchar(z) - 1L))) %% 10L))


