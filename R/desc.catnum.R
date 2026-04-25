


.desc_cat_num <- function(x, g) {
  list(
    type = "cat-num",
    res=spineplot(x ~ g),
    x=x, g=g)
  
}

