

.desc_cat_cat <- function(x, y) {
  list( type = "cat-cat",
        res=tapply(x, g, desc, plotit=FALSE),
        x=x, g=g)
}

