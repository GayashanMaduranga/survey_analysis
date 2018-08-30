data <- read.csv('Survey on career interest of undergraduate.csv',header = TRUE,stringsAsFactors = FALSE)
head(data)

doubleValues <- function(x){
  return (x*2)
}

v <- 1:10

v <- sapply(v, doubleValues)
v
mtcars2 <- mtcars

mtcars2$vs <- sapply(mtcars2$vs,doubleValues)