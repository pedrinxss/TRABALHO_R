
conv_num <- function (x){
  x <- na.omit
  x <- as.numeric(x$População)
  return (x)
}


str(att_texas)
conv_num(att_texas)
na.omit(att_texas, População)
str(att_texas)

conv_num(att_texas)

valor <- as.numeric(as.character(att_texas))

haha <- transform(att_texas, População = as.factor(População))