rel_frequency <- function (x) {
  barplot(table(x))
}

rel_frequency(c(7,7,8,9,7,5,1,8))
