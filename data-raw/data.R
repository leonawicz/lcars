library(trekcolors)

lcars2357 <- data.frame(
  series = 2357L, name = names(lcars_2357()), value = lcars_2357(),
  stringsAsFactors = FALSE
)

lcars2369 <- data.frame(
  series = 2369L, name = names(lcars_2369()), value = lcars_2369(),
  stringsAsFactors = FALSE
)

lcars2375 <- data.frame(
  series = 2375L, name = names(lcars_2375()), value = lcars_2375(),
  stringsAsFactors = FALSE
)

lcars2379 <- data.frame(
  series = 2379L, name = names(lcars_2379()), value = lcars_2379(),
  stringsAsFactors = FALSE
)

lcarsdata <- do.call(rbind, list(lcars2357, lcars2369, lcars2375, lcars2379))

usethis::use_data(lcarsdata)
