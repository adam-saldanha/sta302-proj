day1 <- read_xpt("day1diet.xpt")
day2 <- read_xpt("day2diet.xpt") 
write.csv(day1, "day1diet.csv", row.names = FALSE)
write.csv(day2, "day2diet.csv", row.names = FALSE)
