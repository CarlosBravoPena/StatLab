library(dslabs)
library(dplyr)

  data(heights)
heights
nrows(heights)
summary(heights)
heights
heights[777,1]
names(heights)
max(heights$height)
index<-min(heights$height)
nrow(index)
heights
index<- nrow(min(heights))
match(50, heights$height)
mean(heights$height)
median(heights$height)
str(heights)
summary(heights)
datafiltered<-heights%>%filter(heights$height>78)
summary(datafiltered)
