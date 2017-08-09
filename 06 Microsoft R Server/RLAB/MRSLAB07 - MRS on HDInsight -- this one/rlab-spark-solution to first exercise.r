mydf <- agg(groupBy(airDF, "YEAR", "MONTH"), ARR_DELAY="avg")
mydf
as.data.frame(mydf)