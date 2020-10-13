crimeincidentreports <- read.csv(file.choose(), na.strings = "")
View(crimeincidentreports)

#District and Year
crimeperdistrict <- table(crimeincidentreports$DISTRICT)
names(crimeperdistrict)[1] <- 'NO District Reported'
x <- barplot(crimeperdistrict, las=2, cex.names = 0.5, cex.axis = 0.8, main = "Crimes Per District for 4 Years", xlab = "Districts", ylab = "Number Of Crimes", ylim = c(0,60000))
crimeperdistrict <- data.frame(crimeperdistrict)
text(x = x, y = crimeperdistrict$Freq, label = crimeperdistrict$Freq, pos = 3, cex = 0.8, col = "black")

crimeperyear <- table(crimeincidentreports$YEAR)
x <- barplot(crimeperyear, main = "Crimes per Year", xlab = "Years", ylab = "Number of Crimes", col = "coral3", ylim = range(pretty(c(0, 120000))))
crimeperyear <- data.frame(crimeperyear)
text(x = x, y = crimeperyear$Freq, label = crimeperyear$Freq, pos = 3, cex = 0.8, col = "black")


#offense per Year
Year_2015 <- crimeincidentreports[ which(crimeincidentreports$YEAR==2015),]
Year_2016 <- crimeincidentreports[ which(crimeincidentreports$YEAR==2016),]
Year_2017 <- crimeincidentreports[ which(crimeincidentreports$YEAR==2017),]
Year_2018 <- crimeincidentreports[ which(crimeincidentreports$YEAR==2018),]


#year 2015
offenseperdistrict_2015 <- table(Year_2015$DISTRICT)
names(offenseperdistrict_2015)[1] <- 'No District reported'
offensepermonth_2015 <- table(Year_2015$MONTH)
offenseperday_2015 <- ordered(Year_2015$DAY_OF_WEEK, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
offenseperday_2015 <- table(offenseperday_2015)
offenseperday_2015
offesneseperhour_2015 <- table(Year_2015$HOUR)
offenseperdistrict_2015
offenseperdistrict_2016
counts <- cbind(offenseperdistrict_2015$Freq, offenseperdistrict_2016$Freq)
x <- barplot(t(counts), beside = TRUE, names.arg = offenseperdistrict_2015$Var1, cex.axis = 0.8, main = "Crimes Per Hour In 2017-2018", xlab = "Hour", ylab = "Number Of Crimes", ylim = c(0,10000), col = c("cadetblue", "yellow"), space = c(0,0.6), legend.text = c("2017", "2018"), args.legend = list(title = "Years", x = "topright", cex = .7))
offenseperdistrict_2015 <- data.frame(offenseperdistrict_2015)
c <- cbind.data.frame(offenseperdistrict_2015$Var1, counts)
names(c)[1] <- 'District'
names(c)[2] <- 'Number of offense reported in 2015'
names(c)[3] <- 'Number of offense reported in 2016'
c
write.csv(c, file = "percent.csv")
x <- barplot(offensepermonth_2015, main = "Crimes Per month In 2015", xlab = "Months", ylab = "Number Of Crimes", ylim = c(0,10000), col = "blue")
offensepermonth_2015 <- data.frame(offensepermonth_2015)
text(x = x, y = offensepermonth_2015$Freq, label = offensepermonth_2015$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offenseperday_2015, main = "Crimes Per Day In 2015", xlab = "Days", ylab = "Number Of Crimes", ylim = c(0,10000), col = "beige")
offenseperday_2015 <- data.frame(offenseperday_2015)
text(x = x, y = offenseperday_2015$Freq, label = offenseperday_2015$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offesneseperhour_2015, main = "Crimes Per hour In 2015", xlab = "Hour", ylab = "Number Of Crimes", ylim = c(0,5000), col = "cadetblue")
offesneseperhour_2015 <- data.frame(offesneseperhour_2015)
text(x = x, y = offesneseperhour_2015$Freq, label = offesneseperhour_2015$Freq, pos = 3, cex = 0.8, col = "black")


#Year 2016
offenseperdistrict_2016 <- table(Year_2016$DISTRICT)
names(offenseperdistrict_2016)[1] <- 'No District reported'
offensepermonth_2016 <- table(Year_2016$MONTH)
offenseperday_2016 <- ordered(Year_2016$DAY_OF_WEEK, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
offenseperday_2016 <- table(offenseperday_2016)
offesneseperhour_2016 <- table(Year_2016$HOUR)
y <- sum(offenseperdistrict_2016$Freq)
y
o <- as.numeric(unlist((offenseperdistrict_2016$Freq/y) * 100))
o
mean(o)

x <- barplot(offenseperdistrict_2016, las=2, cex.names = 0.5, cex.axis = 0.8, main = "Crimes Per District In 2016", xlab = "Districts", ylab = "Number Of Crimes", ylim = c(0,20000), col = "red")
offenseperdistrict_2016 <- data.frame(offenseperdistrict_2016)
text(x = x, y = offenseperdistrict_2016$Freq, label = offenseperdistrict_2016$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offensepermonth_2016, main = "Crimes Per month In 2016", xlab = "Months", ylab = "Number Of Crimes", ylim = c(0,10000), col = "blue")
offensepermonth_2016 <- data.frame(offensepermonth_2016)
text(x = x, y = offensepermonth_2016$Freq, label = offensepermonth_2016$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offenseperday_2016, main = "Crimes Per Day In 2016", xlab = "Days", ylab = "Number Of Crimes", ylim = c(0,20000), col = "beige")
offenseperday_2016 <- data.frame(offenseperday_2016)
text(x = x, y = offenseperday_2016$Freq, label = offenseperday_2016$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offesneseperhour_2016, main = "Crimes Per hour In 2016", xlab = "Hour", ylab = "Number Of Crimes", ylim = c(0,7000), col = "cadetblue")
offesneseperhour_2016 <- data.frame(offesneseperhour_2016)
text(x = x, y = offesneseperhour_2016$Freq, label = offesneseperhour_2016$Freq, pos = 3, cex = 0.8, col = "black")


#Year 2017
offenseperdistrict_2017 <- table(Year_2017$DISTRICT)
names(offenseperdistrict_2017)[1] <- 'No District reported'
offensepermonth_2017 <- table(Year_2017$MONTH)
offenseperday_2017 <- ordered(Year_2017$DAY_OF_WEEK, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
offenseperday_2017 <- table(offenseperday_2017)
offesneseperhour_2017 <- table(Year_2017$HOUR)

x <- barplot(offenseperdistrict_2017, las=2, cex.names = 0.5, cex.axis = 0.8, main = "Crimes Per District In 2017", xlab = "Districts", ylab = "Number Of Crimes", ylim = c(0,20000), col = "red")
offenseperdistrict_2017 <- data.frame(offenseperdistrict_2017)
text(x = x, y = offenseperdistrict_2017$Freq, label = offenseperdistrict_2017$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offensepermonth_2017, main = "Crimes Per month In 2017", xlab = "Months", ylab = "Number Of Crimes", ylim = c(0,10000), col = "blue")
offensepermonth_2017 <- data.frame(offensepermonth_2017)
text(x = x, y = offensepermonth_2017$Freq, label = offensepermonth_2017$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offenseperday_2017, main = "Crimes Per Day In 2017", xlab = "Days", ylab = "Number Of Crimes", ylim = c(0,20000), col = "beige")
offenseperday_2017 <- data.frame(offenseperday_2017)
text(x = x, y = offenseperday_2017$Freq, label = offenseperday_2017$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offesneseperhour_2017, main = "Crimes Per hour In 2017", xlab = "Hour", ylab = "Number Of Crimes", ylim = c(0,10000), col = "cadetblue")
offesneseperhour_2017 <- data.frame(offesneseperhour_2017)
text(x = x, y = offesneseperhour_2017$Freq, label = offesneseperhour_2017$Freq, pos = 3, cex = 0.8, col = "black")

#Year 2018
offenseperdistrict_2018 <- table(Year_2018$DISTRICT)
names(offenseperdistrict_2018)[1] <- 'No District reported'
offensepermonth_2018 <- table(Year_2018$MONTH)
offenseperday_2018 <- ordered(Year_2018$DAY_OF_WEEK, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
offenseperday_2018 <- table(offenseperday_2018)
offesneseperhour_2018 <- table(Year_2018$HOUR)

x <- barplot(offenseperdistrict_2018, las=2, cex.names = 0.5, cex.axis = 0.8, main = "Crimes Per District In 2018", xlab = "Districts", ylab = "Number Of Crimes", ylim = c(0,15000), col = "red")
offenseperdistrict_2018 <- data.frame(offenseperdistrict_2018)
text(x = x, y = offenseperdistrict_2018$Freq, label = offenseperdistrict_2018$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offensepermonth_2018, main = "Crimes Per month In 2018", xlab = "Months", ylab = "Number Of Crimes", ylim = c(0,10000), col = "blue")
offensepermonth_2018 <- data.frame(offensepermonth_2018)
text(x = x, y = offensepermonth_2018$Freq, label = offensepermonth_2018$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offenseperday_2018, main = "Crimes Per Day In 2018", xlab = "Days", ylab = "Number Of Crimes", ylim = c(0,15000), col = "beige")
offenseperday_2018 <- data.frame(offenseperday_2018)
text(x = x, y = offenseperday_2018$Freq, label = offenseperday_2018$Freq, pos = 3, cex = 0.8, col = "black")

x <- barplot(offesneseperhour_2018, main = "Crimes Per hour In 2018", xlab = "Hour", ylab = "Number Of Crimes", ylim = c(0,5000), col = "cadetblue")
offesneseperhour_2018 <- data.frame(offesneseperhour_2018)
text(x = x, y = offesneseperhour_2018$Freq, label = offesneseperhour_2018$Freq, pos = 3, cex = 0.8, col = "black")


percentage.function <- function(a, b)
{
  ((b-a)/a)*100
}
as.numeric(unlist(c))
f <- percentage.function(c$`Number of offense reported in 2015`, c$`Number of offense reported in 2016`)
data.frame(f)
str(c)
rm(f)
f
c
write.table(f, "percent.csv", sep = ",", col.names = !file.exists("percent.csv"), append = T)

