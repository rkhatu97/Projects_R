library(tibble)
library(readr)
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(MASS)
library(psych)
library(xts)
library(vars)
library(Metrics)
library(fastDummies)
library(nortest)
library(lattice)

#-------------------
#Problem 1:
#-------------------
files = list.files(path = "D:/Courses/Quarter-2/Enterprise Analytics/Week 3",
                   pattern = "*.csv", full.names = T)

hon <- sapply(files, read_csv, simplify = FALSE) %>%
  bind_rows(.id = "id")
  
str(hon)
View(hon)
hon <- as.data.frame(hon)
hon <- hon[,-c(1,2)]
hon$Date <- as.Date(hon$Date, format = "%m/%d/%Y", tz=Sys.timezone(location = TRUE))
hon <- hon[order(as.Date(hon$Date, format="%Y/%m/%d")),]
hon <- unique(hon)
hon <- data.frame(`close` = hon$Close, `Volume` = hon$Volume, row.names = hon$Date)

#time series
myts <- ts(hon, start = 1, end = 124, frequency = 1)
autoplot(myts, facets = TRUE, main = "Time Series Plot")

alpha_0.15 <- HoltWinters(myts[,1], alpha = 0.15, beta=FALSE, gamma=FALSE)
alpha_0.35 <- HoltWinters(myts[,1], alpha = 0.35, beta=FALSE, gamma=FALSE)
alpha_0.55 <- HoltWinters(myts[,1], alpha = 0.55, beta=FALSE, gamma=FALSE)
alpha_0.75 <- HoltWinters(myts[,1], alpha = 0.75, beta=FALSE, gamma=FALSE)


cal_alpha <- function(u){
  c <- c()
  form <- 0
  for (i in 1:length(myts[,1])) {
    if (i == 1) {
      c <- c(c, myts[,1][i])
    }else if (i >= 2) {
      form <- (u*(myts[,1][i-1]))+((1-u)*c[i-1])
      c <- c(c, form)
    }
  }
  return(c)
}

#0.15
alpha0.15 <- cal_alpha(0.15)
mse(myts[,1], alpha0.15)
forc_0.15 <- forecast(alpha_0.15)

#0.35
alpha0.35 <- cal_alpha(0.35)
mse(myts[,1], alpha0.35)
forc_0.35 <- forecast(alpha_0.35)

#0.55
alpha0.55 <- cal_alpha(0.55)
mse(myts[,1], alpha0.55)
forc_0.55 <- forecast(alpha_0.55)

#0.75
alpha0.75 <- cal_alpha(0.75)
mse(myts[,1], alpha0.75)
forc_0.75 <- forecast(alpha_0.75)

#plot
plot_fun <- function(k){
  df <- merge(as.xts(k), as.xts(myts[,1]))
  names(df) <- c("predicted", "actual")
  df <- data.frame(`predicted` = df$predicted, `actual` = df$actual, row.names = rownames(hon))
  ggplot(df, aes(x=index(df))) + 
    geom_line(aes(y=predicted), col='red') + 
    geom_line(aes(y=actual), col='black') + 
    theme_bw() +
    labs(title="Holt-Winters filtering", x="Time", y="Observed / Fitted") + 
    theme(plot.title = element_text(size=18, face="bold"))
}

plot_fun(alpha_0.15$fitted[,1])
plot_fun(alpha_0.35$fitted[,1])
plot_fun(alpha_0.55$fitted[,1])
plot_fun(alpha_0.75$fitted[,1])

#---------------
#Problem 2
#---------------
beta_0.15 <- HoltWinters(myts[,1], alpha = 0.75, beta=0.15, gamma=FALSE)
beta_0.25 <- HoltWinters(myts[,1], alpha = 0.75, beta=0.25, gamma=FALSE)
beta_0.45 <- HoltWinters(myts[,1], alpha = 0.75, beta=0.45, gamma=FALSE)
beta_0.85 <- HoltWinters(myts[,1], alpha = 0.75, beta=0.85, gamma=FALSE)

cal_beta <- function(beta,alpha){
  c <- c()
  form <- 0
  y <- c()
  k <- eval(as.name(paste("alpha", alpha, sep = "")))
  for (i in 1:length(myts[,1])) {
    if (i == 1) {
      c <- 0
    }else if (i >= 2) {
      form <- (beta*(k[i]-k[i-1]))+((1-beta)*c[i-1])
      c <- c(c, form)
    }
  }
  
  for (j in 1:length(myts[,1])) {
    f <- sum(k[j], c[j])
    y <- c(y, f)
  }
  return(y)
}

#0.15
beta0.15 <- cal_beta(0.15,0.75)
mse(myts[,1], beta0.15)
forc_b_0.15 <- forecast(beta_0.15)

#0.25
beta0.25 <- cal_beta(0.25,0.75)
mse(myts[,1], beta0.25)
forc_b_0.25 <- forecast(beta_0.25)

#0.45
beta0.45 <- cal_beta(0.45,0.75)
mse(myts[,1], beta0.45)
forc_b_0.45 <- forecast(beta_0.45)

#0.85
beta0.85 <- cal_beta(0.85,0.75)
mse(myts[,1], beta0.85)
forc_b_0.85 <- forecast(beta_0.85)

plot_fun(beta_0.15$fitted[,1])
plot_fun(beta_0.25$fitted[,1])
plot_fun(beta_0.45$fitted[,1])
plot_fun(beta_0.85$fitted[,1])

#--------------
#Probem 3
#--------------
heli <- data.frame(`heli.no` = seq(1:8),
                   `labour.hrs` = c(2000,1400,1238,1142,1075,1029,985,957))
heli

model <- lm(labour.hrs ~ ., data = heli)
summary(model)

plot(model)
v <- data.frame(`heli.no` = heli$heli.no, `resid` = resid(model))

ggplot(heli, aes(x = heli.no, y= labour.hrs)) +
  geom_point(colour="black", fill="white") + 
  xlab("Helicopter Number") + 
  ylab("Labor Hours") + 
  ggtitle("Labour hours Vs Helicopter number")

ggplot(v, aes(x = heli.no, y= resid)) +
  geom_point(colour="black", fill="white") + 
  xlab("Helicopter Number") +  
  ylab("Residuals") + 
  ggtitle("Residual Vs Helicopter number")


#--------------
#Problem 4
#--------------
car <-  read.csv(file.choose())
str(car)
View(car)
car <- car[-c(1),]
c <- car[1,]
colnames(car) <-  c
car <- car[-c(1),]
car$Month<-as.character(car$Month)
car$Units <- gsub(",", "", car$Units)
car$Units<-as.integer(car$Units)

#Transforming into categorical variables
t <- car[,c(1,2)]
results <- fastDummies::dummy_cols(t)
knitr::kable(results)
View(results)
results <- results[,-c(1,2)]
car <- cbind(car[,c(3)], results)
names(car)[1] <- 'Units'

#Regression Model
model_car <- lm(Units ~ .,data = car)
summary(model_car)
stepAIC(model_car, direction = "backward")

model_car <- lm(Units ~ Year_1 + Year_2 + Month_Aug + Month_Dec + 
                  Month_Feb + Month_Jan + Month_Jun + Month_Mar + Month_May + 
                  Month_Nov, data = car)
predict <- predict(model_car, newdata = car)

#anova
anova(model_car)

#residuals 
residuals<-resid(model_car)
residuals <- as.data.frame(residuals)

hist(residuals,type = "percent", main = "Histogram of Residuals ",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=10)
ggplot(residuals, aes(x = residuals)) +
  geom_histogram(colour="black", fill="white", binwidth = 1250) + 
  xlab("Values") + 
  ylab("Frequency") + 
  ggtitle("Histogram of residuals")

nlm <- fitdistr(residuals$residuals,"normal")
para <- nlm$estimate

ggplot(residuals, aes(x=residuals)) +
  geom_histogram(aes(y=..density..),      
                 binwidth=1250,
                 colour="black", fill="white") +
  stat_function(fun = dnorm, args = list(mean = para[1], sd = para[2]), geom = "area", alpha = 0.5, fill="#FF6666") + 
  xlab("Residuals") + 
  ylab("Frequency") + 
  ggtitle("Probability Plot")

pearson.test(residuals$residuals)
