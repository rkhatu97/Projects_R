install.packages("SciViews")
install.packages("nortest")
library(SciViews)
library(psych)
library(lattice)
library(MASS)
library(ggplot2)
library(fitdistrplus)
library(nortest)

#-----------------------
#Problem 1
#-----------------------
#Genreating random number
r <- runif(1000, min = 0, max = 1)
x1 <- -ln(r)
describe(x1)
y1 <- as.data.frame(x1)
View(y)

#Histogram
p1 <- ggplot(y1, aes(x=x1)) +
  geom_histogram(colour="black", fill="white", binwidth = 1) + 
  xlab("Value") + 
  ylab("Frequency") + 
  ggtitle("Relative Frequency Histogram")
p1


fit1<-fitdistr(x1,"exponential")
ks.test(x1, "pexp", fit1$estimate)

#Probability Plot
p1 <- ggplot(y1, aes(x=x1)) +
      geom_histogram(aes(y=..density..),      
               binwidth=0.2,
               colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  xlim(c(0, quantile(x1, 0.99))) +
  xlab("Value") + 
  ggtitle("Probability Plot")
p1


#Chi-square test
p1.exp <- pexp(x1)
chisq.test(x1, p = p1.exp, rescale.p = T)

#----------------------
#Problem 2
#----------------------
#Genreating random number
r1 <- runif(10000, min = 0, max = 1)
r2 <- runif(10000, min = 0, max = 1)
r3 <- runif(10000, min = 0, max = 1)
x2 <- -log(r1*r2*r3)
describe(x2)
y2 <- as.data.frame(x2)
View(y2)

#Histogram
p2 <- ggplot(y2, aes(x=x2)) +
  geom_histogram(colour="black", fill="white", binwidth = 1) + 
  xlab("Value") + 
  ylab("Frequency") + 
  ggtitle("Relative Frequency Histogram")

p2

#Probability Plot
p2 <- ggplot(y2, aes(x=x2)) +
  geom_histogram(aes(y=..density..),      
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  xlab("Value") + 
  ggtitle("Probability Plot")
p2

fit2<-fitdist(x2,"gamma")

#Chi-square test
p2.gam<-pgamma(x2, shape = 3)
chisq.test(x2, p = p2.gam, rescale.p = T)

#-----------------------
#Problem 3
#-----------------------

r_3 <- runif(1000, min = 0, max = 1)
r_4 <- runif(1000, min = 0, max = 1)

x_1 <- -log(r_3)
x_2 <- -log(r_4)


y <- c()
count <- 0

for (i in 1:length(x_1)) {
  k <- (x_1[i] - 1)^2/2
  if (x_2[i] >= k) {
    count = count + 1
    r3 <- runif(count)
    if (r3[length(r3)]>0.5){
      y=c(y,x_1[i])
    } else if (r3[length(r3)]<=0.5){
      y=c(y,-x_1[i])
    }
  } 
  
}

describe(y)
y3 <- as.data.frame(y)
View(y3)

#Histogram
p3 <- ggplot(y3, aes(x=y)) +
  geom_histogram(colour="black", fill="white", binwidth = 1) + 
  xlab("Value") + 
  ylab("Frequency") + 
  ggtitle("Relative Frequency Histogram")

p3


#Probability Plot
p3 <- ggplot(y3, aes(x=y)) +
  geom_histogram(aes(y=..density..),      
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  xlab("Value") + 
  ylab("Frequency") + 
  ggtitle("Probability Plot")
p3

fit3<-fitdistr(y,"normal")

#Chisquare Test
pearson.test(y)

#------------------
#Problem 4
#------------------
m<-c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000)
n=c()
for (x in m){
  r_3 <- runif(x, min = 0, max = 1)
  r_4 <- runif(x, min = 0, max = 1)
  
  x_1 <- -log(r_3)
  x_2 <- -log(r_4)
  y=c()
  w=c()
  count=0
  
  for (i in 1:length(x_1)) {
    k <- (x_1[i] - 1)^2/2
    if (x_2[i] >= k) {
      count = count + 1
      r3 <- runif(count)
      if (r3[length(r3)]>0.5){
        y=c(y,x_1[i])
      } else if (r3[length(r3)]<=0.5){
        y=c(y,-x_1[i])
      }
    } 
    
  }
  n<- c(n,count)
  w <- c(w, m/n)
}
n
w
describe(w)

y4 <- as.data.frame(w)
View(y4)

#Histogram
p4 <- ggplot(y4, aes(x=w)) +
  geom_histogram(colour="black", fill="white", binwidth = 0.08) + 
  xlab("Value") + 
  ylab("Frequency") + 
  ggtitle("Relative Frequency Histogram")
p4


#Probability Plot
p4 <- ggplot(y4, aes(x=w)) +
  geom_histogram(aes(y=..density..),      
                 binwidth=0.08,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  xlab("Value") + 
  ylab("Frequency") + 
  ggtitle("Probability Plot")
p4

fit4 <- fitdistr(w,"normal") 

#Chi-square test
p4.norm<-pnorm(w)
chisq.test(w, p = p4.norm, rescale.p = T)

