library(fitdistrplus)
library(triangle)
library(psych)
library(nortest)

#--------------------
#Problem 1 : 
#--------------------

results = NULL
for (k in 1:5000){
  Beth_Israel_Medical  <- rtriangle(1, 20, 300, 80)*0.3
  Tufts_Medical <- rtriangle(1, 20, 300, 80)*0.15
  Massachusetts_General <- rtriangle(1, 20, 300, 80)*0.2
  Boston_Medical <- rtriangle(1, 20, 300, 80)*0.25
  Brigham_and_Women  <- rtriangle(1, 20, 300, 80)*0.1
  total = min (Beth_Israel_Medical, Tufts_Medical, Massachusetts_General, Boston_Medical, Brigham_and_Women)
  results = rbind(results, data.frame(Beth_Israel_Medical, Tufts_Medical, Massachusetts_General, Boston_Medical, Brigham_and_Women, total))
}
head(results)
summary(results)
result_time <- data.frame(Beth_Israel_Medical_time = results$Beth_Israel_Medical*(7*2/60),
                          Tufts_Medical_time = results$Tufts_Medical*(10*2/60),
                          Massachusetts_General_time = results$Massachusetts_General*(15*2/60),
                          Boston_Medical_time = results$Boston_Medical*(15*2/60),
                          Brigham_and_Women_time = results$Brigham_and_Women*(20*2/60),
                          total_time = results$total*(7*2/60))

head(result_time)
head(results)
summary(result_time)
summary(results)
#Histogram
library(ggplot2)

gg_func <- function(s, t = results, k = "Triangular Distribution Histogram"){
ggplot(t, aes(x = t[,c(s)])) +
  geom_histogram(colour="black", fill="white", binwidth = 1) + 
  xlab(toupper(s)) + 
  ylab("Frequency") + 
  ggtitle(k)
}
gg_func("Beth_Israel_Medical")
gg_func("Tufts_Medical")
gg_func("Massachusetts_General")
gg_func("Boston_Medical")
gg_func("Brigham_and_Women")

#Law of large number
avg <- 0
n <- 0
for (k in 1:5000) {
  x <- mean(sample(results$Beth_Israel_Medical, k, replace = TRUE))
  avg[k] = x
  n[k] = k
  avg_beth <- data.frame(avg, n)
}

#Plot
p <- ggplot(avg_beth, aes(x=n, y=avg)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm, se=FALSE) +
  xlab("Sample Size") +
  ylab("Sample Mean") +
  ggtitle("Beth Israel Medical-Law of Large Numbers")
p



#C.I.
ci <- function(alpha,m,n){
  alpha <- (1-(alpha/100))
  conf <- 1-alpha
  me <- qt(conf, df = n - 1)*(sd(m)/sqrt(n))
  cat("Lower Bound", mean(m) - me, '\n')
  cat("True Mean", mean(m), '\n')
  cat("Uper Bound", mean(m) + me, '\n')
  cat("Margin Of Error", me, '\n')
}
ci(95,result_time$Beth_Israel_Medical,5000)

#Histogram
gg_func("Beth_Israel_Medical_time", result_time, "Relative Frequency Histogram")

#Probability Plot
p <- ggplot(result_time, aes(x=Beth_Israel_Medical_time)) +
  geom_histogram(aes(y=..density..),      
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  xlab("Beth_Israel_Medical") + 
  ylab("Frequency") + 
  ggtitle("Probability Plot")
p


#Chi-square test
p.tri <- pltriangle(result_time$Beth_Israel_Medical_time)
chisq.test(result_time$Beth_Israel_Medical_time, p = p.tri, rescale.p = T)

#problem 5 with t
t_prob <- function(m,n){
  data.frame((m/n * 60)/m)
}
avg_per <- data.frame(t_prob(results$Beth_Israel_Medical, result_time$Beth_Israel_Medical_time),
                      t_prob(results$Tufts_Medical, result_time$Tufts_Medical_time),
                      t_prob(results$Massachusetts_General, result_time$Massachusetts_General_time),
                      t_prob(results$Boston_Medical, result_time$Boston_Medical_time),
                      t_prob(results$Brigham_and_Women, result_time$Brigham_and_Women_time))
colnames(avg_per) <- c("Beth_Israel_Medical", "Tufts_Medical", "Massachusetts_General", "Boston_Medical", "Brigham_and_Women")
for (i in n) {
  avg_per$per_victim[i] <- sum(avg_per$Beth_Israel_Medical[i], avg_per$Tufts_Medical[i], avg_per$Massachusetts_General[i], avg_per$Boston_Medical[i], avg_per$Brigham_and_Women[i])  
}

ci(95,avg_per$per_victim,5000)

#Histogram
gg_func("per_victim", avg_per, "Relative Frequency Histogram")

#Probability Plot
p <- ggplot(avg_per, aes(x=per_victim)) +
  geom_histogram(aes(y=..density..),      
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  xlab("Value") + 
  ylab("Frequency") + 
  ggtitle("Probability Plot")
p


#Chi-square test
p.tri <- pltriangle(avg_per$per_victim)
chisq.test(avg_per$per_victim, p = p.tri, rescale.p = T)


#--------------------
#Problem 2 : 
#--------------------
results = NULL
for (k in 1:5000){
  Beth_Israel_Medical <- rnorm(1, 150, 50)*0.3
  Tufts_Medical <- rnorm(1, 150, 50)*0.15
  Massachusetts_General <- rnorm(1, 150, 50)*0.2
  Boston_Medical <- rnorm(1, 150, 50)*0.25
  Brigham_and_Women <- rnorm(1, 150, 50)*0.1
  total = min (Beth_Israel_Medical, Tufts_Medical, Massachusetts_General, Boston_Medical, Brigham_and_Women)
  results = rbind(results, data.frame(Beth_Israel_Medical, Tufts_Medical, Massachusetts_General, Boston_Medical, Brigham_and_Women, total))
}
head(results)
describe(results)
results <- abs(results)

gg_func("Beth_Israel_Medical", k = "Normal Distribution Histogram")
gg_func("Tufts_Medical", k = "Normal Distribution Histogram")
gg_func("Massachusetts_General", k = "Normal Distribution Histogram")
gg_func("Boston_Medical", k = "Normal Distribution Histogram")
gg_func("Brigham_and_Women", k = "Normal Distribution Histogram")

result_time <- data.frame(Beth_Israel_Medical_time = ((results$Beth_Israel_Medical*(7*2/60))+2), 
                          Tufts_Medical_time = ((results$Tufts_Medical*(10*2/60))+4), 
                          Massachusetts_General_time = ((results$Massachusetts_General*(15*2/60))+3),
                          Boston_Medical_time = ((results$Boston_Medical*(15*2/60))+5), 
                          Brigham_and_Women_time = ((results$Brigham_and_Women*(15*2/60))+5), 
                          total_time = ((results$total*(7*2/60))+2))
#Law of large number
avg <- 0
n <- 0
for (k in 1:5000) {
  x <- mean(sample(results$Beth_Israel_Medical, k, replace = TRUE))
  avg[k] = x
  n[k] = k
  avg_beth <- data.frame(avg, n)
}

#Plot
p <- ggplot(avg_beth, aes(x=n, y=avg)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm, se=FALSE) +
  xlab("Sample Size") +
  ylab("Sample Mean") +
  ggtitle("Plot")
p


#C.I.
ci <- function(alpha,m,n){
  alpha <- (1-(alpha/100))
  conf <- 1-alpha
  me <- qt(conf, df = n - 1)*(sd(m)/sqrt(n))
  cat("Lower Bound", mean(m) - me, '\n')
  cat("True Mean", mean(m), '\n')
  cat("Uper Bound", mean(m) + me, '\n')
  cat("Margin Of Error", me, '\n')
}
ci(95,result_time$Beth_Israel_Medical_time,5000)

#Histogram
gg_func("Beth_Israel_Medical_time", result_time, "Relative Frequency Histogram")

#Probability Plot
p <- ggplot(result_time, aes(x=Beth_Israel_Medical_time)) +
  geom_histogram(aes(y=..density..),      
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  xlab("Value") + 
  ylab("Frequency") + 
  ggtitle("Probability Plot")
p

fit <- fitdistr(result_time$Beth_Israel_Medical_time,"normal")

#Chi-square test
p.norm <- pnorm(result_time$Beth_Israel_Medical_time)
chisq.test(abs(result_time$Beth_Israel_Medical_time), p = p.norm, rescale.p = T)


#problem 5 with t
t_prob <- function(m,n){
  data.frame((m/n * 60)/m) 
}
avg_per <- data.frame(t_prob(results$Beth_Israel_Medical, result_time$Beth_Israel_Medical_time),
                      t_prob(results$Tufts_Medical, result_time$Tufts_Medical_time),
                      t_prob(results$Massachusetts_General, result_time$Massachusetts_General_time),
                      t_prob(results$Boston_Medical, result_time$Boston_Medical_time),
                      t_prob(results$Brigham_and_Women, result_time$Brigham_and_Women_time))
colnames(avg_per) <- c("Beth_Israel_Medical", "Tufts_Medical", "Massachusetts_General", "Boston_Medical", "Brigham_and_Women")
for (i in n) {
  avg_per$per_victim[i] <- sum(avg_per$Beth_Israel_Medical[i], avg_per$Tufts_Medical[i], avg_per$Massachusetts_General[i], avg_per$Boston_Medical[i], avg_per$Brigham_and_Women[i])  
}

ci(95,avg_per$per_victim,5000)

#Histogram
gg_func("per_victim", avg_per, "Relative Frequency Histogram")

#Probability Plot
p <- ggplot(avg_per, aes(x=per_victim)) +
  geom_histogram(aes(y=..density..),      
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  xlab("Value") + 
  ylab("Frequency") + 
  ggtitle("Probability Plot") 
p

fit <- fitdistr(avg_per$per_victim, 'normal')

#Chi-square test
p.norm <- pnorm(abs(avg_per$per_victim))
chisq.test(avg_per$per_victim, p = p.exp, rescale.p = T)
