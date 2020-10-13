  #Week-2
mydata <- read.csv(file.choose())
x <- mydata
  a <- as.data.frame(lapply(x, function(y) gsub("<Unanswered>", NA, y)))
View(a)

b <- a[seq(13, 21,2)]
c <- lapply(b, as.character)
new.function <- function(o){
  gsub("[a-zA-Z ]", "", o) 
  gsub("[[:punct:]]", "", o)
  gsub("[^[:alnum:][:blank:]+?&/\\-]", "", o)
  gsub("[^0-9A-Za-z///' ]","" , o ,ignore.case = TRUE)
  u <- lapply(o, as.integer)
  k <- ifelse(u < 6 & u > 0, u, "")
  z <- lapply(k, as.numeric)
}

d <- lapply(c, new.function)
View(d)

table.function <- function(l){
  h <- table(unlist(l))
  i <- as.data.frame(h)
}

f <- table.function(a$Answer1) 
names(f)[1] <- 'Answer1'
g <- table.function(a$Answer2) 
names(g)[1] <- 'Answer2'
h <- table.function(a$Answer3) 
names(h)[1] <- 'Answer3'
i <- table.function(a$Answer4) 
names(i)[1] <- 'Answer4'
j <- table.function(d$Answer5) 
names(j)[1] <- 'Answer5'
k <- table.function(d$Answer6) 
names(k)[1] <- 'Answer6'
l <- table.function(d$Answer7) 
names(l)[1] <- 'Answer7'
m <- table.function(d$Answer8) 
names(m)[1] <- 'Answer8'
n <- table.function(d$Answer9) 
names(n)[1] <- 'Answer9'


x <- barplot(as.numeric(unlist(g$Freq)), names.arg = g$Answer2, main = "Number Of Responses For Age Of Students", xlim = c(0, 5), ylim = c(0, 50), xlab = "Age Of Students", ylab = "Number Of Responses", space = 0.1, width = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), col = "red")
text(x = x, y = as.numeric(unlist(g$Freq)), label = as.numeric(unlist(g$Freq)), pos = 3, cex = 0.8, col = "blue")
x <- barplot(as.numeric(unlist(h$Freq)), names.arg = h$Answer3, main = "Number Of Responses For Years Of Work Experience", xlim = c(0, 5), ylim = c(0, 50), xlab = "Years Of Experience", ylab = "Number Of Responses", space = 0.1, width = c(0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3), col = "green")
text(x = x, y = as.numeric(unlist(h$Freq)), label = as.numeric(unlist(h$Freq)), pos = 3, cex = 0.8, col = "blue")
x <- barplot(as.numeric(unlist(i$Freq)), names.arg = i$Answer4, main = "Number Of Responses For Years Spent Studying Analytics", xlim = c(0, 5), ylim = c(0, 50), xlab = "Years Spent Studying Analytics", ylab = "Number Of Responses", space = 0.1, width = c(0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3), col = "orange")
text(x = x, y = as.numeric(unlist(i$Freq)), label = as.numeric(unlist(i$Freq)), pos = 3, cex = 0.8, col = "blue")
x <- barplot(as.numeric(unlist(j$Freq)), names.arg = j$Answer5, main = "Number Of Responses For R Programming Expertise", xlim = c(0, 5), ylim = c(0, 80), xlab = "R Programming Expertise", ylab = "Number Of Responses", space = 0.1, col = "black")
text(x = x, y = as.numeric(unlist(j$Freq)), label = as.numeric(unlist(j$Freq)), pos = 3, cex = 0.8, col = "blue")
x <- barplot(as.numeric(unlist(k$Freq)), names.arg = k$Answer6, main = "Number Of Responses For Central Tendency : (Mean, median, mode) Expertise", xlim = c(0, 5), ylim = c(0, 50), xlab = "Central Tendency : (Mean, median, mode) Expertise", ylab = "Number Of Responses", space = 0.1, width = c(0.7,0.7,0.7,0.7,0.7), col = "violet")
text(x = x, y = as.numeric(unlist(k$Freq)), label = as.numeric(unlist(k$Freq)), pos = 3, cex = 0.8, col = "blue")
x <- barplot(as.numeric(unlist(l$Freq)), names.arg = l$Answer7, main = "Number Of Responses For Measures of Variability: (range, variance, standard deviation) Expertise", xlim = c(0, 5), ylim = c(0, 50), xlab = "Measures of Variability: (range, variance, standard deviation) Expertise", ylab = "Number Of Responses", space = 0.1, width = c(0.7,0.7,0.7,0.7,0.7), col = "yellow")
text(x = x, y = as.numeric(unlist(l$Freq)), label = as.numeric(unlist(l$Freq)), pos = 3, cex = 0.8, col = "blue")
x <- barplot(as.numeric(unlist(m$Freq)), names.arg = m$Answer8, main = "Number Of Responses For Bivariate Concepts: (covariance, correlation) Expertise", xlim = c(0, 5), ylim = c(0, 50), xlab = "Bivariate Concepts: (covariance, correlation) Expertise", ylab = "Number Of Responses", space = 0.1, width = c(0.7,0.7,0.7,0.7,0.7), col = "blue")
text(x = x, y = as.numeric(unlist(m$Freq)), label = as.numeric(unlist(m$Freq)), pos = 3, cex = 0.8, col = "blue")
x <- barplot(as.numeric(unlist(n$Freq)), names.arg = n$Answer9, main = "Number Of Responses For Probabilities and distributions Expertise", xlim = c(0, 5), ylim = c(0, 50), xlab = "Probabilities and distributions Expertise", ylab = "Number Of Responses", space = 0.1, width = c(0.7,0.7,0.7,0.7,0.7), col = "aliceblue")
text(x = x, y = as.numeric(unlist(n$Freq)), label = as.numeric(unlist(n$Freq)), pos = 3, cex = 0.8, col = "blue")



#Cross-Tabulation
df <- as.data.frame(table(a$Answer2, unlist(d$Answer5)))
names(df)[1] <- 'Undergraduate Degree'
names(df)[2] <- 'Age'
df <- as.data.frame(table(a$Answer1, a$Answer2))
df = df[!(df$Freq == 0), ]
df
write.table(df, "cross-table.csv", sep = ",", col.names = !file.exists("cross-table.csv"), append = T)


#Week-3

fin<-data.frame(Answer_5=unlist(d$Answer5),Answer_6=unlist(d$Answer6),Answer_7=unlist(d$Answer7),Answer_8=unlist(d$Answer8),Answer_9=unlist(d$Answer9))
View(fin)
q1<-a[seq(1,12)]
View(q1)
b1<- data.frame(q1,Answer_5=unlist(d$Answer5),Question_6=a$Question6,Answer_6=unlist(d$Answer6),Question_7=a$Question7,Answer_7=unlist(d$Answer7),Question_8=a$Question8,Answer_8=unlist(d$Answer8),Question_9=a$Question9,Answer_9=unlist(d$Answer9))
View(b1)
str(b1)
summary(b1)

install.packages("psych",dependencies = TRUE)
library(psych)
table_b1 <- psych::describe(b1)
table_b1 <- table_b1[c("mean","sd","min","max","n")]
table_b1 <- table_b1[c(-1, -2, -3, -4),]
table_b1
write.csv(table_b1,"table_b1.csv")



#stats for class 1

table1 <- psych::describeBy(b1, group = list(b1$Class))
class(table1)
x_1 <- table1[[1]]
x_1 <- x_1[c(-1, -2, -3, -4),]
x_1 <- x_1[,c("n", "mean", "median", "min", "max", "sd")]
write.csv(x_1,"Class_1.csv")

#stats for class 2

table2 <- psych::describeBy(b1, group = list(b1$Class))
class(table2)
x_2 <- table2[[2]]
x_2 <- x_2[c(-1, -2, -3, -4),]
x_2 <- x_2[,c("n", "mean", "median", "min", "max", "sd")]
write.csv(x_2,"Class_2.csv")

#stats for Computer Science 

table3 <- psych::describeBy(b1, group = list(b1$Answer1))
class(table3)
x_3 <- table3[["Computer/IT"]]
x_3 <- x_3[c(-1, -2, -3, -4),]
x_3 <- x_3[,c("n", "mean", "median", "min", "max", "sd")]
write.csv(x_3,"Comp.csv")
b1
#stats for Economics

table4 <- psych::describeBy(b1, group = list(b1$Answer1))
class(table4)
x_4 <- table4[["Economics"]]
x_4 <- x_4[c(-1, -2, -3, -4),]
x_4 <- x_4[,c("n", "mean", "median", "min", "max", "sd")]
write.csv(x_4,"Economics.csv")

#t-test
#Twosample 
t.test(b1$Answer_5, b1$Answer_9, mu =3, alternative = c("two.sided"),conf.level=0.95)

#Onesample 
t.test(b1$Answer_5, mu = 3, alternative = c("less"),conf.level=0.95)
t.test(b1$Answer_5, mu = 3, alternative = c("greater"),conf.level=0.95)
t.test(b1$Answer_5, mu = 3, alternative = c("two.sided"),conf.level=0.95)

#Plots
library(ggplot2)
View(b1)
plot1 <- ggplot(data = b1, mapping = aes(x = b1$Answer2, y = b1$Answer3)) +
  geom_point(color = "darkblue") +
  xlab("Age") +
  ylab("Work Experience") +
  ggtitle("Age VS Workexperience")
print(plot1)

plot2 <- ggplot(data = b1, mapping = aes(x = b1$Answer2, y = b1$Answer4)) +
  geom_point(color = "Red") +
  xlab("Age") +
  ylab("Experience in Statistics/Analytics") +
  ggtitle("Age VS Experience in Statistics/Analytics")
print(plot2)

plot3 <- ggplot(data = b1, mapping = aes(x = b1$Answer2, y = b1$Answer1)) +
  geom_point(color = "green") +
  xlab("Age") +
  ylab("Undergraduate") +
  ggtitle("Age VS Undergraduate")
print(plot3)

#jitter plot
plot4 <- ggplot(data = b1, mapping = aes(x = b1$Answer1, y = b1$Answer_5)) +
  geom_jitter(color = "darkgreen") +
  xlab("Undergraduate Degree") +
   ylab("R Expertise") +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=90, hjust=1)) +
  ggtitle("Undergraduate Degree VS R Expertise")
print(plot4)

plot5 <- ggplot(data = b1, mapping = aes(x = b1$Answer4, y = b1$Answer_9)) +
  geom_jitter(color = "brown") +
  xlab("Experience in Statistics/Analytics") +
  ylab("Expertise in Probability") +
  ggtitle("Experience in Statistics/Analytics VS Expertise in Probability")
print(plot5)

#box plot 
plot6 <- ggplot(data = a, aes(x = factor(b1$Answer_5), y = b1$Answer_9)) +
  geom_boxplot(fill='#A4A4A4') +
  ggtitle("Box Plot for R Expertise vs Probablity and Distribution Expertise") +
  xlab("R Expertise") + 
  ylab("Probability and Distribution Expertise")+
  ggtitle("R Expertise VS Probability and Distribution Expertise")
print(plot6)


#week 4

class_1 <- b1[ which(b1$Class==1),]
class_2 <- b1[ which(b1$Class==2),]
class_3 <- b1[ which(b1$Class==3),]
class_4 <- b1[ which(b1$Class==4),]


t.test(class_1$Answer_5, class_2$Answer_5, mu = 3, paired = T)
t.test(class_1$Answer_7, class_2$Answer_7, mu = 3, paired = T)
t.test(class_1$Answer_8, class_2$Answer_8, mu = 3, paired = T)

p2<- ggplot(b1, aes(b1$Answer_5)) + 
  geom_histogram(fill = "white", color = "grey30", binwidth = 1) +
  xlab("r_expertise") +
  ylab("Number of responses") +
  ylim(c(0,60)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("R_expertise")
print(p2)

#Week 5
w <- data.frame(b1$Answer_5, b1$Answer_6, b1$Answer_7, b1$Answer_8, b1$Answer_9)
names(w)[1] <- 'R expertise'
names(w)[2] <- 'Central Tendency'
names(w)[3] <- 'Measures of variability'
names(w)[4] <- 'Bivariate Concepts'
names(w)[5] <- 'Probabilities And Distribution'
w <- na.omit(w)
w <- cor(w)
write.csv(w, file = "correlation.csv")
library(ggcorrplot)
install.packages("ggcorrplot")
ggcorrplot(w, type = "lower", lab = TRUE) + ggtitle("Correlation Plot")



#regression table
# linear regression table
head(b1)
View(b1)
m1 <- lm(data=l,  Age ~ Work_Experience + Years_studied_Analytics) 
summary(m1)
write.csv(m1, "regression.csv")
plot(y=l$Age, x=l$Work_Experience, xlab = "Work Experience", ylab = "Age", main = "Age VS Work Experience")
abline(m1)


