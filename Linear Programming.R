#Linear Programming 

#--------
#Problem 1
#--------

library(lpSolve)

f.max.obj <- c(20.5,34,42)


f.con <- matrix(c(0.4, 0.7, 0.8,
                0.6, 1, 1.2,
                1.4, 2.6, 3.1,
                1, 0, 0,
                0, 1, 0,
                0, 0, 1,
                1, 0, 0,
                0, 1, 0,
                0, 0, 1), ncol = 3, byrow = TRUE)

f.dir <- c("<=",
           "<=",
           "<=",
           ">=",
           ">=",
           ">=",
           "<=",
           "<=",
           "<=")

f.rhs <- c(23400,
           23400,
           46800,
           14000,
           6200,
           2600,
           21000,
           12500,
           4200)

a <- lp("max", f.max.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)
c <- a$solution
f <- f.max.obj * c
cat(sprintf("Number of Compressor for Small Unit=\"%s\"
            \nNumber of Compressor for Medium Unit=\"%s\"
            \nNumber of Compressor for Large Unit=\"%s\" ", round(c[1], 0), c[2], c[3]))
cat(paste0("Total Profit: $", round(sum(f),2)))


#--------
#Problem 2
#--------
f.obj <- c(2.15 ,2.10 ,1.75 ,1.50 ,1.20 ,0.65,
           1.95 ,2.00 ,1.70 ,1.53 ,1.10 ,0.55,
           1.70 ,1.85 ,1.50 ,1.41 ,0.95 ,0.40,
           0.60 ,0.55 ,0.35 ,0.60 ,0.40 ,0.95,
           0.90 ,0.80 ,0.35 ,0.15 ,0.60 ,0.5,
           0.60 ,1.05 ,0.60 ,0.50 ,0.25 ,0.30,
           0.40 ,0.95 ,0.70 ,0.70 ,0.35 ,0.40,
           1.00 ,1.10 ,1.35 ,1.60 ,1.60 ,1.70)
m <- 4
n <- 12
f.con <- matrix(0, n+m, n*m)
for (i in 1:m){
  for (j in 1: n){
    f.con[i , n*(i-1) + j] <- 1
    f.con[m +j , n*(i-1) + j] <- 1
  }
}

f.dir <- c(rep("<=", m) , rep(">=", n))

f.rhs <- c(40000,
           35000,
           15000,
           16000,
           5000,
           16000,
           4200,
           3700,
           4500,
           7500,
           3000,
           9000,
           3300,
           12000,
           9500,
           16000)


a <- lp("min", f.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)
cat(paste0("Total Shipping Cost: $", a$objval))

Atlanta <- c(a$solution[1:12], sum(a$solution[1:12]))
Lexington <- c(a$solution[13:24], sum(a$solution[13:24]))
Milwaukee <- c(a$solution[25:36], sum(a$solution[25:36]))
Salt_Lake_City <- c(a$solution[37:48], sum(a$solution[37:48]))
data <- data.frame(`Atlanta` = Atlanta, `Lexington` = Lexington, `Milwaukee` = Milwaukee, `Salt_Lake_City` = Salt_Lake_City)
for(i in 1:length(Atlanta)){
  data$Demand_Met[i] <- sum(Atlanta[i], Lexington[i], Milwaukee[i], Salt_Lake_City[i])
}
data <- as.matrix(data)
data <- t(data)
j <- c("Seattle",	"San Francisco",	"Las Vegas", "Tuscon", "Denver",	
       "Charlotte",	"Minneapolis",	"Fayetteville",	"Birmingham",	
       "Orlando",	"Cleveland", "Philadelphia",	"TOTAL SHIPPED")
colnames(data) <- j
print(data)




