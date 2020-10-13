                                  #Hypothesis Testing
#_______________________________________________________________________________________#
#A. Package MASS
install.packages("MASS")
library(MASS)

#_______________________________________________________________________________________#
#B. Chem dataset One Sample t-test
chem
chem <- data.frame(chem)
t.test(chem, mu = 1, alternative = "greater")

#_______________________________________________________________________________________#
#C. Cats dataset Two Sample t-test
cats
male_cats <- subset(cats, Sex == "M")
female_cats <- subset(cats, Sex == "F") 
t.test(male_cats$Bwt, female_cats$Bwt, alternative = "two.sided")
cats

#_______________________________________________________________________________________#
#D. Shoes Dataset Paired t-test
shoes
shoes <- data.frame(shoes)
t.test(shoes$A, shoes$B, alternative = "greater", paired = T)

#_______________________________________________________________________________________#
#E. Bacteria data set test of equal or given proportions
a <- table(bacteria$hilo, bacteria$ap)
prop.test(a, length(a), alternative = "two.sided", conf.level = 0.95, correct = TRUE)

#_______________________________________________________________________________________#
#F Cats data set f-test
var.test(male_cats$Bwt, female_cats$Bwt)




                                      #Inferential Statistics Assignment
#_______________________________________________________________________________________#
forestfires <- read.csv(file.choose())
View(forestfires)
write.csv(forestfires, file = "forestfire.csv")
#_______________________________________________________________________________________#
#One sample t-test for temperature greater than 18 degree celsius
t.test(forestfires$temp, mu =18, alternative = "greater")
#_______________________________________________________________________________________#
#Two sample t-test wind speed equal to 4km/h
Month_Aug <- forestfires[which(forestfires$month=='aug'),]
Month_Sep <- forestfires[which(forestfires$month=='sep'),]
t.test(Month_Aug$wind, Month_Sep$wind, mu=4, alternative = "two.sided")

#_______________________________________________________________________________________#
#Paired t-test for temperature equal to 9 degree celsius 
Month_Apr <- forestfires[which(forestfires$month=='apr'),]
Month_Dec <- forestfires[which(forestfires$month=='dec'),]
t.test(Month_Apr$temp, Month_Dec$temp, mu=9, alternative = "two.sided", paired = T)
#_______________________________________________________________________________________#
#Test of equal or given proportions
forestfires$rainfall <- ifelse(forestfires$rain == 0, 'n', 'y')
temp_rainfall <- table(Month_Aug$month, Month_Aug$rainfall)
temp_rainfall <- as.matrix(temp_rainfall[c('aug'),])
colnames(temp_rainfall) <- c('aug')
temp_rainfall <- t(temp_rainfall)
prop.test(temp_rainfall, length(temp_rainfall), alternative = "two.sided", conf.level = 0.95, correct = TRUE)

#_______________________________________________________________________________________#
#f-test
var.test(Month_Aug$temp, Month_Sep$temp)
