myproject<-read.csv("C:/insurance.csv", header=TRUE, sep=",")
options(max.print = 1500)
myproject

install.packages("tibble")
library("tibble")
myproject$sex <- as.numeric(factor(myproject$sex, levels = c("male","female"), labels = c(1,2)))
myproject

myproject$smoker <- as.numeric(factor(myproject$smoker, levels = c("no","yes"), labels = c(1,2)))
myproject

myproject$region <- as.numeric(factor(myproject$region, levels = c("southeast","northeast","southwest","northwest"), labels = c(1,2,3,4)))
myproject


age_charge <- cor.test(myproject$age, myproject$charges, method = "pearson")
age_charge

sex_charge <- cor.test(myproject$sex, myproject$charges, method = "pearson")
sex_charge

bmi_charge <- cor.test(myproject$bmi, myproject$charges, method = "pearson")
bmi_charge

children_charge <- cor.test(myproject$children, myproject$charges, method = "pearson")
children_charge

smoker_charge <- cor.test(myproject$smoker, myproject$charges, method = "pearson")
smoker_charge

region_charge <- cor.test(myproject$region, myproject$charges, method = "pearson")
region_charge



hist(myproject$age, main = "Age", xlab = "Age")

hist(myproject$bmi, main = "Bmi", xlab = "Bmi")

hist(myproject$children, main = "Children", xlab = "Children")

hist(myproject$charges, main = "Charges", xlab = "Charges")




sex <- table(myproject$sex)
barplot(sex, main="Sex", xlab="Sex")

sm <- table(myproject$smoker)
barplot(sm, main="Smoker", xlab="Smoker")

reg <- table(myproject$region)
barplot(reg, main="Region", xlab="Region")




a<- boxplot.stats(myproject$age)$out
boxplot(myproject$age,
        ylab = "age",
        main = "Boxplot for Age"
)
mtext(paste("Outliers: ", paste(a, collapse = ", ")))


b<- boxplot.stats(myproject$bmi)$out
boxplot(myproject$bmi,
        ylab = "bmi",
        main = "Boxplot for Bmi"
)
mtext(paste("Outliers: ", paste(b, collapse = ", ")))


c<- boxplot.stats(myproject$children)$out
boxplot(myproject$children,
        ylab = "children",
        main = "Boxplot for Children"
)
mtext(paste("Outliers: ", paste(c, collapse = ", ")))


d<- boxplot.stats(myproject$charges)$out
boxplot(myproject$charges,
        ylab = "charges",
        main = "Boxplot for Charges"
)
mtext(paste("Outliers: ", paste(d, collapse = ", ")))




plot(myproject$age, myproject$charges, 
     xlab = "Age", ylab = "Charges",
     main = "Scatterplot of Age vs Charges")
abline(lm(myproject$charges ~ myproject$age), col = "blue")


plot(myproject$sex, myproject$charges, 
     xlab = "Sex", ylab = "Charges",
     main = "Scatterplot of Sex vs Charges")
abline(lm(myproject$charges ~ myproject$sex), col = "blue")


plot(myproject$bmi, myproject$charges,
     xlab = "Bmi", ylab = "Charges",
     main = "Scatterplot of Bmi vs Charges")
abline(lm(myproject$charges ~ myproject$bmi), col = "blue")


plot(myproject$children, myproject$charges,
     xlab = "Children", ylab = "Charges",
     main = "Scatterplot of Children vs Charges")
abline(lm(myproject$charges ~ myproject$children), col = "blue")


plot(myproject$smoker, myproject$charges,
     xlab = "Smoker", ylab = "Charges",
     main = "Scatterplot of Smoker vs Charges")
abline(lm(myproject$charges ~ myproject$smoker), col = "blue")


plot(myproject$region, myproject$charges,
     xlab = "Region", ylab = "Charges",
     main = "Scatterplot of Region vs Charges")
abline(lm(myproject$charges ~ myproject$region), col = "blue")



install.packages("ggplot2")
library(ggplot2)

region = c("southeast", "southwest", "northeast", "northwest", "southeast")
charges = c(23456, 34567, 45678, 56789, 67890)
ggplot(myproject, aes(x = region, y = charges, fill = region)) + 
  geom_violin(trim = FALSE, scale = "count") +
  stat_summary(fun.y = "median", geom = "point", size = 2, color = "orange") +
  scale_fill_manual(values = c("southeast" = "blue", "southwest" = "red", "northeast" = "yellow", "northwest" = "green")) +
  labs(x = "Region", y = "Charges") +
  theme_minimal()


smoker = c("no", "yes")
charges = c(23456, 34567)
ggplot(myproject, aes(x = smoker, y = charges, fill = smoker)) + 
  geom_violin(trim = FALSE, scale = "count") +
  stat_summary(fun.y = "median", geom = "point", size = 2, color = "orange") +
  scale_fill_manual(values = c("no" = "blue", "yes" = "red")) +
  labs(x = "Smoker", y = "Charges") +
  theme_minimal()


sex = c("male", "female")
charges = c(23456, 34567)
ggplot(myproject, aes(x = sex, y = charges, fill = sex)) + 
  geom_violin(trim = FALSE, scale = "count") +
  stat_summary(fun.y = "median", geom = "point", size = 2, color = "orange") +
  scale_fill_manual(values = c("male" = "blue", "female" = "green")) +
  labs(x = "Sex", y = "Charges") +
  theme_minimal()




ggplot(myproject, aes(x = age, y = charges)) +  
  geom_line() +  
  labs(x = "Age", y = "Charges", title = "Age vs Charges")


ggplot(myproject, aes(x = sex, y = charges)) +  
  geom_line() +  
  labs(x = "Sex", y = "Charges", title = "Sex vs Charges")


ggplot(myproject, aes(x = bmi, y = charges)) +  
  geom_line() +  
  labs(x = "Bmi", y = "Charges", title = "Bmi vs Charges")


ggplot(myproject, aes(x = children, y = charges)) +  
  geom_line() +  
  labs(x = "Children", y = "Charges", title = "Children vs Charges")


ggplot(myproject, aes(x = smoker, y = charges)) +  
  geom_line() +  
  labs(x = "Smoker", y = "Charges", title = "Smoker vs Charges")


ggplot(myproject, aes(x = region, y = charges)) +  
  geom_line() +  
  labs(x = "Region", y = "Charges", title = "Region vs Charges")




