library(readxl)
dat = read_excel("SportingLifeFull3.xlsx")
model = glm(Incomplete ~ factor(Track)+factor(Season)+factor(TypeofFence)+
              factor(DistanceCat)+factor(GroundConditions)+factor(Runners)+factor(Age)+factor(Weight), data= dat)
summary(model)
plot(model$coefficients)
coef = model$coefficients
coef = coef[-1]
names(coef)[1:21] <- str_sub(names(coef)[1:21],14)
names(coef)[22:24] <- str_sub(names(coef)[22:24],15)
names(coef)[25] <- str_sub(names(coef)[25],20)
names(coef)[26:28] <- str_sub(names(coef)[26:28],20)
names(coef)[29:31] <- str_sub(names(coef)[29:31],25)
names(coef)[32:35] <- str_sub(names(coef)[32:35],16)
names(coef)[36:44] <- str_sub(names(coef)[36:44],12)
names(coef)[45:47] <- str_sub(names(coef)[45:47],15)
expo = 100*(exp(coef)-1)
names =names(coef)
expo = as.data.frame(expo)
expo$expo[1]
effect = 100*(1 - expo$expo)

effects = cbind(names, effect)
plot(effects)
effect = as.data.frame(effect)
names = as.data.frame(names(coef))
effects = cbind(names, effect)
effects

library(stringr)
effects

?barplot
cols <- c("green", "red",'green','green','green','green','green','red','green','red','red','red',
          'green','green','green','red','red','green','red','red','red')
barplot(expo[1:21], las = 2, col = cols, ylim = c(-10,10), main = 'Bar Chart of the effect of Track Selection on Incompletion',
        ylab = '% effect when compared to Ballinrobe')
abline(h = 5, col = 'blue', lty = 2)
abline(h = -5, col = 'blue', lty = 2)
cols2 <- c("red", "green",'green','green','red','red','red','red','red','red','red','red',
          'red','red','green','red','red','red','red','red','red','red','red','green','green','green')
barplot(expo[22:47], las = 2, col = cols2, ylim = c(-10,10), main = 'Bar Chart of the effect of All Other Predictors on Incompletion',
        ylab = '% Effect when Compared with Base Case')
abline(h = 5,col = 'blue', lty = 2)
abline(h = -5, col = 'blue', lty = 2)
cols <- c("green", "red")[(effect$effect > 0) + 1] 
barplot(effect[26:28], las = 2, col = c('blue', 'red'), ylim = c(-10,10), main = 'Bar Chart of the effect of Track Selection on Incompletion')
barplot(effect[29:31], las = 2, col = 'blue', ylim = c(-10,10), main = 'Bar Chart of the effect of Track Selection on Incompletion')
barplot(effect[32:35], las = 2, col = 'blue', ylim = c(-10,10), main = 'Bar Chart of the effect of Track Selection on Incompletion')
barplot(effect[36:44], las = 2, col = 'blue', ylim = c(-10,10), main = 'Bar Chart of the effect of Track Selection on Incompletion')
barplot(effect[45:47], las = 2, col = 'blue', ylim = c(-10,10), main = 'Bar Chart of the effect of Track Selection on Incompletion')
model$effects
pchisq(3407.8, 29058)

age$levels = dat$Age
age$levels = c(age)
ageFactor <- as.numeric(age)

runners = dat$Runners
runners = c(runners)
runnersFactor <- as.factor(runners)

par(oma=c(2,0,0,0))  #so labels are not cut off  
barplot(table(runnersFactor),xlab = 'No. of Runners',ylab = "Frequency", main = "Distribution of No. of Runners",
        border="black", col="blue",las=2)
par(oma=c(2,0,0,0))  #so labels are not cut off  
barplot(table(ageLevels),xlab = 'Age',ylab = "Frequency", main = "Distribution of Age",
        border="black", col="blue",las=2)


probabilities <- predict.glm(model, interval = "prediction", level = .95)
max(probabilities)
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")

ageLevels = c('2','4','5','6','7','8','9','10','11','12')
rates = c('14.2','10.8','12.2','13.1','15.3','17.6','17.3','16.9','16.2','22.4')
plot(ageLevels, rates, xlab = 'Age',ylab = 'Incompletion Rate %', main = 'Age v Incompletion Rates',
     type = 'b', lty = 2, lwd = 3)


