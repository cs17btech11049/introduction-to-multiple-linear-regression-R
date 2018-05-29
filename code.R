# 1.1

before <- c(370, 385, 375, 380, 378)
after <- c(387, 397, 380, 392, 389)

m1 <- round(mean(before),2)
mx1 <- round(mean(after),2)

s1 <- round(sd(before),2)
sx1 <- round(sd(after),2)

z <- round((mx1 - m1) / sx1,2)

one_sided_high <- round(pnorm(abs(z), lower.tail = F),2)


t.test(after, before, paired=T, alternative = "greater", conf.level = 0.95)

# 1.2

a <- 0.5 
pbar <- 110/200           
p0 <- 0.5                
n <- 200                  
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) 
z

pval <- 2*pnorm(z, lower.tail=FALSE)  
pval  

# 1.3

s <- 230
n <- 16
mx <- 1020
a <- 0.02
df <- n - 1
sx <- round(s/sqrt(n))

error <- round(qnorm(0.99)*sx/sqrt(n))
left <- a-error
right <- a+error
left
right
mx+left
mx+right

# 1.4

sx <- 275
n <- 16
mx <- 1020
a <- 0.02
df <- n - 1

error <- round(qnorm(0.99)*sx/sqrt(n))
left <- a-error
right <- a+error
left
right
mx+left
mx+right

# 1.5

pbar <- 476/500      
p0 <- 0.9          
n <- 500                
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) 

pval <- pnorm(z) 
pval

# 1.6

library(ggplot2)
g <- rgamma(100, shape = 1,  scale = 1)
e <- rexp(100, rate = 1)
plot(density(g))
plot(density(e))

t.test(g,e, var.equal=TRUE, paired=FALSE)

# 1.7

scores <- c(92, 83, 95, 96, 85, 61, 76, 80, 92, 87)

t.test(scores, mu=82, conf.level = 0.95)

# 2.1 Poisson

samples <- 10000
size <- 1000

set.seed(0)
x1 <- replicate(samples, rpois(n=size, lambda = 2.7))

hist(x1)

means <- colMeans(x1)

plot(density(means))

mean(means)

ks.test(means,"pnorm",mean(means),sd(means))
qqnorm(means)
qqline(means)

# 2.2 Binomial

samples <- 10000
size <- 1000

set.seed(0)
x2 <- replicate(samples, rbinom(n=size, size=20, prob = 0.3))


hist(x2)

means <- colMeans(x2)

plot(density(means))

mean(means)

ks.test(means,"pnorm",mean(means),sd(means))
qqnorm(means)
qqline(means)

# 3.1 

anova_data <- read.csv("./anova_data.csv")

anova <- anova_data
str(anova)
names(anova)
anova <- as.data.frame(anova)

# 3.2

colSums(sapply(anova, is.na))
anovac <- anova[complete.cases(anova["employrate"]),]
colSums(sapply(anovac, is.na))
cemp <- aggregate(anovac$employrate, by=list(Category=anovac$continent), FUN=mean)
names(cemp)
colnames(cemp) <- c("continent", "employrate")
cemp[, 2] <- sapply(cemp[, 2], as.numeric)


library(ggplot2) 
ggplot(cemp, aes(x=factor(continent), y=employrate)) + stat_summary(fun.y="mean", geom="bar")

# 3.3

ow_anova <- aov(employrate ~ continent, anovac)
summary(ow_anova)
anova(ow_anova)
confint(ow_anova)

anovafact <- anovac
require(ggplot2)
 
ggplot(anovafact, aes(x = continent, y = employrate)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Continent") +
  ylab("Employrate")


anovamod <- aov(employrate ~ continent, anovafact)
anovamod
summary(anovamod)
anova(anovamod)
confint(anovamod)

# 3.4

tuk<- TukeyHSD(anovamod)
tuk

plot (tuk)

# 3.5

bartlett.test(employrate ~ continent, data=anovac)

fligner.test(employrate ~ continent, data=anovac)

# 3.6

ggplot(anovafact, aes(x = continent, y = incomeperperson)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Continent") +
  ylab("Incomeperperson")
  
ggplot(anovafact, aes(x = continent, y = femaleemployrate)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Continent") +
  ylab("femaleemployrate")
  
ggplot(anovafact, aes(x = continent, y = urbanrate)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Continent") +
  ylab("urbanrate") 
  
plot(anovafact$urbanrate~anovafact$employrate)
abline(test <- lm(anovafact$urbanrate~anovafact$employrate, data = anovafact))

# 4.1

housing <- read.table("./housing.data", quote="\"", comment.char="")

# 4.2

colnames(housing) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS",  "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

# 4.3

model1 <- lm(MEDV ~ ., data = housing)

housing2 <- housing
sapply(housing2,class)

scale2 = scale(housing2[,c( "MEDV")])
housing2[,c("MEDV")] <- scale2
model2 <- lm(MEDV ~ ., data = housing2)
model3 <- lm(log(MEDV) ~ ., data = housing)

plot(model3)
hist(housing$MEDV)
qqnorm(housing$MEDV)
qqline(housing$MEDV)

#log(medv) improves things
qqnorm(log(housing$MEDV))

model1 <- lm(log(MEDV) ~ ., data = housing)
summary(model1)
model2 <- lm(log(MEDV) ~. -INDUS -AGE, data = housing)
summary(model2)

library(car)
vif(model2)
#There are multicollinearity issues TAX>5

cor(housing)
#TAX is higly correlated to CRIM 0.58, INDUS 0.72, NOX 0.66, RAD 0.91 and others
#we will not include TAX in the model

model3 <- lm(log(MEDV) ~. -INDUS -AGE -TAX, data = housing)
#summary(model3)
model4 <- lm(log(MEDV) ~. -ZN -INDUS -AGE -TAX, data = housing)
#summary(model4)
#anova(model4)
model5 <- lm(log(MEDV) ~. -RAD -ZN -INDUS -AGE -TAX, data = housing)
#anova(model5)
#anova(model4,model5)
#plot(model5)
model6 <- lm(log(MEDV) ~. -RAD -INDUS -AGE -TAX, data = housing)
#summary(model6)

model7 <- lm(MEDV ~. -RAD -INDUS -AGE -TAX, data = housing)
summary(model7)
plot(model7)

mean(model7$residuals)
hist(model7$residuals, xlab="residuals", main="Histogram of residuals")

qqnorm(model7$residuals, main="Normal Probability Plot", pch=19)
qqline(model7$residuals)

#plot residuals against fitted values (Y-hat)
plot(model7$fitted.values, model7$residuals, main="Residuals vs. Fitted", xlab="Fitted values", ylab="Residuals", pch=19)
abline(h=0)

# 4.6

mfull<-lm(MEDV ~. -RAD -INDUS -AGE -TAX, data = housing)
mnull<-lm(MEDV ~1, data = housing)
summary(step(mnull, scope=list(lower=mnull,upper=mfull), direction='both'))

summary(step(mfull, direction='both'))

model8 <- lm(MEDV ~ LSTAT + RM + PTRATIO + DIS + NOX + CHAS + 
    B + ZN + CRIM, data = housing)
summary(model8)
summary(mfull)

# 4.8

plot(mfull, 3)
residuals <- mfull$residuals
fitted_values <- mfull$fitted.values
train_ids <- rownames(mfull)
large_residuals <- ifelse(abs(residuals) > 150,train_ids,'')
hist(residuals)
library(ggplot2)

par(mfrow=c(1,1))
qqnorm(residuals, main = "Normal Q-Q Plot for Housing data set")
qqline(residuals)

cooksd <- cooks.distance(mfull)
#we clearly have outliers
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(housing[influential, ])

