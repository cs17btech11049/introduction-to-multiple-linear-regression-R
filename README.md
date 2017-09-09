# introduction-to-multiple-linear-regression-R

---
title: "Assignment 2"

author: "Eva Giannatou"

date: "December 20, 2016"

output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

##Question 1

- 1.1

H0: There is no increase in efficiency (the mean stays the same)

H1:  There is an increase in efficiency (the mean increases)

95% confidence interval

Suppose null hypothesis 

```{r}
before <- c(370, 385, 375, 380, 378)
after <- c(387, 397, 380, 392, 389)

m1 <- round(mean(before),2)
mx1 <- round(mean(after),2)

s1 <- round(sd(before),2)
sx1 <- round(sd(after),2)

z <- round((mx1 - m1) / sx1,2)

one_sided_high <- round(pnorm(abs(z), lower.tail = F),2)


t.test(after, before, paired=T, alternative = "greater", conf.level = 0.95)

```

The probability of getting a result at least so many standard deviations away is 0.03 < alpha=0.05. This result seems to favor the alternative hypothesis, which means that there is probably an efficiency increase.

Moreover according to T-test' p-value = 0.001991, we reject Ho. The mean increases when the gas additives are used.

- 1.2

H0: Equally divided opinion  
H1: Not equally divided opinion

Suppose null hypothesis 

```{r}

a <- 0.5 
pbar <- 110/200           
p0 <- 0.5                
n <- 200                  
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) 
z

pval <- 2*pnorm(z, lower.tail=FALSE)  
pval  


```
Pnorm function computes the two-tailed p-value of the test statistic. It doubles the upper tail p-value as the sample proportion is greater than the hypothesized value. Since p-value > 0.05 significance level, we do not reject the null hypothesis that p = 0.5.

- 1.3


```{r}
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
```
The true mean has a probability of 98% of being in the interval between 986 and 1054 squared hours assuming that the original random variable is normally distributed, and the samples are independent.

- 1.4 

```{r}
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
```
The true mean has a probability of 98% of being in the interval between 860 and 1180 squared hours assuming that the original random variable is normally distributed, and the samples are independent.

- 1.5

H0: at least 90% of the bulbs will bloom

H1: less than 90% of the bulbs will bloom

Suppose null hypothesis 

```{r}
pbar <- 476/500      
p0 <- 0.9          
n <- 500                
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) 

pval <- pnorm(z) 
pval
```

Since the p-value 0.99 > a = 0.05, we fail to reject H0.

- 1.6

H0: The two samples came from the same distribution

H1: They didn't come from the same distribution

Suppose null hypothesis 

```{r}
library(ggplot2)
g <- rgamma(100, shape = 1,  scale = 1)
e <- rexp(100, rate = 1)
plot(density(g))
plot(density(e))

t.test(g,e, var.equal=TRUE, paired=FALSE)
```

We obtained p-value greater than 0.05 and therefore we can conclude that the averages of two groups are significantly similar. This confirms that we can accept the null hypothesis H0. In this case the result is expected beacause the Gamma(1, b) is the Exponential distribution.

- 1.7

H0: m=82

H1: m!=82

Suppose null hypothesis 

```{r}
scores <- c(92, 83, 95, 96, 85, 61, 76, 80, 92, 87)

t.test(scores, mu=82, conf.level = 0.95)

```

Since the p-value 0.44 > a = 0.05, we fail to reject H0.

##Question 2

- 2.1 Poisson

```{r}
samples <- 10000
size <- 1000

set.seed(0)
x1 <- replicate(samples, rpois(n=size, lambda = 2.7))

hist(x1)

means <- colMeans(x1)

plot(density(means))
```

The density plot of the means seems to follow the normal ditribution. According to the central limit theorem the sampling distribution of the mean of any independent, random variable will be normal or nearly normal, if the sample size is large enough.

```{r}
mean(means)
```

The mean of the sample means is 2.7 which equals to lamda.This is expected, since in the Poisson distribution, the mean E[X]=lamda.

H0: sample means are normally distributed
H1: sample means are not normally distributed

suppose H0

```{r}
ks.test(means,"pnorm",mean(means),sd(means))
qqnorm(means)
qqline(means)
```

The ks.test eturns the P-value correspoding to the D statistic.The D statistic is the absolute max distance (supremum) between the CDFs of the two samples. The closer this number is to 0 the more likely it is that the two samples were drawn from the same.  distribution. 

The p-value equals to 0.36 > 0.05 and the maximum distance between the CDFs is really small. Therefore we fail to reject the null hypothesis that the sample means are normally distributed. 

- 2.2 Binomial

```{r}
samples <- 10000
size <- 1000

set.seed(0)
x2 <- replicate(samples, rbinom(n=size, size=20, prob = 0.3))


hist(x2)

means <- colMeans(x2)

plot(density(means))
```

The density plot of the means seems to follow the normal ditribution. According to the central limit theorem the sampling distribution of the mean of any independent, random variable will be normal or nearly normal, if the sample size is large enough.

```{r}
mean(means)
```

The mean of my sample means is 6 which equals to lamda.This is expected, since in the Binomial distribution, the mean E[X]=np.

H0: sample means are normally distributed
H1: sample means are not normally distributed

suppose H0

```{r}
ks.test(means,"pnorm",mean(means),sd(means))
qqnorm(means)
qqline(means)
```

P-value=0.5>a and D = 0.0081568 and therefore we fail to reject the null hypothesis that the sample means are normally distributed.
If n is large enough and p not close to 0 or 1, then the distribution
appears close to symmetric. 

##Question 3

- 3.1 

```{r}
anova_data <- read.csv("./anova_data.csv")

anova <- anova_data
str(anova)
names(anova)
anova <- as.data.frame(anova)
```

- 3.2 

```{r}

colSums(sapply(anova, is.na))
anovac <- anova[complete.cases(anova["employrate"]),]
colSums(sapply(anovac, is.na))
cemp <- aggregate(anovac$employrate, by=list(Category=anovac$continent), FUN=mean)
names(cemp)
colnames(cemp) <- c("continent", "employrate")
cemp[, 2] <- sapply(cemp[, 2], as.numeric)


library(ggplot2) 
ggplot(cemp, aes(x=factor(continent), y=employrate)) + stat_summary(fun.y="mean", geom="bar")


```

- 3.3

```{r}
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
```

F value is 3.938, and p-value is very low too. This means tha the variation of employrate means among different continents is much larger than the variation of employrate within each continents, and our p-value is less than 0.05 (as suggested by normal scientific standard). Hence we can conclude that for our confidence interval we accept the alternative hypothesis H1 that there is a significant relationship between continents and employrate.


- 3.4 

Not all  means are equal. Tukey test is designed to evaluate pair means.

```{r}
tuk<- TukeyHSD(anovamod)
tuk
```

From the table above (looking at "diff" and "p adj" columns) I can see which continents have significant differences in eployrate from others. For example I can conclude that:

There is no significant difference in employrate between Asia and Africa etc where p adj =0.99 > 0.05.

There is significant difference in employrate between East Europe and Africa etc where p<<0.05.

Next step is visualizing the differences between means in each continent. Significant differences are the ones that do not cross the zero value.

```{r}
plot (tuk)

```

- 3.5 

Bartlett's test will be used to determine whether the variance in employrate is the same for all continents. A significance level of 0.05 will be used.
```{r}
bartlett.test(employrate ~ continent, data=anovac)

```

I will also use fligner test to determine whether the the variance in employrate is the same for all continents. The null hypothesis is once more that all of the variances are equal.

```{r}
fligner.test(employrate ~ continent, data=anovac)

```
From the output of both 2 tests we can see that the p-value<< 0.05. This means we can assume the alternative H1 hypothesis that the variance is not the same for all continents. This means that there is evidence to suggest that the variance in employrate is different for each continent.

- 3.6 
 
```{r}
ggplot(anovafact, aes(x = continent, y = incomeperperson)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Continent") +
  ylab("Incomeperperson")
```

Although EE has low average income, the dispersion and therefore the wealth gap is not wide. In contrast NORAM and WE, continents with much higher average incomes have dramatically higher economic inequality.

```{r}
  ggplot(anovafact, aes(x = continent, y = femaleemployrate)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Continent") +
  ylab("femaleemployrate")
```

Female employment rate is low in EE.


```{r}  
   ggplot(anovafact, aes(x = continent, y = suicideper100th)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Continent") +
  ylab("suicideper100th") 
```

EE has the highest number of suicides comparing to the other continents.

```{r}    
     ggplot(anovafact, aes(x = continent, y = urbanrate)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Continent") +
  ylab("urbanrate") 
```

Urbanization is intense in EE. Negative effects of urbanization is unemployment, shortage of housing and therefore higher rents, lack of decent transportation- commuting. Moreover, urbanization has social effects such as poverty, lack of opportunities due to high competition, psychological problems, drugs, crime, violence etc. Life in the city is more stressful, overcrowiding results to loss of privacy,  the absence of green areas and nature result health problems. Concluding, urbanization in EE possibly explains the high suicide and unemployment rate and may have a negative effect on people's chance of finding a job when they graduate.   

Although we have gained some interesting insights, this model misses important variables and therefore we cannot draw concrete conclusions. 

For example employment rate depends on other factors as well such as education, economic and political stability, family wealth etc.

```{r} 

plot(anovafact$urbanrate~anovafact$employrate)
abline(test <- lm(anovafact$urbanrate~anovafact$employrate, data = anovafact))

```

This graph makes it clear that there is a negative correlation between urbanrization and employment rate. 

##Question 4

- 4.1

```{r}
housing <- read.table("./housing.data", quote="\"", comment.char="")

```

- 4.2

```{r}
colnames(housing) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS",  "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")
```

- 4.3

```{r}

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
```

```{r}
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
```


```{r}
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


```

- 4.4

This model does not fit our data well and we can definitely improve it.

The residuals are the difference between the actual values of the variable MEDV and predicted values from our model. In this case we have high values of residuals.

NOX and cHAS have high standard error of coefficients and we should examine this further. Note: the number error is relevant to the value of the coefficient. HIgher values will have higher errors.

t-values: shows whether whether the coeeficient is meaningful for the model and it is used to calculate the p-value.

P-value is the probability that the variable is not relevant. The lower the better. 
The stars on the right indicate that it is unlikely that no relationship exists. More stars are better.

Residual standard error: 4.833: This number is the standard deviation of the residuals and it should be proportional to the quantiles of the residuals.  

For a normal distribution, the 1st and 3rd quantiles should be 1.5 +/- the std error. In this case is should have been aprox 4.3 instead of 4.8.

R-squared = 72% evaluates the goodness of fit.R-squared should be improved.

Our model ONLY fits well houses with low median value MEDV. 

- 4.5 

F-statistic takes the parameters of our model and compares it to a model that has fewer parameters. In theory the model with more parameters should fit better. High P-value means that our model does not perform better than the model with fewer parameters. Our P-value is really small which means that our model is better than the model with fewer parameters.

- 4.6

The remaining variables are the following LSTAT, RM, PTRATIO, DIS, NOX, CHAS, B, ZN, CRIM


```{r}

mfull<-lm(MEDV ~. -RAD -INDUS -AGE -TAX, data = housing)
mnull<-lm(MEDV ~1, data = housing)
summary(step(mnull, scope=list(lower=mnull,upper=mfull), direction='both'))

summary(step(mfull, direction='both'))

model8 <- lm(MEDV ~ LSTAT + RM + PTRATIO + DIS + NOX + CHAS + 
    B + ZN + CRIM, data = housing)
summary(model8)
summary(mfull)

```


- 4.7 

Coefficient of CRIM= -0.061174

- 4.8

```{r}

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

```

The model's residuals must be normally distributed and equally spread. In our case the residuals are not normally distributed. There are many extreme positive and few negative residuals. The distribution is "heavy tailed." Positive values for the residual (on the y-axis) mean the prediction was too low, negative values mean the prediction was too high and 0 means the guess was exactly correct.The relationship between the sample percentiles and theoretical percentiles is not linear. 

Concluding, there might be a non-linear relationship between predictor variables and an outcome variable and we can problbly improve our model by adding logarithmic or exponential terms. Removing the outliers may also improve our model.


#check for outliers

```{r}
cooksd <- cooks.distance(mfull)
#we clearly have outliers
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(housing[influential, ])

```




