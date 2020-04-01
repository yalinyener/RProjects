mtcars
summary(mtcars)
count(mtcars)
names(mtcars)

mtcars

any(is.na(mtcars))

str(mtcars)
summary(mtcars$mpg)
hist(mtcars$mpg)
glimpse(mtcars)
profiling_num(mtcars)

chart.Correlation(mtcars)

names(mtcars)
featurePlot(x=mtcars[,c("cyl","disp","wt","hp","drat")],y=mtcars$mpg )

model <- lm(mtcars$mpg~
              mtcars$cyl+
              mtcars$disp+
              mtcars$wt+
              mtcars$hp+
              mtcars$drat)
summary(model)
plot(model)

confint(model)

ols_plot_response(model)
ols_test_normality(model)

model2 <- lm (mpg~.,data=mtcars)
summary(model2)

b <- ols_step_all_possible(model2)
b
names(b)

data.frame(b$predictors, b$adjr) 


forward <- ols_step_forward_aic(model2)
plot(forward)


back <- ols_step_backward_aic(model2)
plot(back)


two <- ols_step_both_aic(model2)
plot(two)

new.model <- lm (mpg~
                  wt+cyl+hp, mtcars)

summary(new.model)

predict(new.model)
head(predict(new.model))

anova(new.model)


kar <- data.frame(
  y <- head(mtcars$mpg),
  ysapka <- head(predict(new.model))
)

kar

fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) # çoklu doğrusal regresyon modeli
fit
library(car)

outlierTest(fit)
qqPlot(fit, main="QQ Plot") 

dev.new()
leveragePlots(fit)


# Etkili gözlemlerin belirlenmesi
avPlots(fit)
dev.new()
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
dev.new()
# Influence Plot 
influencePlot(fit,id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


qqPlot(fit, main="QQ Plot")

library(MASS)
dev.new()

sresid <- studres(fit) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

step <- stepAIC(fit, direction="both")
step$anova 

crPlots(fit)
ceresPlots(fit)
