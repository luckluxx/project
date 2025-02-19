data(swiss)
fit <- lm(Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality, data=swiss)
summary(fit)

# 잔차의 등분산성 확인: 무작위로 퍼짐
plot(fit$fitted.values, fit$residuals,
     main = "Residuals vs Fitted Plot",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 20, col = "blue")
abline(h = 0, col = "red")

# 잔차의 정규성 확인: 정규분포를 따름
qqnorm(fit$residuals, main = "QQ Plot of Residuals")
qqline(fit$residuals, col = "red")


library(car)
ncvTest(fit) # p값을 통해 등분산성을 만족함

h=hat(model.matrix(fit)) ; h
plot(h, type = "h", xlab= "case index", main= "leverage plot")
# 19번째, 45번째 관측치 영향력이 가장 큼

outlierTest(fit) 
# 유의수준 0.05에서 Sierre만이 유의한 이상점으로 판단

cutoff = 4/(nrow(swiss)-length(fit$coefficients)-2)
plot(fit, which=4, col=2, lwd=2, cook.levels=cutoff) 
# 영향점 Porrentruy, Sierre, Neuchatel(Cook's distance 기준)
influencePlot(fit, main= "Influence Plot", sub= "Circle size is proportial to Cook's Distance")

# 다중공선성 확인
vif(fit) # -> 5를 넘는 값이 없음. 우려X



data2 <- data.frame(
  Y = c(354, 190, 405, 263, 451, 302, 288, 385, 402, 365, 209, 290, 346,
        254, 395, 434, 220, 374, 308, 220, 311, 181, 274, 303, 244),
  X1 = c(84, 73, 65, 70, 76, 69, 63, 72, 79, 75, 27, 89, 65,
         57, 59, 69, 60, 79, 75, 82, 59, 67, 85, 55, 63),
  X2 = c(46, 20, 52, 30, 57, 25, 28, 36, 57, 44, 24, 31, 52,
         23, 60, 48, 34, 51, 50, 34, 46, 23, 37, 40, 30))
fit2 <- lm(Y~X1+X2, data=data2)
summary(fit2)

# 잔차의 등분산성 확인: 무작위로 퍼짐
plot(fit2$fitted.values, fit2$residuals,
     main = "Residuals vs Fitted Plot",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 20, col = "blue")
abline(h = 0, col = "red")

# 잔차의 정규성 확인: 정규분포를 따름
qqnorm(fit2$residuals, main = "QQ Plot of Residuals")
qqline(fit2$residuals, col = "red")

ncvTest(fit2) # p값을 통해 등분산성을 만족함

h2=hat(model.matrix(fit2)) ; h2
plot(h, type = "h2", xlab= "case index", main= "leverage plot")
# 11번째 관측치 영향력이 가장 큼

outlierTest(fit2) 
# 유의수준 0.05에서 8번 환자만이 유의한 이상점으로 판단

cutoff = 4/(nrow(data2)-length(fit2$coefficients)-2)
plot(fit2, which=4, col=2, lwd=2, cook.levels=cutoff)
# 영향점은 6, 16, 20번 환자(Cook's distance 기준)
influencePlot(fit2, main= "Influence Plot", sub= "Circle size is proportial to Cook's Distance")

# 다중공선성 확인
vif(fit2) # -> 5를 넘는 값이 없음. 우려X