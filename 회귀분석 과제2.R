fish <- 1:18
fish_data <- data.frame(
  Fish = fish,
  Y = c(2.51, 2.57, 2.43, 2.62, 2.74, 2.68, 2.83, 2.91, 2.98, 3.17,
        3.05, 3.09, 3.32, 3.22, 3.29, 3.44, 3.52, 3.55),
  X = c(5.0, 5.0, 5.0, 4.8, 4.8, 4.8, 4.6, 4.6, 4.6, 4.4, 4.4,
        4.4, 4.2, 4.2, 4.2, 4.0, 4.0, 4.0)
)
fish_data

plot(fish_data$X, fish_data$Y, xlab = "X", ylab = "Y")

cor(fish_data$X, fish_data$Y)

fit <- lm(Y ~ X, data = fish_data)
summary(fit)

abline(fit)

tb1 = abs((-0.99810)/0.03867)
ptb1 = 2*pt(tb1, 16, lower.tail=FALSE); ptb1

t_value = qt(0.975, 16)
lower_bound = -0.99810 - (t_value * 0.03867)
upper_bound = -0.99810 + (t_value * 0.03867)
cat("95% 신뢰구간: [", lower_bound, ", ", upper_bound, "]")

Y_hat <- predict(fit)
TSS <- sum((fish_data$Y - mean(fish_data$Y))^2)
RSS <- sum((fish_data$Y - Y_hat)^2)
1 - (RSS / TSS) #x가 y에 미치는 영향을 잘 설명함

fish_res = fit$residuals; fish_res
sum(fish_res^2)

plot(Y_hat, fish_res, xlab = "예측값 (Y_hat)", ylab = "잔차 (e)")
#서로 독립적이다

qqnorm(fish_res)
#잔차가 정규분포를 따르지 않는다

new_data <- data.frame(X = 5.5)
predict(fit, new_data)





binu_data <- data.frame(
  X = c(3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0),
  Y = c(24.4, 32.1, 37.1, 40.4, 43.3, 51.4, 61.9, 66.1, 77.2, 79.2)
)

plot(binu_data$X, binu_data$Y, xlab = "X", ylab = "Y")

model2 <- lm(Y ~ X, data = binu_data)
summary(model2)

tb2 = abs((12.4424)/0.6242)
ptb2 = 2*pt(tb2, 8, lower.tail=FALSE); ptb2

Y_hat2 <- predict(model2)
TSS2 <- sum((binu_data$Y - mean(binu_data$Y))^2)
RSS2 <- sum((binu_data$Y - Y_hat2)^2)
1 - (RSS2 / TSS2) #x가 y에 미치는 영향을 잘 설명함

predict(model2)

binu_res = model2$residuals; binu_res

plot(Y_hat2, binu_res, xlab = "예측값 (Y_hat)", ylab = "잔차 (e)")
#서로 독립하지 않고, 이분산성을 나타냄

qqnorm(binu_res)
#잔차가 정규분포를 따름(45도선에 가까움)

new_data2 <- data.frame(X = 5.3)
predict(model2, new_data2)
