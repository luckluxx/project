library(caret)
library(tidyverse)
library(tidymodels)
library(skimr)
library(naniar) # 기초통계량
library(ggpubr)
library(scales)
library(gridExtra)

## 1. 파일 읽기
DF <- read_csv("C:/Users/morni/OneDrive/바탕 화면/coding/df2015na.csv")
str(DF)
head(DF)

## 2. 변수 조정
# 이산형은 모두 factor로 바꿈
DF <-
  DF %>%
  mutate(gnd=factor(gnd), bld=factor(bld),
         lft=factor(lft, labels=c('N', 'Y')),
         smk=factor(smk, labels=c('N', 'Y')),
         alc=factor(alc, labels=c('N', 'Y')))
str(DF)

## 3. 결측
DF %>% skim() # 결측 현황
# 완전한 관측값 비율=완전한 관측값/n
sum(complete.cases(DF))/nrow(DF)*100
# 변수별 결측비율, Missing=결측셀비율, Present=비결측셀비율
naniar::vis_miss(DF)
naniar::miss_var_summary(DF)

## 4. 간단 탐색
# caret::featurePlot(x:연속형, y:요인, plot='strip.scatter.box...)
featurePlot(x=DF %>% select_if(is.numeric), y=DF$gnd,
            plot='box',
            scales=list(x=list(realtion='free'), y=list(relation='free')))
featurePlot(x=DF %>% select_if(is.numeric), y=DF$bld,
            plot='box',
            scales=list(x=list(realtion='free'), y=list(relation='free')))
featurePlot(x=DF %>% select_if(is.numeric), y=DF$lft,
            plot='box',
            scales=list(x=list(realtion='free'), y=list(relation='free')))
featurePlot(x=DF %>% select_if(is.numeric), y=DF$smk,
            plot='box',
            scales=list(x=list(realtion='free'), y=list(relation='free')))
featurePlot(x=DF %>% select_if(is.numeric), y=DF$alc,
            plot='box',
            scales=list(x=list(realtion='free'), y=list(relation='free')))
# 연속-연속(상관계수/상관행렬)
R <- cor(DF %>% select_if(is.numeric), use='pairwise.complete.obs')
round(R, 4)
sort(R['ht',], decreasing=TRUE) # 타겟:키, 상관계수가 높은 순
corrplot::corrplot.mixed(R, upper='ellipse', order='FPC')
# 상관행렬 그리기
library(GGally)
DF %>% select_if(is.numeric) %>%
  ggcorr(geom='tile', label=TRUE)
# 남녀 분포
DF %>%
  ggplot(aes(x=wt, y=ht))+
  geom_density2d()+
  geom_point(aes(col=gnd, shape=gnd))

## 5. 분할/예측값 저장소
# TR:TS를 0.75:0.25로 1회 분할
set.seed(1655)
IS <- initial_split(DF, prop=0.75) # prop: TR비율율
TR <- training(IS)
TS <- testing(IS)
TROUT <- TR %>% dplyr::select(ht) # 예측값 저장 장소
TSOUT <- TS %>% dplyr::select(ht)

## 6. 전처리
RC <-
  recipe(ht~., data=TR) %>% # ht를 제외한 TR의 모든 변수로 예측
  step_impute_median(all_numeric_predictors()) %>% # 모든 숫자형 x에 대해 결측을 중위수 대체
  step_impute_mode(all_nominal_predictors()) %>% # 모든 이산형 x에 대해 결측을 최빈값 대체
  step_dummy(all_nominal_predictors()) # 모든 이산형 변수 가변수화
RC  
  
## 7. 튜닝계획 지정
trCtrl <- trainControl(method='cv', number=5)

## 8. 선형회귀모형
modelLookup('lm')

set.seed(1655)
Mlm <-
  train(RC, data=TR,
        method='lm',
        trControl=trCtrl) # 성능평가: RMSE, Rsquared
Mlm
Mlm$results
summary(Mlm)

plot(varImp(Mlm)) # 변수중요도
Mlm$bestTune # 튜닝 최적모수
Mlm$finalModel # lm객체
Mlm$resample # 최적모수값에 대한 CV통계량
# 예측값 저장
TROUT <- TR%>% dplyr::select(ht)
TSOUT <- TS%>% dplyr::select(ht)
TROUT <- TROUT %>% bind_cols(yhlm=predict(Mlm, newdata=TR))
TSOUT <- TSOUT %>% bind_cols(yhlm=predict(Mlm, newdata=TS))
head(TSOUT)
# 성능 평가
metreg <- function(y, yh){
  c(rmse=rmse_vec(y, yh),
    mae=mae_vec(y, yh),
    rsq=rsq_vec(y, yh))
}
metreg(TSOUT$ht, TSOUT$yhlm)
METlm <-
  metreg(TROUT$ht, TROUT$yhlm) %>%
  bind_rows(metreg(TSOUT$ht, TSOUT$yhlm)) %>%
  bind_cols(data.frame(model=c('lm','lm'), TRTS=c('TR','TS')))
METlm
g1 <- TROUT %>% ggplot(aes(x=yhlm, y=ht)) + geom_point()
g2 <- TROUT %>% ggplot(aes(x=yhlm, y=ht-yhlm)) + geom_point()
g3 <- TSOUT %>% ggplot(aes(x=yhlm, y=ht)) + geom_point()
g4 <- TSOUT %>% ggplot(aes(x=yhlm, y=ht-yhlm)) + geom_point()
grid.arrange(g1,g2,g3,g4, ncol=2)

## 9. lmStepAIC: AIC 변수선택
modelLookup('lmStepAIC')
set.seed(1655)
Mstep <-
  train(RC, data=TR,
        method='lmStepAIC',
        direction='backward', # 후진선택법
        trControl=trCtrl)
Mstep
Mstep$results # CV결과
summary(Mstep)
ggplot(varImp(Mstep)) # 변수중요도
Mstep$bestTune # 튜닝모수 없음
Mstep$finalModel # lm객체
Mstep$resample
TROUT <- TROUT %>% mutate(yhstep=predict(Mstep, newdata=TR))
TSOUT <- TSOUT %>% mutate(yhstep=predict(Mstep, newdata=TS))
head(TSOUT)
g1 <- TROUT %>% ggplot(aes(x=yhstep, y=ht)) + geom_point()
g2 <- TROUT %>% ggplot(aes(x=yhstep, y=ht-yhstep)) + geom_point()
g3 <- TSOUT %>% ggplot(aes(x=yhstep, y=ht)) + geom_point()
g4 <- TSOUT %>% ggplot(aes(x=yhstep, y=ht-yhstep)) + geom_point()
grid.arrange(g1,g2,g3,g4, ncol=2)

METstep <-
  metreg(TROUT$ht, TROUT$yhstep) %>%
  bind_rows(metreg(TSOUT$ht, TSOUT$yhstep)) %>%
  bind_cols(data.frame(model=c('lmStepAIC', 'lmStepAIC'), TRTS=c('TR','TS')))
METstep

## 10. 튜닝
modelLookup('glmnet')
set.seed(1655)
# 최적 모수값
glmnetGrid <- expand.grid(alpha=seq(0,1,by=0.25), lambda=seq(0.0, 0.1, by=0.01))
Mglmnet <-
  train(RC, data=TR,
        method='glmnet',
        trControl=trCtrl,
        tuneGrid=glmnetGrid)
Mglmnet # alpha는 라쏘, lambda는 벌점
Mglmnet$results
ggplot(Mglmnet)
ggplot(varImp(Mglmnet))
Mglmnet$bestTune
Mglmnet$resample
plot(Mglmnet$finalModel)
plot(Mglmnet$finalModel, xvar='lambda', label=TRUE)
abline(v=log(Mglmnet$bestTune$lambda), lty=2)
coef(Mglmnet$final, s=Mglmnet$bestTune$lambda)
TROUT <- TROUT %>% mutate(yhglmnet=predict(Mglmnet, newdata=TR))
TSOUT <- TSOUT %>% mutate(yhglmnet=predict(Mglmnet, newdata=TS))
head(TSOUT)
g1 <- TROUT %>% ggplot(aes(x=yhglmnet, y=ht)) + geom_point()
g2 <- TROUT %>% ggplot(aes(x=yhglmnet, y=ht-yhglmnet)) + geom_point()
g3 <- TSOUT %>% ggplot(aes(x=yhglmnet, y=ht)) + geom_point()
g4 <- TSOUT %>% ggplot(aes(x=yhglmnet, y=ht-yhglmnet)) + geom_point()
grid.arrange(g1,g2,g3,g4, ncol=2)

METglmnet <-
  metreg(TROUT$ht, TROUT$yhglmnet) %>%
  bind_rows(metreg(TSOUT$ht, TSOUT$yhglmnet)) %>%
  bind_cols(data.frame(model=c('glmnet', 'glmnet'), TRTS=c('TR','TS')))
METglmnet
