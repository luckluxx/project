library(caret)
library(tidyverse)
library(tidymodels)
library(pROC)

DF <- read_csv("C:/Users/morni/Downloads/TITANICIMP.csv")
# 전처리
DF <-
  DF %>% 
  mutate(gender=factor(gender), 
         class=factor(class), 
         embarked=factor(embarked),
         y = factor(survived, labels=c('N','Y'))) %>% 
  dplyr::select(-survived)
str(DF)

# 분할
set.seed(1655)
IS <- initial_split(DF, prop=3/4, strata=y)
TR <- training(IS)
TS <- testing(IS)

TROUT <- TR %>% dplyr::select(y)
TSOUT <- TS %>% dplyr::select(y)

# 전처리
RC <- 
  recipe(y~., data=DF) %>%
  step_zv(all_numeric_predictors()) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors()) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal_predictors())
RC

# 튜닝계획
trCtrl <- trainControl(method='cv', number=5,
                       summaryFunction=twoClassSummary,
                       classProbs=TRUE)

metbin <- function(y, ph, yh, event_level='first'){
  cnf <- table(yh, y)
  if(event_level =='second'){
    tn  <-  cnf[1,1];  fn <- cnf[1,2]; fp <- cnf[2,1]; tp <- cnf[2,2];
  }else {
    tn  <-  cnf[2,2];  fn <- cnf[2,1]; fp <- cnf[1,2]; tp <- cnf[1,1];
  }
  c(acc   = accuracy_vec(y, yh),
    rocauc= roc_auc_vec(y, ph, event_level=event_level),
    prauc = pr_auc_vec(y, ph, event_level=event_level),
    f1    = f_meas_vec(y, yh, event_level=event_level),
    kap   = kap_vec(y, yh),
    sens  = sens_vec(y, yh, event_level=event_level),
    spec  = spec_vec(y, yh, event_level=event_level),
    prec  = precision_vec(y, yh, event_level=event_level),
    tn    = tn,
    fn    = fn,
    fp    = fp,
    tp    = tp
  )
}

# 로지스틱
modelLookup('glm')
set.seed(1655)
Mglm <- train(RC, data=TR, method='glm',
              family='binomial', metric='ROC',trControl=trCtrl)
TROUT <-
  TROUT %>%
  mutate(
    phglm = predict(Mglm, TR, type='prob')[,'Y'],
    yhglm = predict(Mglm, TR))

TSOUT <-
  TSOUT %>%
  mutate(
    phglm = predict(Mglm, TS, type='prob')[,'Y'],
    yhglm = predict(Mglm, TS))

confusionMatrix(TROUT$yhglm, TROUT$y, positive='Y')
confusionMatrix(TSOUT$yhglm, TSOUT$y, positive='Y')

gl <- autoplot(roc_curve(TSOUT, 'y', 'phglm', event_level='second'))
g2 <- autoplot(pr_curve(TSOUT, 'y', 'phglm', event_level='second'))
ggpubr::ggarrange(gl,g2,nrow=1,ncol=2)

METglm <- 
  bind_rows(
    metbin(TROUT$y, TROUT$phglm, TROUT$yhglm, event_level='second'),
    metbin(TSOUT$y, TSOUT$phglm, TSOUT$yhglm, event_level='second')) %>%
  
  mutate(id='glm', TRTS=c('TR','TS'))
METglm

# glmStepAIC
modelLookup('glmStepAIC')
set.seed(1655)
Mstep <-
  train(RC, data=TR, method='glmStepAIC', direction='backward',
        metric='ROC', # 후진선택법
        trControl=trCtrl)
TROUT <-
  TROUT %>%
  mutate(
    phstep = predict(Mstep, TR, type='prob')[,'Y'],
    yhstep = predict(Mstep, TR))

TSOUT <-
  TSOUT %>%
  mutate(
    phstep = predict(Mstep, TS, type='prob')[,'Y'],
    yhstep = predict(Mstep, TS))
confusionMatrix(TROUT$yhstep, TROUT$y, positive='Y')
confusionMatrix(TSOUT$yhstep, TSOUT$y, positive='Y')

METstep <- 
  bind_rows(
    metbin(TROUT$y, TROUT$phstep, TROUT$yhstep,
           event_level='second'),
    metbin(TSOUT$y, TSOUT$phstep, TSOUT$yhstep,
           event_level='second')) %>%
  
  mutate(id='stepAIC', TRTS=c('TR','TS'))
METstep

# glmnet
modelLookup('glmnet')
set.seed(1655)
Mgnet <- train(RC, data=TR, method='glmnet', tuneLength=5, trControl=trCtrl)
Mgnet$bestTune

TROUT <-
  TROUT %>%
  mutate(
    phgnet = predict(Mgnet, TR, type='prob')[,'Y'],
    yhgnet = predict(Mgnet, TR))

TSOUT <-
  TSOUT %>%
  mutate(
    phgnet = predict(Mgnet, TS, type='prob')[,'Y'],
    yhgnet = predict(Mgnet, TS))
confusionMatrix(TROUT$yhgnet, TROUT$y, positive='Y')
confusionMatrix(TSOUT$yhgnet, TSOUT$y, positive='Y')

gl <- autoplot(roc_curve(TSOUT, 'y', 'phgnet', event_level='second'))
g2 <- autoplot(pr_curve(TSOUT, 'y', 'phgnet', event_level='second'))
ggpubr::ggarrange(gl,g2,nrow=1,ncol=2)

METgnet <- 
  bind_rows(
    metbin(TROUT$y, TROUT$phgnet, TROUT$yhgnet,
           event_level='second'),
    metbin(TSOUT$y, TSOUT$phgnet, TSOUT$yhgnet,
           event_level='second')) %>%
  
  mutate(id='glmnet', TRTS=c('TR','TS'))
METgnet

# 결정 트리
library(rpart)
modelLookup('rpart')
set.seed(1655)
Mrp <- train(RC, data=TR, method='rpart', metric='ROC', tuneLength=20,
             trControl=trCtrl, control=rpart.control(minsplit=10, minbucket=5))
Mrp$bestTune

TROUT <-
  TROUT %>%
  mutate(
    phrp = predict(Mrp, TR, type='prob')[,'Y'],
    yhrp = predict(Mrp, TR))

TSOUT <-
  TSOUT %>%
  mutate(
    phrp = predict(Mrp, TS, type='prob')[,'Y'],
    yhrp = predict(Mrp, TS))
confusionMatrix(TROUT$yhrp, TROUT$y, positive='Y')
confusionMatrix(TSOUT$yhrp, TSOUT$y, positive='Y')
gl <- autoplot(roc_curve(TSOUT, 'y', 'phrp', event_level='second'))
g2 <- autoplot(pr_curve(TSOUT, 'y', 'phrp', event_level='second'))
ggpubr::ggarrange(gl,g2,nrow=1,ncol=2)

METrp <- 
  bind_rows(
    metbin(TROUT$y, TROUT$phrp, TROUT$yhrp,
           event_level='second'),
    metbin(TSOUT$y, TSOUT$phrp, TSOUT$yhrp,
           event_level='second')) %>%
  
  mutate(id='rp', TRTS=c('TR','TS'))
METrp

# 랜덤 포레스트
modelLookup('rf')
rfGrid <- data.frame(mtry = 2:(ncol(TR)-1)) 
set.seed(1655)
Mrf <- train(RC, data=TR,  
             method='rf', importance=TRUE, ntree=500, 
             tuneGrid = rfGrid, 
             metric='ROC',
             trControl=trCtrl)
plot(Mrf)
Mrf$bestTune

finCtrl <- trainControl(method = 'none', classProbs = TRUE)
Frf <- train(RC, data=DF,  
             method='rf', importance=TRUE, ntree=500, mtrty=7,
             metric='ROC',
             trControl=finCtrl)

TROUT <-
  TROUT %>%
  mutate(
    phrf = predict(Frf, TR, type='prob')[,'Y'],
    yhrf = predict(Frf, TR))

TSOUT <-
  TSOUT %>%
  mutate(
    phrf = predict(Frf, TS, type='prob')[,'Y'],
    yhrf = predict(Frf, TS))

confusionMatrix(TROUT$yhrf, TROUT$y, positive='Y')
confusionMatrix(TSOUT$yhrf, TSOUT$y, positive='Y')

gl <- autoplot(roc_curve(TSOUT, 'y', 'phrf', event_level='second'))
g2 <- autoplot(pr_curve(TSOUT, 'y', 'phrf', event_level='second'))
ggpubr::ggarrange(gl,g2,nrow=1,ncol=2)

METrf <- 
  bind_rows(
    metbin(TROUT$y, TROUT$phrf, TROUT$yhrf,
           event_level='second'),
    metbin(TSOUT$y, TSOUT$phrf, TSOUT$yhrf,
           event_level='second')) %>%
  
  mutate(id='rf', TRTS=c('TR','TS'))
METrf

# 랜덤포레스트(ranger)
modelLookup('ranger')
rangerGrid <- expand.grid(mtry=seq(2, ncol(TR)-1, by=1),
                          splitrule=c('gini', 'extratrees'),
                          min.node.size=5)
set.seed(1655)
Mranger <- train(RC, data=TR, method='ranger', importance='impurity',
                 tuneGrid=rangerGrid, metric='ROC', trControl=trCtrl)
TROUT <-
  TROUT %>%
  mutate(
    phranger = predict(Mranger, TR, type='prob')[,'Y'],
    yhranger = predict(Mranger, TR))

TSOUT <-
  TSOUT %>%
  mutate(
    phranger = predict(Mranger, TS, type='prob')[,'Y'],
    yhranger = predict(Mranger, TS))

gl <- autoplot(roc_curve(TSOUT, 'y', 'phranger', event_level='second'))
g2 <- autoplot(pr_curve(TSOUT, 'y', 'phranger', event_level='second'))
ggpubr::ggarrange(gl,g2,nrow=1,ncol=2)

confusionMatrix(TROUT$yhranger, TROUT$y, positive='Y')
confusionMatrix(TSOUT$yhranger, TSOUT$y, positive='Y')

METranger <- 
  bind_rows(
    metbin(TROUT$y, TROUT$phranger, TROUT$yhranger,
           event_level='second'),
    metbin(TSOUT$y, TSOUT$phranger, TSOUT$yhranger,
           event_level='second')) %>%
  
  mutate(id='ranger', TRTS=c('TR','TS'))
METranger


# 부스팅
modelLookup('gbm') # 모수 mtry
set.seed(1655)
gbmGrid <- expand.grid(n.trees=seq(100, 200, by=100),
                       interaction.depth=1:3,
                       shrinkage=seq(0.0, 1.0, by=0.5),
                       n.minobsinnode=10)

Mgbm <- train(RC, data=TR, method='gbm', distribution='bernoulli',
              metric='ROC',
              tuneLength=3, # tuneGrid=rpartGrid,
              trControl=trCtrl)
plot(Mgbm)
Mgbm$bestTune

TROUT <-
  TROUT %>%
  mutate(
    phgbm = predict(Mgbm, TR, type='prob')[,'Y'],
    yhgbm = predict(Mgbm, TR))

TSOUT <-
  TSOUT %>%
  mutate(
    phgbm = predict(Mgbm, TS, type='prob')[,'Y'],
    yhgbm = predict(Mgbm, TS))

confusionMatrix(TROUT$yhgbm, TROUT$y, positive='Y')
confusionMatrix(TSOUT$yhgbm, TSOUT$y, positive='Y')

gl <- autoplot(roc_curve(TSOUT, 'y', 'phgbm', event_level='second'))
g2 <- autoplot(pr_curve(TSOUT, 'y', 'phgbm', event_level='second'))
ggpubr::ggarrange(gl,g2,nrow=1,ncol=2)

TRrocgbm <- roc(TROUT$y, TROUT$phgbm)
auc(TRrocgbm)
coords(TRrocgbm, x='best', best.method='youden')

METgbm <- 
  bind_rows(
    metbin(TROUT$y, TROUT$phgbm, TROUT$yhgbm,
           event_level='second'),
    metbin(TSOUT$y, TSOUT$phgbm, TSOUT$yhgbm,
           event_level='second')) %>%
  
  mutate(id='gbm', TRTS=c('TR','TS'))
METgbm

# 모델 평가
resamp <- resamples(list(LM=Mglm,
                         STEP=Mstep,
                         GLMNET=Mgnet,
                         RPRT=Mrp,
                         RANGER=Mranger,
                         RF=Mrf,
                         GBM=Mgbm))
summary(resamp)
bwplot(resamp)

# TR/TS 평가
MET <- bind_rows(METglm, METstep, METgnet, METrp, METranger, METrf, METgbm) %>%
  arrange(TRTS, acc)
MET
TRrocranger <- roc(TROUT$y, TROUT$phranger)
TRrocrf <- roc(TROUT$y, TROUT$phrf)

plot(TRrocranger, col='red')
plot(TRrocrf, col='green')
plot(TRrocgbm, col='blue')

# 최종모형 적합
finCntl <- trainControl(method='none', classProbs=TRUE)
Mfin <- train(RC, data=TR, method='ranger',
              importance='impurity', tuneGrid=Mranger$bestTune,
              trControl = finCntl)
head(predict(Mfin, newdata=TS))
head(predict(Mfin, newdata=TS, type='prob'))
# 예측값 탐색
tmp <- cbind(TR, ph=predict(Mfin, newdata=TR, type='prob')[,'Y'])
tmp %>% arrange(desc(ph)) %>% head(n=15)
