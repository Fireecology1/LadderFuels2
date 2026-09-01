#Process XV 'b' - testing different M values
#Original log(SFC) model formulation

# .libPaths(c(.libPaths(), "C:/Dan/RPackages"))
# require(tidyverse)

require(vtreat)
require(stringr)

#set up same as CFI work
fd4 <- read.csv('c:/Dan/_Remote_projects/LadderFuels2/files/fire_data_2026.csv') %>%  
  mutate(CFI2=factor(CFI1, labels=c('Surface', 'Crown'))) %>%
  filter(num <=113) #no Dewdrop or other right now

#Set up indexes
sharp <- filter(fd4, str_detect(ExpProject, "Sharpsand IM")) %>% 
  pull(num)  #original Sharpsand Immature stands only
sharp.sm <- filter(fd4, str_detect(ExpProject, "Sharpsand SM")) %>% 
  pull(num)  #Sharpsand - immature
icfme <- 77:87 #order is A, 1, 3, 4, 5, 6, 7, 8a, 8b, 9, 2; fixed in icfme process

#Cross-validation 
#
#kWayCrossValidation - let's see how this compares


#Number of loops for cross-validation (~20 for testing; 1000 for production)
Times=1000
nSplits=4

#chg names so models work correctly
fd4$FSG <- fd4$FSG1
fd4$CFI <- fd4$CFI1

#Variations on models 15, 16 from previous XV process:

M = 6 #or so? See below #originally 3

fd7 <- fd4 %>% 
  mutate(SFC3 = SFC + SFC.LF*M)

f15 <- formula(CFI ~ ws + I(FSG^1.5) + I(log(SFC3)) + ws:MC.FFMC)
f16 <- formula(CFI ~ ws + I(FSG^1.5) + I(log(SFC3)) + ws:MC.SA)

#new data model forms - fmult1, fmult2, fmult3, fmult4, fmult5, fmult6
M1=1
M2=2
M3=3 #use this
M4=4 #use
M5=5 #use
M6=6 #use this
M7=7 #use 
M8=8 #use
M9=9 #use
M10=10

fmult1<-fd4 %>%
  mutate(SFC3=SFC + SFC.LF*M1)

fmult2<-fd4 %>%
  mutate(SFC3=SFC + SFC.LF*M2)

fmult3<-fd4 %>% 
  mutate(SFC3=SFC + SFC.LF*M3)

fmult4<-fd4 %>%
  mutate(SFC3=SFC + SFC.LF*M4)

fmult5<-fd4 %>%
  mutate(SFC3=SFC + SFC.LF*M5)

fmult6<-fd4 %>%
  mutate(SFC3=SFC + SFC.LF*M6)

fmult7<-fd4 %>%
  mutate(SFC3=SFC + SFC.LF*M7)

fmult8<-fd4 %>%
  mutate(SFC3=SFC + SFC.LF*M8)

fmult9<-fd4 %>%
  mutate(SFC3=SFC + SFC.LF*M9)

fmult10<-fd4 %>%
  mutate(SFC3=SFC + SFC.LF*M10)


#Looping XV process

#1: fmult1
# form=f16
# fd.xv <- fmult1 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)
# 
# for(rep in 1:Times) {
#   splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)
#   
#   # initialize the prediction vector
#   xx <- .Random.seed
#   fd.xv$predxv <- 0  
#   
#   for(i in 1:4) {
#     split <- splitPlan[[i]]
#     model <- glm(formula=form, data = fd.xv[split$train, ], 
#                  family=binomial(link="logit"))
#     fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
#                                            type="response")
#   }
#   
#   fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
#     mutate(CorrXv=round(predxv, digits=0)==CFI)
#   
#   aic[rep]<-model$aic
#   correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)
#   
#   #Confusion matrix and Matthews correlation coefficient (mcc)
#   tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
#   tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
#   fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
#   fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
#   mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
#   
#   #rename for each
#   model.a <- model
#   correct.a <- mean(correct)
#   mcc.a <- mean(mcc)
#   aic.a <- mean(aic)
#   seed.a <- xx
# }


#2: fmult2
form=f16
fd.xv <- fmult2 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)

for(rep in 1:Times) {
  splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)

  # initialize the prediction vector
  xx <- .Random.seed
  fd.xv$predxv <- 0

  for(i in 1:4) {
    split <- splitPlan[[i]]
    model <- glm(formula=form, data = fd.xv[split$train, ],
                 family=binomial(link="logit"))
    fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
                                           type="response")
  }

  fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
    mutate(CorrXv=round(predxv, digits=0)==CFI)

  aic[rep]<-model$aic
  correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)

  #Confusion matrix and Matthews correlation coefficient (mcc)
  tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
  tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
  fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
  fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
  mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))

  #rename for each
  model.b <- model
  correct.b <- mean(correct)
  mcc.b <- mean(mcc)
  aic.b <- mean(aic)
  seed.b <- xx
}
#end big loop; 

#3: fmult3
form=f16
fd.xv <- fmult3 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)

for(rep in 1:Times) {
  splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)
  
  # initialize the prediction vector
  xx <- .Random.seed
  fd.xv$predxv <- 0  
  
  for(i in 1:4) {
    split <- splitPlan[[i]]
    model <- glm(formula=form, data = fd.xv[split$train, ], 
                 family=binomial(link="logit"))
    fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
                                           type="response")
  }
  
  fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
    mutate(CorrXv=round(predxv, digits=0)==CFI)
  
  aic[rep]<-model$aic
  correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)
  
  #Confusion matrix and Matthews correlation coefficient (mcc)
  tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
  tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
  fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
  fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
  mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
  
  #rename for each
  model.c <- model
  correct.c <- mean(correct)
  mcc.c <- mean(mcc)
  aic.c <- mean(aic)
  seed.c <- xx
}
#end big loop; 


#4: fmult4
form=f16
fd.xv <- fmult4 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)

for(rep in 1:Times) {
  splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)

  # initialize the prediction vector
  xx <- .Random.seed
  fd.xv$predxv <- 0

  for(i in 1:4) {
    split <- splitPlan[[i]]
    model <- glm(formula=form, data = fd.xv[split$train, ],
                 family=binomial(link="logit"))
    fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
                                           type="response")
  }

  fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
    mutate(CorrXv=round(predxv, digits=0)==CFI)

  aic[rep]<-model$aic
  correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)

  #Confusion matrix and Matthews correlation coefficient (mcc)
  tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
  tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
  fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
  fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
  mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))

  #rename for each
  model.d <- model
  correct.d <- mean(correct)
  mcc.d <- mean(mcc)
  aic.d <- mean(aic)
  seed.d <- xx
}
#end big loop; 


#5: fmult5
form=f16
fd.xv <- fmult5 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)

for(rep in 1:Times) {
  splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)

  # initialize the prediction vector
  xx <- .Random.seed
  fd.xv$predxv <- 0

  for(i in 1:4) {
    split <- splitPlan[[i]]
    model <- glm(formula=form, data = fd.xv[split$train, ],
                 family=binomial(link="logit"))
    fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
                                           type="response")
  }

  fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
    mutate(CorrXv=round(predxv, digits=0)==CFI)

  aic[rep]<-model$aic
  correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)
  ##
  #Confusion matrix and Matthews correlation coefficient (mcc)
  tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
  tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
  fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
  fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
  mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))

  #rename for each
  model.e <- model
  correct.e <- mean(correct)
  mcc.e <- mean(mcc)
  aic.e <- mean(aic)
  seed.e <- xx
}
#end big loop; 

#6: fmult6
form=f16
fd.xv <- fmult6 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)

for(rep in 1:Times) {
  splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)
  
  # initialize the prediction vector
  xx <- .Random.seed
  fd.xv$predxv <- 0  
  
  for(i in 1:4) {
    split <- splitPlan[[i]]
    model <- glm(formula=form, data = fd.xv[split$train, ], 
                 family=binomial(link="logit"))
    fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
                                           type="response")
  }
  
  fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
    mutate(CorrXv=round(predxv, digits=0)==CFI)
  
  aic[rep]<-model$aic
  correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)
  ##
  #Confusion matrix and Matthews correlation coefficient (mcc)
  tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
  tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
  fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
  fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
  mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
  
  #rename for each
  model.f <- model
  correct.f <- mean(correct)
  mcc.f <- mean(mcc)
  aic.f <- mean(aic)
  seed.f <- xx
}
#end big loop; 


#7: fmult7
form=f16
fd.xv <- fmult7 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)

for(rep in 1:Times) {
  splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)

  # initialize the prediction vector
  xx <- .Random.seed
  fd.xv$predxv <- 0

  for(i in 1:4) {
    split <- splitPlan[[i]]
    model <- glm(formula=form, data = fd.xv[split$train, ],
                 family=binomial(link="logit"))
    fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
                                           type="response")
  }

  fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
    mutate(CorrXv=round(predxv, digits=0)==CFI)

  aic[rep]<-model$aic
  correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)
  ##
  #Confusion matrix and Matthews correlation coefficient (mcc)
  tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
  tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
  fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
  fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
  mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))

  #rename for each
  model.g <- model
  correct.g <- mean(correct)
  mcc.g <- mean(mcc)
  aic.g <- mean(aic)
  seed.g <- xx
}
#end big loop; 

#8: fmult8
form=f16
fd.xv <- fmult8 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)

for(rep in 1:Times) {
  splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)
  
  # initialize the prediction vector
  xx <- .Random.seed
  fd.xv$predxv <- 0
  
  for(i in 1:4) {
    split <- splitPlan[[i]]
    model <- glm(formula=form, data = fd.xv[split$train, ],
                 family=binomial(link="logit"))
    fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
                                           type="response")
  }
  
  fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
    mutate(CorrXv=round(predxv, digits=0)==CFI)
  
  aic[rep]<-model$aic
  correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)
  ##
  #Confusion matrix and Matthews correlation coefficient (mcc)
  tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
  tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
  fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
  fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
  mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
  
  #rename for each
  model.h <- model
  correct.h <- mean(correct)
  mcc.h <- mean(mcc)
  aic.h <- mean(aic)
  seed.h <- xx
}
#end big loop; 

#9: fmult9
form=f16
fd.xv <- fmult9 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)

for(rep in 1:Times) {
  splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)
  
  # initialize the prediction vector
  xx <- .Random.seed
  fd.xv$predxv <- 0
  
  for(i in 1:4) {
    split <- splitPlan[[i]]
    model <- glm(formula=form, data = fd.xv[split$train, ],
                 family=binomial(link="logit"))
    fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
                                           type="response")
  }
  
  fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
    mutate(CorrXv=round(predxv, digits=0)==CFI)
  
  aic[rep]<-model$aic
  correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)
  ##
  #Confusion matrix and Matthews correlation coefficient (mcc)
  tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
  tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
  fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
  fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
  mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
  
  #rename for each
  model.i <- model
  correct.i <- mean(correct)
  mcc.i <- mean(mcc)
  aic.i <- mean(aic)
  seed.i <- xx
}
#end big loop; 

#10: fmult10
form=f16
fd.xv <- fmult10 %>% select(num, fire, ws, MC.SA, MC.FFMC, FSG, SFC3, SFC.LF, CFI)

for(rep in 1:Times) {
  splitPlan <- kWayCrossValidation(nrow(fd.xv), nSplits, NULL, NULL)
  
  # initialize the prediction vector
  xx <- .Random.seed
  fd.xv$predxv <- 0
  
  for(i in 1:4) {
    split <- splitPlan[[i]]
    model <- glm(formula=form, data = fd.xv[split$train, ],
                 family=binomial(link="logit"))
    fd.xv$predxv[split$app] <- predict.glm(model, newdata = fd.xv[split$app, ],
                                           type="response")
  }
  
  fdxv<-mutate(fd.xv, predxv=round(predxv, digits=4)) %>%
    mutate(CorrXv=round(predxv, digits=0)==CFI)
  
  aic[rep]<-model$aic
  correct[rep]<-nrow(filter(fdxv, CorrXv==TRUE))/nrow(fdxv)
  ##
  #Confusion matrix and Matthews correlation coefficient (mcc)
  tp<-nrow(filter(fdxv, CorrXv==TRUE & CFI==1))  #TRUE crown fires
  tn<-nrow(filter(fdxv, CorrXv==TRUE & CFI==0))  #TRUE surface fires
  fp<-nrow(filter(fdxv, CorrXv==FALSE & CFI==1))  #FALSE missed crown fires
  fn<-nrow(filter(fdxv, CorrXv==FALSE & CFI==0))  #FALSE missed surface fires
  mcc[rep]<-(tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
  
  #rename for each
  model.j <- model
  correct.j <- mean(correct)
  mcc.j <- mean(mcc)
  aic.j <- mean(aic)
  seed.j <- xx
}
#end big loop; 

#M=2:10 #9 values
#letters are through b,c,d,e,f,g,h,i,j

#Summary table

#
accuracy.vals <- c(correct.b, correct.c, correct.d, correct.e, correct.f, correct.g, correct.h, correct.i, correct.j) 
mcc.vals <- c(mcc.b, mcc.c, mcc.d, mcc.e, mcc.f, mcc.g, mcc.h, mcc.i, mcc.j) #previously mcc.a through mcc.h
aic.vals <- c(aic.b, aic.c, aic.d, aic.e, aic.f, aic.g, aic.h, aic.i, aic.j)
modelform <- f16 
multval <- c(2,3,4,5,6,7,8,9,10) #M value for run
Dataset <- rep('LF x M to SFC', times=9)  

#Save?
#XV.archive <- data.frame(as.character(modelform), accuracy.vals, mcc.vals, aic.vals, Dataset)
#XV.seed <- data.frame(seed.a, seed.b, seed.c, seed.d, seed.e, seed.f, seed.g, seed.h, seed.i, seed.j, 
#                      seed.k, seed.l, seed.r)

# write.csv(XV.archive, './XVarchive.csv' )
# write.csv(XV.seed, './XVseed.csv' )

XVM.summary <- data.frame(mod=as.character(modelform)[3], 
                         multval, 
                         Accuracy=round(accuracy.vals, 4), MCC=round(mcc.vals, 4), AIC=round(aic.vals, 2), 
                         Dataset) %>%
  mutate(MCC.rank=rank(length(MCC) + 1 - rank(MCC)),  #isn't there a descending rank function?
         AIC.rank=rank(AIC)) 

#M=6 is right in the middle, but probably best overall compromise

final.mod.ffmc <- glm(f15, data=fd7, family=binomial)
final.mod.sa <- glm(f16, data=fd7, family=binomial)

#Use these Aug 19, 2026
# save(final.mod.ffmc, file='./files/finmod_ffmc.rda')
# save(final.mod.sa, file='./files/finmod_sa.rda')

#write.csv(fd7, './files/fd7.csv')