## recent version of code following methodology as in Nagarajan et al. 2014

library(bnlearn)
library(catnet)
library(erer)
library(pcalg)
library(e1071)
library(randomForest)
library(rpart)
library(Rgraphviz)
##### Difference when entire dataset is taken ###

setwd("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\wits\\VUHDDS\\VUHDDS")

# data_bn_a <- read.csv('11var_2009.csv')
data_bn_a <- read.csv('inpatient_11var_2009.csv')

data_bn <- na.omit(data_bn_a)

colnames(data_bn)

data_bn <- data_bn[,!colnames(data_bn)%in%c('Year')]

## Discretize ToS ##

cutoff<-quantile(data_bn$PDAYS,(0:4)/4)
# hist(data_bn$CHRGS,xlim=c(1,1000),breaks=100000)
#cutoff<-quantile(data_bn$CHRGS,(0:4)/4,na.rm=TRUE)

bin_var <- function(var)
{
  cutoff<-quantile(var,(0:4)/4)
  var_cat <- vector()
  for(i in 1:length(var))
  {
    if (var[i] < cutoff[2]) {var_cat[i] ="Below 2 days"} else 
      if (var[i] < cutoff[3] & var[i] >= cutoff[2]) {var_cat[i] ="Below 3 days"} else 
        if (var[i] < cutoff[4] & var[i] >= cutoff[3]) {var_cat[i] ="Below 5 days"} else
        {var_cat[i] ="Above 5 days"}     
  }
  return(var_cat)
}

## Outpatient does not have good TimeofStaydata.
# Hence
#save.image("inpatient_outpatient2009")

bin_charge <- function(var)
{
  cutoff<-quantile(var,(0:4)/4)
  var_cat <- vector()
  for(i in 1:length(var))
  {
    if (var[i] < cutoff[2]) {var_cat[i] ="Below 6200$"} else 
      if (var[i] < cutoff[3] & var[i] >= cutoff[2]) {var_cat[i] ="Below 11600$"} else 
        if (var[i] < cutoff[4] & var[i] >= cutoff[3]) {var_cat[i] ="Below 23300$"} else
        {var_cat[i] ="Above 23300$"}     
  }
  return(var_cat)
}

system.time(data_bn$ToS <- bin_var(data_bn$PDAYS))
system.time(data_bn$Bill <- bin_charge(data_bn$CHRGS))

data_n <- as.data.frame(data_bn[,!colnames(data_bn)%in% c('PDAYS','CHRGS')])
data_n <- apply(data_n,2,function(x) as.factor(x) )

levels_b <- sapply(data_n,function(x) levels(x) )

### Start with straight-forward full learning (hc and gs):

# Removing Procedure group as it has null values:
#data_np <- data_n[,!colnames(data_n)%in% c('CCSPXGRP')] 
#data_np <- na.omit(data_np)

## CAH = Critical access hospital!
## Too many levels for some varaibles 

set.seed(123)

smpsize <- floor(0.8 * nrow(data_n))
bntr  <- sample(seq_len(nrow(data_n)),size = smpsize,replace=FALSE)

bntrain <- as.data.frame(data_n[bntr,],row.names=F)
bntest <- as.data.frame(data_n[-bntr,],row.names=F)

write.csv(bntrain,"inpatient_training.csv")
write.csv(bntest,"inpatient_test.csv")

### Set the basic network and then start working ###
## Only bnlearn and catnet for now: Create initial network for both separately

# hc, gs, catnet, bootstrapp contrast results; predict with SVM & random forest; domain interpretations

[1] "HNUM2"     "ATYPE"     "AGEGRP"    "SEX"       "DSTAT"     "CCSDXGRP"  "CAH"       "STRES"    
[9] "ADMID_QTR" "ToS"       "Bill"     

bl = read.csv("blacklist.csv")

parPool <- list(
  c(1),
  c(2,1,3,4,7,9),
  c(3),
  c(4),
  c(5,1,2,9,10),
  c(6,2,3,4,8,9),
  c(7,1),
  c(8),
  c(9),
  c(10,1,2,3,4,6,8,9),
  c(11,10,6)
)

bn.gs <- gs(bntrain,blacklist=bl)
bn.hc <- hc(bntrain,blacklist=bl)
bn.mmhc <- mmhc(bntrain,blacklist=bl)

gs.fitted <- bn.fit(bn.gs,data=bntrain)
hc.fitted <- bn.fit(bn.hc,data=bntrain)
mmhc.fitted <- bn.fit(bn.mmhc,data=bntrain)

score(bn.gs,data=bntrain,type="aic")
score(bn.hc,data=bntrain,type="aic")
score(bn.mmhc,data=bntrain,type="aic")

plot(bn.gs)
plot(bn.hc)
plot(bn.mmhc)
# Minimum AIC is best, so gs is the best

system.time(
  cat.sa <- cnSearchSA(data= bntrain,maxParentSet=4,parentsPool=parPool,maxIter=100,
                          stopDiff=0.0001,numThreads=4)
)

find_AIC.sa <- cnFindAIC(object= cat.sa)
find_BIC.sa <- cnFindBIC(object= cat.sa)

cnMatEdges(find_AIC.sa)
cnMatEdges(find_BIC.sa)

cnDot(find_AIC.sa,"catnet_AIC")
cnDot(find_BIC.sa,"catnet_BIC")


## Averaged network:

nodes = names(bntrain)
start = random.graph(nodes=nodes,method="ic-dag",num=100)
netlist=lapply(start,function(net)
{
  hc(bntrain,blacklist=bl,score="aic",start=net)
}
)
rnd = custom.strength(netlist,nodes=nodes)
rnd[(rnd$strength > 0.85) & (rnd$direction >= 0.5),]

avg.start = averaged.network(rnd,threshold=0.85)
#plot(avg.start)

## add direction hospital to CAH with domain knowledge
avg.hc <- set.arc(avg.start,"HNUM2","CAH")
plot(avg.hc)
graphviz.plot(avg.hc,layout="fdp")

avg.hc.fitted <- bn.fit(avg.hc,bntrain)


## Plot the graphs, parameter learning for bnlearn 

## Compare performance with randomforest, decision tree, naive bayes, SVM for diff variables

## conclude... and paper

## ML models: randomforest, decision tree, naive bayes, SVM

nb <- naiveBayes(Bill~.,data=bntrain)
nb.predict.Bill <- predict(nb,bntest)

dt <- rpart(Bill~.,data=bntrain)
dt.predict.Bill <- predict(dt,bntest)

#rf <- randomForest(Bill~ .,data=bntrain)
#rf.predict.Bill <- predict(rf,bntest)

#svm <- svm(Bill~ .,data=bntrain)
#svm.predict.Bill <- predict(svm,bntest)

## ToS

nb <- naiveBayes(ToS~.,data=bntrain)
nb.predict.ToS <- predict(nb,bntest)

dt <- rpart(ToS~.,data=bntrain)
dt.predict.ToS <- predict(dt,bntest)

#rf <- randomForest(ToS~ .,data=bntrain)
#rf.predict.ToS <- predict(rf,bntest)

#svm <- svm(ToS~ .,data=bntrain)
#svm.predict.ToS <- predict(svm,bntest)

## CCSDXGRP

nb <- naiveBayes(CCSDXGRP~.,data=bntrain)
nb.predict.CCSDXGRP <- predict(nb,bntest)

dt <- rpart(CCSDXGRP~.,data=bntrain)
dt.predict.CCSDXGRP <- predict(dt,bntest)

#rf <- randomForest(CCSDXGRP~ .,data=bntrain)
#rf.predict.CCSDXGRP <- predict(rf,bntest)

#svm <- svm(CCSDXGRP~ .,data=bntrain)
#svm.predict.CCSDXGRP <- predict(svm,bntest)

## Predict for Bill, ToS and CCSDXGRP

test1 <- bntest
test1[,c("Bill")] <- NA
sa.predict.Bill <- cnPredict(find_AIC.sa,test1)

bn.gs.predict.Bill <- predict(gs.fitted,data=test1[,1:(ncol(test1)-1)],node="Bill")
bn.hc.predict.Bill <- predict(hc.fitted,data=test1[,1:(ncol(test1)-1)],node="Bill")
bn.mmhc.predict.Bill <- predict(mmhc.fitted,data=test1[,1:(ncol(test1)-1)],node="Bill")
avg.hc.predict.Bill <- predict(avg.hc.fitted,data=test1[,1:(ncol(test1)-1)],node="Bill")


test2 <- bntest
test2[,c("ToS")] <- NA
sa.predict.ToS <- cnPredict(find_AIC.sa,test2)

bn.gs.predict.ToS <- predict(gs.fitted,data=test2[,!colnames(test2) %in% c("ToS")],node="ToS")
bn.hc.predict.ToS <- predict(hc.fitted,data=test2[,!colnames(test2) %in% c("ToS")],node="ToS")
bn.mmhc.predict.ToS <- predict(mmhc.fitted,data=test2[,!colnames(test2) %in% c("ToS")],node="ToS")
avg.hc.predict.ToS <- predict(avg.hc.fitted,data=test2[,!colnames(test2) %in% c("ToS")],node="ToS")


test3 <- bntest
test3[,c("CCSDXGRP")] <- NA
sa.predict.CCSDXGRP <- cnPredict(find_AIC.sa,test3)

bn.gs.predict.CCSDXGRP <- predict(gs.fitted,data=test3[,!colnames(test3) %in% c("CCSDXGRP")],node="CCSDXGRP")
bn.hc.predict.CCSDXGRP <- predict(hc.fitted,data=test3[,!colnames(test3) %in% c("CCSDXGRP")],node="CCSDXGRP")
bn.mmhc.predict.CCSDXGRP <- predict(mmhc.fitted,data=test3[,!colnames(test3) %in% c("CCSDXGRP")],node="CCSDXGRP")
avg.hc.predict.CCSDXGRP <- predict(avg.hc.fitted,data=test3[,!colnames(test3) %in% c("CCSDXGRP")],node="CCSDXGRP")

pBill <- data.frame(actual = bntest[,c("Bill")],nb=nb.predict.Bill,dt=dt.predict.Bill,
                    gs=bn.gs.predict.Bill,hc=bn.hc.predict.Bill,mmhc=bn.mmhc.predict.Bill,sa=sa.predict.Bill,
                    avg.hc = avg.hc.predict.Bill)

write.csv(pBill,"Bill_predictions.csv")

pCCSDXGRP <- data.frame(actual = bntest[,c("CCSDXGRP")],nb=nb.predict.CCSDXGRP,dt=dt.predict.CCSDXGRP,
                    gs=bn.gs.predict.CCSDXGRP,hc=bn.hc.predict.CCSDXGRP,mmhc=bn.mmhc.predict.CCSDXGRP,sa=sa.predict.CCSDXGRP,
                    avg.hc = avg.hc.predict.CCSDXGRP)

write.csv(pCCSDXGRP,"CCSDXGRP_predictions.csv")

pToS <- data.frame(actual = bntest[,c("ToS")],nb=nb.predict.ToS,dt=dt.predict.ToS,
                    gs=bn.gs.predict.ToS,hc=bn.hc.predict.ToS,mmhc=bn.mmhc.predict.ToS,sa=sa.predict.ToS,
                   avg.hc = avg.hc.predict.ToS)

write.csv(pToS,"ToS_predictions.csv")

### Metric logic for hc diagram:

hc.fitted


