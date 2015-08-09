## Strategy:
#### Revised objective of the paper: 
## (a) Introduce application of BN to EHR? data. Tell them of different methods
## (b) + (When to use what*) 
## (c) + Domain conclusion for existing dataset (Time of stay as output)

## Discretize ToS and describe the dataset in the paper
## Start with BN, catnet and bootstrapp like in Nagarajan's book. 
## WRITE THE PAPER
### complete stats HW?!


library(bnlearn)
library(catnet)
library(erer)

##### Difference when entire dataset is taken ###

setwd("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\wits\\VUHDDS\\VUHDDS")

data_bn <- read.csv('temp_data2.csv')
colnames(data_bn)



data_b <- apply(data_d,2,function(x) as.factor(x) )
data_n <- as.data.frame(data_b)

nrow(subset(data_n,is.na(data_n$CCSPXGRP)))  ## Number of null values in procedure
                                             
levels_b <- sapply(data_n,function(x) levels(x) )

#data_sm <- data_n[,c('ATYPE','AGEGRP','SEX','CCSDXGRP')]
#data_p <- as.data.frame(na.omit(data_sm))

data_p <- data_n[,!colnames(data_n)%in% c('ZIPGRP')]

parPool <- list(
  c(1),
  c(2),
  c(3),
  c(4,1,2,3,6),
  c(5,4,6),
  c(6,1,2,3),
  c(7,6)
)


system.time(
  netlist.3 <- cnSearchSA(data= data_p,maxParentSet=4,parentsPool=parPool,maxIter=10,
                          stopDiff=0.0001,numThreads=4)
)
#  seconds

find_AIC.sa3 <- cnFindAIC(object= netlist.3)
find_BIC.sa3 <- cnFindBIC(object= netlist.3)

cnMatEdges(find_AIC.sa3)
cnMatEdges(find_BIC.sa3)

cnDot(find_AIC.sa3,"VUHDDS_SA_0805.dot")

cnet1 <- cnRandomCatnet(numnodes=4, maxParents=2, numCategories=2)
cnDot(cnet1,"example1.dot")

aggregated_column <- list()

for(i in 1:length(data_cat_138))
{  
  prob_out[[i]] length(levels(data_cat_138[,i]))
  
  ### This reads the columns other than the last few for the output lables ###
  cols <- colnames(prob_out[[1]])[1:(ncol(prob_out[[1]]) - length(levels(data_cat_138[,1])))] 
  
  x <- prob_out[[1]]
  
  z <- as.data.frame.matrix(x)   ## This took me hours ##
  
  # 
  xdf <- data.frame(matrix(unlist(x), nrow=nrow(x), byrow=T))
  xdsf <- data.frame(matrix(x, nrow=nrow(x), byrow=T))
  
  z <- data.frame(matrix(unlist(x)))
  x$Support <- 0
  
  data_f_sub <- data_cat_138[,colnames(data_cat_138)%in% cols]
  
  data_f_sub$Support <- 0
  d <- aggregate(Support ~ . ,data = data_f_sub,length)
  
  dq <- aggregate(Support ~ dput(cols) ,data = data_f_sub,length)
  
  ## Finally, merge this column to the CPT:
  
  y <- merge(z,d,by=dput(cols),all.x=TRUE)   
  
  qy <- merge(z,d,by=c('ToD', 'Window_dist', 'Activity', 'CO2'),all.x=TRUE)  
  
  one = c(1:5); dput(as.character(one))
  
  # After all this monkey business (what is left to do, is to order by Support) 
  ##and screen out top CPT insights
  
  ### Repeat entire analysis with only two levels for each of the inputs and adding Age, BMI, etc
  #### Write everything.... Prune later 
  
  # Prediction/Exact inference in grain & catnet ###
  
  cnCompare(find_AIC.4p, find_AIC.sa4pb, extended = TRUE)
  
  # cnPearsonTest(find_AIC.4p,data_cat_138) - Doesnt work
  
  f <- cnPredict(find_AIC.4p,data_cat_138)
  

##### Archive #####

bn.gs <- gs(data_p)
bn.hc <- hc(data_p)

plot(bn.gs)
score(bn.gs,data = data_p, type = "bic")
score(cextend(cpdag(bn.gs)),data_p,type ="bde",iss=10)

relevant("CCSDXGRP",data = data_p)

res = set.arc(gs(data_bn2), "SDNN", "CO2")

res1 <- drop.arc(bn.,"BMI_gr","Gender")

bn.boot <- boot.strength(data = data_bn2, R=500,algorithm = "hc",algorithm.args = list(score = "bde",iss =10))

sig_boot <- bn.boot[(bn.boot$strength > 0.85) & (bn.boot$direction >= 0.5),]

avg.boot <- averaged.network(bn.boot,threshold = 0.85)



parPool <- list(
  c(2,3,4,5,7),
  c(2),
  c(3),
  c(4),
  c(5,2,3,4),
  c(6,1,5,7),
  c(1,2,3,4,7,5)
  )


norder <- c(2,3,4,1,5,7,6)

system.time(
  model.1 <- cnSearchOrder(data=data_n[,1:7],maxParentSet=4,parentsPool= parPool ,nodeOrder=norder))

## 


find_AIC.1 <- cnFindAIC(object=model.1)
find_BIC.1 <- cnFindBIC(object=model.1)

cnMatEdges(find_BIC.1)
cnMatEdges(find_AIC.1)

### Stochastic search/simulated annealing

system.time(
  netlist.3 <- cnSearchSA(data= data_cat138,maxParentSet=4,parentsPool=parPool,maxIter=10,
                          stopDiff=0.0001,numThreads=4)
)
#  seconds

find_AIC.sa3 <- cnFindAIC(object= netlist.3)
find_BIC.sa3 <- cnFindBIC(object= netlist.3)

cnMatEdges(find_AIC.sa3)
cnMatEdges(find_BIC.sa3)

## Nagarajan method (manual annealing):

nodes <- colnames(data_cat_138)

## From Nagarajan book:
## Kind of useless, as we only get good candidates for graph and not CPT/Parameter learning ##

netlist.n <- vector(10,mode="list")
system.time(
netlist.n <- lapply(netlist.n, function(net){
  boot = data_cat_138[sample(nrow(data_cat_138),replace=TRUE),]
  nets = cnSearchOrder(data=boot,maxParentSet=4,parentsPool= parPool_p ,nodeOrder=norder_p)
  best = cnFindBIC(nets,nrow(data_cat_138))
  cnMatEdges(best)
})
)
## 1675 seconds

sa = custom.strength(netlist.n,nodes=nodes)

bn_averaged_network <- sa[(sa$strength > 0.85) & (sa$direction >= 0.5),]

bn_list <- bn.fit(bn_averaged_network,data_cat_138)

as.graphNEL(sa)

cnPlot(cnet)
cnDot(cnet,"cnet.dot")

#cnPlot(find_AIC.sa4p)
cnDot(find_AIC.sa4p,"catnet_SA_0624.dot")
cnDot(find_AIC.sa3p,"catnet_DP_0624.dot")

library(graph)
a = bn.fit(hc(learning.test), learning.test)
b = as.graphNEL(a)
c = as.bn(b)

##### Chose find_AIC.4p finally #########

cnDot(find_AIC.4p,"catnet_DP_0624")

prob_single_out <- cnPlotProb(find_AIC.4p)
prob_out <- cnProb(find_AIC.4p)

### Calculate the support of each CPT entry ###

# DT[, countcat:=.N, by=list(country,category)] ##Data.table from 
##http://stackoverflow.com/questions/11081535/applying-calculation-per-groups-within-r-dataframe

for(var in 1:length(prob_out))
{
 
s <- data.table(prob_out[[1]][,1:4]) 
m<- as.data.frame(prob_out[[1]][,1:4],row.names=F)

colnames(prob_out[[1]])

d <- s[, countcat:=.N, by=as.list(colnames(s)[1:4])] 

m$N <- 0
  rep(0,nrow(m))
d <- aggregate(N ~ ToD + Window_dist + Activity + CO2, data = m, length)

dw <- aggregate(N ~ ToD + Window_dist + Activity, data = m, length)

write.list(prob_out,"probability_values_DP_24June.csv")


d <- aggregate(N ~ ToD + Window_dist + Activity + CO2, data = m, length)

### Create your own Support function for CPT table (as none others available)
## To find number of tuples for each combination in prob_out

aggregated_column <- list()

for(i in 1:length(data_cat_138))
{  
  prob_out[[i]] length(levels(data_cat_138[,i]))

  ### This reads the columns other than the last few for the output lables ###
  cols <- colnames(prob_out[[1]])[1:(ncol(prob_out[[1]]) - length(levels(data_cat_138[,1])))] 
  
  x <- prob_out[[1]]
  
  z <- as.data.frame.matrix(x)   ## This took me hours ##
  
  # 
  xdf <- data.frame(matrix(unlist(x), nrow=nrow(x), byrow=T))
  xdsf <- data.frame(matrix(x, nrow=nrow(x), byrow=T))
  
  z <- data.frame(matrix(unlist(x)))
  x$Support <- 0
  
  data_f_sub <- data_cat_138[,colnames(data_cat_138)%in% cols]
  
  data_f_sub$Support <- 0
  d <- aggregate(Support ~ . ,data = data_f_sub,length)
  
  dq <- aggregate(Support ~ dput(cols) ,data = data_f_sub,length)
  
  ## Finally, merge this column to the CPT:
  
  y <- merge(z,d,by=dput(cols),all.x=TRUE)   
  
  qy <- merge(z,d,by=c('ToD', 'Window_dist', 'Activity', 'CO2'),all.x=TRUE)  
  
  one = c(1:5); dput(as.character(one))
  
  # After all this monkey business (what is left to do, is to order by Support) 
  ##and screen out top CPT insights
  
  ### Repeat entire analysis with only two levels for each of the inputs and adding Age, BMI, etc
  #### Write everything.... Prune later 
  
  # Prediction/Exact inference in grain & catnet ###
  
  cnCompare(find_AIC.4p, find_AIC.sa4pb, extended = TRUE)

  # cnPearsonTest(find_AIC.4p,data_cat_138) - Doesnt work

  f <- cnPredict(find_AIC.4p,data_cat_138)
  
  ## Manual five-fold cross validation!!
  

  
