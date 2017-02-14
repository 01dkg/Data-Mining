oj$store <- factor(oj$store)
oj[1:2,]
x <- model.matrix(logmove ~ log(price)*(feat + brand 
                                        + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM 
                                        + HVAL150 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5)^2, data=oj)
dim(x)

## First column of x consists of ones (the intercept)
## We strip the column of ones as intercept is included automatically
x=x[,-1]
## We normalize the covariates as they are of very different magnitudes
## Each normalized covariate has mean 0 and standard deviation 1
for (j in 1:209) {
  x[,j]=(x[,j]-mean(x[,j]))/sd(x[,j])
}

## One could consider the standard regression model
reg <- lm(oj$logmove~x)
summary(reg)
p0=predict(reg)

## Or, one could consider LASSO 
library(lars) 
lasso <- lars(x=x, y=oj$logmove, trace=TRUE)
coef(lasso, s=c(.25,.50,0.75,1.00), mode="fraction")
## creates LASSO estimates as function of lambda 
## gives you the estimates for four shrinkage coef 

## Check that predictions in regression and lars (s=1) are the same
p1=predict(lasso,x,s=1,mode="fraction")
p1$fit  	
pdiff=p1$fit-p0
pdiff  ## zero differences

## out of sample prediction; estimate model on 20,000 rows
MSElasso10=dim(10) 
MSElasso50=dim(10) 
MSElasso90=dim(10) 
MSElasso100=dim(10) 
set.seed(1)	## fixes seed to make random draws reproducible
for(i in 1:10){
  train <- sample(1:nrow(oj), 20000)
  lasso <- lars(x=x[train,], y=oj$logmove[train])
  MSElasso10[i]= 
    mean((predict(lasso,x[-train,], s=.10, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso50[i]=  
    mean((predict(lasso,x[-train,], s=.50, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso90[i]=
    mean((predict(lasso,x[-train,], s=.90, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso100[i]=
    mean((predict(lasso,x[-train,], s=1.0, mode="fraction")
          $fit - oj$logmove[-train])^2)
}
mean(MSElasso10)
mean(MSElasso50)
mean(MSElasso90)
mean(MSElasso100)
boxplot(MSElasso10,MSElasso50,MSElasso90,MSElasso100,ylab="MSE", sub="LASSO model",xlab="s=0.10            s=0.50             s=0.9          s=1.0(LS)")

## out of sample prediction; estimate model on 1,000 rows
set.seed(1)	## fixes seed to make random draws reproducible
for(i in 1:10){
  train <- sample(1:nrow(oj), 1000)
  lasso <- lars(x=x[train,], y=oj$logmove[train])  
  MSElasso10[i]= 
    mean((predict(lasso,x[-train,], s=.10, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso50[i]=  
    mean((predict(lasso,x[-train,], s=.50, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso90[i]=
    mean((predict(lasso,x[-train,], s=.90, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso100[i]=
    mean((predict(lasso,x[-train,], s=1.0, mode="fraction")
          $fit - oj$logmove[-train])^2)
}
mean(MSElasso10)
mean(MSElasso50)
mean(MSElasso90)
mean(MSElasso100)
boxplot(MSElasso10,MSElasso50,MSElasso90,MSElasso100,ylab="MSE", sub="LASSO model",xlab="s=0.10            s=0.50             s=0.9          s=1.0(LS)")



