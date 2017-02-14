z=read.table("yogurt.dat",header=TRUE) 

units=levels(factor(z[,2]))
nunits=length(units)
Z=matrix(1,nrow=nunits,ncol=1)   # regressors for mean of distribution of heterogeneity

varietydata=NULL

for (i in 1:nunits) {
	x=as.matrix(z[z[,2]==units[i],4:8])
	p=as.matrix(z[z[,2]==units[i],9])
	iota=matrix(1,ncol=1,nrow=ncol(x))
	p=p%*%t(iota)
	varietydata[[i]]=list(q=x,price=p)
		}


set.seed(66)
ngoods=5
beta=c(0,.5,.5,.5,.5)          # initial values
delta=c(-.5,-.5,-.5,-.5,-.5)
lambda=1                       # scale of random error == here set to N(0,1)

nz=ncol(Z)

#
# create varaibles and run MCMC
#

source("rhierVarietyRw.R")
nz=1
Data=list(varietydata=varietydata,Z=Z)
nvar=ngoods-1
nu=nvar+3
V=nu*diag(nvar)
Deltabar=matrix(0,nrow=nz,ncol=nvar)
A=matrix(.01,ncol=1,nrow=1)
Prior=list(nu=nu,V=V,Deltabar=Deltabar,A=A)
R=10000	                       # 10,000 iterations
r=50                           # r is passed to the GHK routine in the likelihood
s=.75                          # random-walk step size for beta
sdelta=.01                     # random-walk step size for delta
inc.root=diag(nvar)            # Identity matrix (I) used for random-walk
delta0=delta
lambda0=lambda
Betas0=matrix(0,ncol=nvar,nrow=nunits)
keep=10
Mcmc=list(R=R,r=r,keep=keep,inc.root=inc.root,s=s,sdelta=sdelta,delta0=delta0,lambda0=lambda0,Betas0=Betas0)

out=rhierVarietyRw(Data=Data,Prior=Prior,Mcmc=Mcmc)

#
# plot output
#
matplot(out$Deltadraw,type="l")
readline()
matplot(out$Vbetadraw,type="l")
readline()
matplot(t(out$betadraw[1,,]),type="l")
readline()
matplot(out$deltadraw,type="l")
readline()
matplot(out$loglike,type="l")
readline()
matplot(out$reject,type="l")




