# =======================================
# =====  LAD-SIS
# =======================================
library(quantreg)

LAD.SIS<-function(X=X,Y=Y)
{
  p=dim(X)[2]
  n=length(Y)
  s<-matrix(0,nrow=p,ncol=1) 
  
  Y<- Y-mean(Y) #centered y
  X<- apply(X,2,function(x) (x-mean(x))/(sd(x))) # standardize X
  
  for(j in 1:p){
    fit1<- rq(Y~X[,j], tau = 0.5) # quantreg package: tau=0.5 is equevalent to LAD
    s[j]<- mean(abs(fit1$residuals))
  }
  Rank<- rank(s); id<- (1:p)[Rank<=floor(n/log(n))]
  list(rank.LAD.SIS=Rank, selected.features=id) 
}
