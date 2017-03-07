# <function>
# <name>
# skewness
# </name>
# <description>
# Sample skewness. The biased portion is the population skewness; correction is for finite sample.  
# D. N. Joanes and C. A. Gill. “Comparing Measures of Sample Skewness and Kurtosis”. The Statistician 47(1):183–189
# </description>
# <arguments>
#
# </arguments>
# <sample>
# 
# </sample>
# <source>
skewness=function(x)
{
 x=x[!is.na(x)]
 n=length(x)
 if(length(x)<=2) return(0)

 mn=mean(x)
 sumcube=sum((x-mn)^3)
 sumsq=sum((x-mn)^2)
 biased=sqrt(n)*sumcube/(sumsq^1.5)
 correction=sqrt(n)*sqrt((n-1)/(n-2))

 return(biased*correction)
}
# </source>
# </function>
# 
#
#
# <function>
# <name>
# skewness
# </name>
# <description>
# Standard error of skewness. Depends only on sample size. 
# </description>
# <arguments>
#
# </arguments>
# <sample>
# 
# </sample>
# <source>
se.skewness=function(x)
{
 x=x[!is.na(x)]
 n=length(x)
 if(length(x)<=2) return(0)

 part1=6/(n-2)
 part2=n/(n+1)
 part3=(n-1)/(n+3)
 return(sqrt(part1*part2*part3))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# skewness
# </name>
# <description>
# Sample kurtosis. The biased portion is the population kurtosis; corrected is for finite sample.  
# </description>
# <arguments>
#
# </arguments>
# <sample>
# 
# </sample>
# <source>
kurtosis=function(x)
{
 x=x[!is.na(x)]
 n=length(x)
 if(length(x)<=3) return(0)

 mn=mean(x)
 moment4=sum((x-mn)^4)/n
 moment2=sum((x-mn)^2)/n
 biased=moment4/(moment2^2)
 
 part1=(biased*(n+1)+6)/(n-2)
 part2=(n-1)/(n-3)

 return(part1*part2)
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# skewness
# </name>
# <description>
# Standard error of kurtosis. Depends only on sample size. 
# </description>
# <arguments>
#
# </arguments>
# <sample>
# 
# </sample>
# <source>
se.kurtosis=function(x)
{
 x=x[!is.na(x)]
 n=length(x)
 if(length(x)<=3) return(0)

 SES=se.skewness(x)
 part=n/(n-3)
 part1=n*part-1/(n-3)
 part2=n+5
 
 return(2*SES*sqrt(part1/part2))
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# regslope
# </name>
# <description>
# Returns slope of regression as single scalar (for use with apply).
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
regslope=function(y,x)
{
 if(length(y[is.infinite(y)])>0) return(NA)
 if(length(y[is.infinite(x)])>0) return(NA)

 fit=lm(y~x,na.action="na.exclude")
 return(summary(fit)$coef[2,1])
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# regslope.noint
# </name>
# <description>
#  Returns slope of regression with no intercept as single scalar (for use with apply).

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
regslope.noint=function(y,x)
{
 fit=lm(y~x+0)
 return(summary(fit)$coef[2,1])
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# regress.plot
# </name>
# <description>
#  Performs regression in convenient way and returns coefficients and
# probabilities in a single vector, and plots a graph.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
regress.plot=function(x,y,title="",graphit=T,add=F,pts=T,clr="blue",ptnames=NULL,xrange=NULL,yrange=NULL,xtitle=NULL,ytitle=NULL)
{
 fit=lm(y~x)
 regcoef=summary(fit)$coef[,1]
 prob=summary(fit)$coef[,4]
 rsq=cor(x,y)^2

 if(is.null(xrange)) xrange=range(x)
 if(is.null(yrange)) yrange=range(y)
 if(is.null(xtitle)) xtitle="x"
 if(is.null(ytitle)) ytitle="y"
 if(graphit)
  {
   if(add & pts) points(x,y,pch=16)
   if(!add & pts) plot(x,y,pch=16,main=title)
   abline(fit,col=clr)

   if(!is.null(ptnames)) identify(x,y,ptnames)
  }

 return(list(coef=c(regcoef,prob,rsq),full=summary(fit)))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# regress.loglog
# </name>
# <description>
#  Performs regression and graphs in a convenient way: with or without log-transforming x and y variables (the option addone
# can be included to handle zeros for log-transformation), with or
# without manual point labelling, without or without the best-fit line added, and with many options for colors and points. 
# add can be a vector of length 2, a constant to be added to every value
# of x, y to remove zeroes.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
regress.loglog=function(x,y,xlog=TRUE,ylog=TRUE,addone=NULL,graphit=TRUE,xrange=NULL,yrange=NULL,add=FALSE,pts=16,lwidth=1,
                        drawline="solid",title="",xtitle=NULL,ytitle=NULL,ptnames=NULL,ptsize=1,clr="blue",lineclr="red",includeaxis=TRUE)
{
 exist = !is.na(x) & !is.na(y)
 x=x[exist]
 y=y[exist]
 ptnames=ptnames[exist]

 if(!is.null(addone)) 
  {
   x[x==0]=x[x==0]+addone[1]
   y[y==0]=y[y==0]+addone[2]
  }

 pos = !is.na(x)
 if(xlog) pos = pos & x>0
 if(ylog) pos = pos & y>0
 x=x[pos]
 if(length(x)==0)  return(list(coef=NULL,prob=NULL,rsq=NULL,pred=NULL,full=NULL))  ## Added March 2010

 y=y[pos]
 ptnames=ptnames[pos]

 if(xlog) xreg=log(x)
 else xreg=x
 if(ylog) yreg=log(y)
 else yreg=y

 fit=lm(yreg~xreg)
 
 regcoef=summary(fit)$coef[,1]
 prob=summary(fit)$coef[,4]
 rsq=cor(xreg,yreg)^2

 if(is.null(xrange)) xrange=range(x)
 if(is.null(yrange)) yrange=range(y)
 if(is.null(xtitle)) xtitle="x"
 if(is.null(ytitle)) ytitle="y"

 if(ylog) predy=exp(fit$fitted)
 else predy=fit$fitted

 if(graphit)
  {
   logaxs=""
   if(xlog) logaxs=pst(logaxs,"x")
   if(ylog) logaxs=pst(logaxs,"y")

   if(add & !is.null(pts)) points(x,y,pch=pts,col=clr,cex=ptsize,cex.lab=ptsize,cex.axis=ptsize)

   if(!add & !is.null(pts))
      plot(x,y,pch=pts,main=title,log=logaxs,xlim=xrange,ylim=yrange,xlab=xtitle,ylab=ytitle,col=clr,
	       cex=ptsize,cex.lab=ptsize,cex.axis=ptsize,axes=includeaxis)
   if(!includeaxis) box()

   ord=order(x)
   if(!is.null(drawline)) lines(x[ord],predy[ord],col=lineclr,lty=drawline,lwd=lwidth)

   if(!is.null(ptnames)) identify(x,y,ptnames)
  }

 return(list(coef=regcoef,prob=prob,rsq=rsq,pred=predy,full=summary(fit)))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# majoraxisreg
# </name>
# <description>
#  A major axis regression with parameters fitted by optim. The regression
# is the line which minimizes perpendicular distance summed over all points
# (and squared).

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
majoraxisreg=function(x,y,title="",graphit=F,add=F,pts=T,clr="blue",xtitle="x",ytitle="y",ptsize=1,labsize=1)
{
 start.param=c(1,1)

 fit=optim(start.param,minum.perpdist,x=x,y=y)
 m=fit$par[2]
 b=fit$par[1]

 if(graphit)
  {
   if(add & pts) points(x,y,pch=16)
   if(!add & pts) plot(x,y,pch=16,main=title,xlab=xtitle,ylab=ytitle,cex=ptsize,cex.lab=labsize,cex.axis=labsize)
   abline(b,m,col=clr)
  }

 return(fit$par)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# minum.perpdist
# </name>
# <description>
#  The sum of squares used by majoraxisreg.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
minum.perpdist=function(param,x,y)
    return(sumsq(perpendicular.distance(param[1],param[2],x,y)))



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# majoraxisreg.no.int
# </name>
# <description>
#  Major axis regression with no intercept. Only a slope
# is returned. Below is the same for standard regression.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
majoraxisreg.no.int=function(x,y)
{
 inc=(!is.na(x)&!is.na(y))
 x=x[inc]
 y=y[inc]

 a=sum(x*y)
 b=sum(x^2)-sum(y^2)
 c=(-a)

 answer=numeric()
 answer[1]=(-b+sqrt(b^2-4*a*c))/(2*a)
 answer[2]=(-b-sqrt(b^2-4*a*c))/(2*a)
 return(max(answer,na.rm=T))
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# standardreg.no.int
# </name>
# <description>
#  Standard regression with no intercept.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
standardreg.no.int=function(x,y)
{
 inc=!is.na(x)&!is.na(y)
 x=x[inc]
 y=y[inc]

 return(sum(x*y)/sum(x^2))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# autoregression
# </name>
# <description>
# Autocorrelation with a given lag of a vector y.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
autoregression=function(y,lag,xlog=FALSE,ylog=FALSE,graphit=TRUE)
{
 N=length(y)

 lagindex=(1+lag):N
 nonlagindex=1:(N-lag)

 ylag=y[lagindex]
 ynon=y[nonlagindex]

 return(regress.loglog(ynon,ylag,xlog=xlog,ylog=ylog,graphit=graphit))
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# regression.Bayes
# </name>
# <description>
# Regression using the Gibbs sampler, with just one x variable. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
regression.Bayes=function(x,y,start.param=NULL,steps=1100,whichuse=101:1100,showstep=50)
{
 no.x=dim(x)[2]
 X=matrix(nrow=length(y),ncol=no.x+1)
 X[,1]=1
 X[,2:(no.x+1)]=as.matrix(x)
 
 if(is.null(start.param)) start.param=c(rep(0,no.x+1),1)
 
 beta=matrix(nrow=steps,ncol=no.x+1)
 beta[1,]=start.param[1:(no.x+1)]
 var=numeric()
 var[1]=start.param[no.x+2]
 
 for(i in 2:steps)
  {
   beta[i,]=Gibbs.regslope(X,y,var[i-1])
   var[i]=Gibbs.regsigma(X,y,beta[i,])
   
   if(i%%showstep==1) cat("step ", i, ": slope= ", beta[i,], "; sd= ", sqrt(var[i]), "\n")
  }
 
 keepbeta=beta[whichuse,]
 keepvar=var[whichuse]
 
 upperbeta=apply(keepbeta,2,quantile,prob=0.975)
 lowerbeta=apply(keepbeta,2,quantile,prob=0.025)
 meanbeta=colMeans(keepbeta)
 
 CIvar=quantile(keepvar,prob=c(0.025,.975))
 meanvar=mean(keepvar)
 
 return(list(means=c(meanbeta,lowerbeta,upperbeta,sqrt(meanvar),sqrt(CIvar)),fullbeta=keepbeta,fullvar=keepvar))
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# Gibbs.regslope
# </name>
# <description>
# Updates the regression slope (used in regression.Bayes).
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
Gibbs.regslope=function(X,y,var)
{
 betahat=solve(t(X)%*%X)%*%t(X)%*%y
 Vbeta=solve(t(X)%*%X)
# browser()
 
 return(mvrnorm(1,mu=betahat,Sigma=var*Vbeta))
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# Gibbs.regsigma
# </name>
# <description>
# Updates the regression standard deviation (used in regression.Bayes).
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
Gibbs.regsigma=function(X,y,beta)
{
 n=length(y)
 k=dim(X)[2]
 M=y-X%*%beta
 
 s=(n-k)/2
 c=0.5*t(M)%*%M
 
 return(rinvgamma(1,shape=s,scale=c))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# Gibbs.normalmean
# </name>
# <description>
# The standard Gibbs sampler for a normal distribution with unknown mean and variance. 
# </description>
# <arguments>
# y is the vector of observations<br>
# sigma is the latest draw of the SD, using sqrt(Gibbs.normalvar)
# </arguments>
# <sample>
# 
# </sample>
# <source>
Gibbs.normalmean=function(y,sigma)
{
 y=y[!is.na(y)]
 if(sd(y)==0 | length(y)==1) return(mean(y))
 
 samplemean=mean(y)
 n=length(y)
 
 return(rnorm(1,mean=samplemean,sd=sqrt(sigma^2/n)))
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# Gibbs.normalvar
# </name>
# <description>
# Gibbs draw for the variance of a normal distribution (http://www.biostat.jhsph.edu/~fdominic/teaching/BM/3-4.pdf). If all y are
# identical, it returns a small positive number. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
Gibbs.normalvar=function(y)
{
 y=y[!is.na(y)]
 if(sd(y)==0) return(0.01)

 n=length(y)
 vr=var(y)
 
 return(rinvgamma(1,shape=(n-1)/2,scale=(n-1)*vr/2))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# model.xy
# </name>
# <description>
# Generic Bayesian routine for fitting a model to y given 1 predictor variable x. The function
# of y~x must be supplied, as well as the function for the SD~y. Any functions with any numbers
# of parameters can be used: predfunc is the function of y~x, and sdfunc is the function sd~y.
# The function badpredpar is needed so the user can make up any definition
# for parameter values that are out of bounds. Without this, the model could not support any generic predfunc.
# Badpredpar must accept two vectors of parameters, one for main and one for sd.
# The ellipses allow additional parameters to be passed to the model function, but there is no such option for the
# sd function. The additional parameters mean that some of the variables defining the model do not have to be fitted.
# The sd function can be omitted if the likelihood does not require it. 
# This works only if one likelihood function defines the likelihood of the model, given data and parameters only.
# If the likelihood of some parameters is conditional on other parameters, as in hierarchical model, this can't be used.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
#  testx=1:10; testy=1+2*testx+rnorm(10,0,1)
#   model.xy(x=testx,y=testy,predfunc=linear.model,llikefunc=llike.GaussModel,badpredpar=BadParam,start.predpar=c(1,1),
#            sdfunc=constant,start.sdpar=1,llikefuncSD=llike.GaussModelSD)
# </sample>
# <source>
model.xy=function(x,y,predfunc,llikefunc,badpredpar,badsdpar=NULL,start.predpar,sdfunc=NULL,start.sdpar=NULL,llikefuncSD,
                  logx=FALSE,logy=FALSE,add=c(0,0),steps=100,showstep=20,burnin=50,...)
{
 x=x+add[1]
 y=y+add[2]

 runx=x
 runy=y

 if(logx) runx=log(x[x>0 & y>0])
 if(logy) runy=log(y[x>0 & y>0])

 nopredpar=length(start.predpar)
 predpar=matrix(nrow=steps,ncol=nopredpar)
 predparscale=0.5*start.predpar
 predparscale[predparscale<0]=(-predparscale[predparscale<0])
 predparscale[predparscale<.1]=.1
 predpar[1,]=start.predpar

 if(!is.null(start.sdpar))
  {
   nosdpar=length(start.sdpar)
   sdpar=matrix(nrow=steps,ncol=nosdpar)
   sdparscale=0.5*start.sdpar
   sdparscale[sdparscale<=0]=.1

   sdpar[1,]=start.sdpar
  }

 extra=list(...)
 if(is.null(extra$MINIMUM_SD)) MINIMUM_SD=0
 else MINIMUM_SD=extra$MINIMUM_SD  

 llike=numeric()
 i=1
 if(!is.null(sdfunc)) SD=sdfunc(x=runx,param=sdpar[i,])
 llike[i]=llikefunc(testparam=predpar[i,1],allparam=predpar[i,],whichtest=1,x=runx,obs=runy,model=predfunc,
                    badpred=badpredpar,SD=SD,...)
 cat(i, ": ", round(predpar[i,],4), round(sdpar[i,],4), round(llike[i],4), "\n")

 for(i in 2:steps)
  {
   if(!is.null(sdfunc)) SD=sdfunc(x=runx,param=sdpar[i-1,])
   if(length(which(SD<=MINIMUM_SD))>0) browser()

   for(j in 1:nopredpar)
    {
     param=arrangeParam.Gibbs(i,j,predpar)
     # browser()     
     metropresult=metrop1step(func=llikefunc,start.param=param[j],scale.param=predparscale[j],adjust=1.02,target=0.25,
                              allparam=param,whichtest=j,x=runx,obs=runy,model=predfunc,badpred=badpredpar,SD=SD,...)
     predpar[i,j]=metropresult[1]
     predparscale[j]=metropresult[2]
    }

   pred=predfunc(x=x,param=predpar[i,])
  
   if(!is.null(sdfunc)) for(j in 1:nosdpar)
    {
     param=arrangeParam.Gibbs(i,j,sdpar)

     metropresult=metrop1step(func=llikefuncSD,start.param=param[j],scale.param=sdparscale[j],adjust=1.02,target=0.25,
                              allparam=param,whichtest=j,x=runx,pred=pred,obs=runy,model=sdfunc,badsd=badsdpar,...)
     sdpar[i,j]=metropresult[1]
     sdparscale[j]=metropresult[2]
    # browser()
    }
    
   if(!is.null(sdfunc)) SD=sdfunc(x=runx,param=sdpar[i,])
 
   if(length(which(SD<=MINIMUM_SD))>0) browser()
   llike[i]=llikefunc(testparam=predpar[i,1],allparam=predpar[i,],whichtest=1,x=runx,obs=runy,model=predfunc,badpred=badpredpar,SD=SD,...)
                    
   if(i%%showstep==0 | i==2) cat(i, ": ", round(predpar[i,],3), round(sdpar[i,],4), round(llike[i],2), "\n")
  }

 keep=(-(1:burnin))
 best=colMedians(predpar[keep,])
 bestmean=colMeans(predpar[keep,])
 conf=apply(predpar[keep,],2,CI)
 
 if(!is.null(sdfunc))
  {
   bestSD=IfElse(nosdpar==1,median(sdpar[keep]),colMedians(sdpar[keep,]))
   bestSDmean=IfElse(nosdpar==1,mean(sdpar[keep]),colMeans(sdpar[keep,]))
   confSD=IfElse(nosdpar==1,CI(sdpar[keep]),apply(sdpar[keep,],2,CI))
  }
 else bestSD=confSD=sdpar=NULL
 
 pred=predfunc(x=x,param=best,...)
 predSD=sdfunc(x=x,param=bestSD)
 modelresult=data.frame(x=runx,y=runy,pred,predSD)
 modelresult=modelresult[order(modelresult$x),]
 
 return(list(best=best,bestmean=bestmean,CI=conf,bestSD=bestSD,bestSDmean=bestSDmean,confSD=confSD,
             model=modelresult,fullparam=predpar,sdpar=sdpar,llike=llike,burn=burnin,keep=keep))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# arrangeParam.llike
# </name>
# <description>
# Used in likelihood function of a Gibbs sampler. Allows any of a set of parameters to be submitted to metrop1step; 
# whichtest is the index of the parameter to test. If NULL, zero, or NA, it simply returns allparam.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
arrangeParam.llike=function(testparam,allparam,whichtest)
{
 param=allparam
 if(whichtest>0 & !is.na(whichtest) & !is.null(whichtest)) param[whichtest]=testparam

 return(param)
}
 

# <function>
# <name>
# arrangeParam.Gibbs
# </name>
# <description>
# Used in the loop of a Gibbs sampler, setting parameters not yet tested (j and above) to previous value (i-1), 
# and other parameters (<j) to new value (i). 
# This has unfortunate need for the entire matrix allparam, when only row i-1 and row i are needed. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
arrangeParam.Gibbs=function(i,j,allparam)
{
 if(is.null(dim(allparam))) return(allparam[i-1])
 
 param=allparam[i-1,]
 if(j>1) param[1:(j-1)]=allparam[i,1:(j-1)]
 
 return(param)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# llike.GaussModel
# </name>
# <description>
# This is for model.xy. It takes the model function, its parameters, x values, observed values of the dependent variable obs, and sd values,
# to generate a likelihood. One of the parameters is passed as testparam, for use with metrop1step.
# This requires a badparam function for testing parameters. The standdard deviation is passed as an argument,
# not calculated from sdmodel.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
llike.GaussModel=function(testparam,allparam,whichtest,x,obs,model,badpred,SD,...)
{
 param=arrangeParam.llike(testparam,allparam,whichtest)

 pred=model(x=x,param=param,...)
 # Passing ... to badpred. I added that Sept 2013 so that growthfit.bin could pass MINBINSAMPLE from command line. This now forces all 
 if(badpred(x,param,pred,...)) return(-Inf)

 llike=dnorm(obs,mean=pred,sd=SD,log=TRUE)

 total=sum(llike)
 if(is.na(total) | is.infinite(total) | is.null(total)) 
  {
   cat('Something wrong with llike calculation from observed and predicted; check right now SD, obs, x, and pred\n')
   browser()
  }

 return(total)
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# llike.GaussModelSD
# </name>
# <description>
# This is for model.xy. Take the function for the SD, its parameters, and both predicted and observed values of the 
# dependent variable (pred,obs)
# to generate a likelihood. One of the parameters is passed as testparam, for use with metrop1step. The predicted value
# for each observation is included, and not calculated from the predicting function. 
# MINIMUM_SD=.0001
# MINIMUM_SD should be set in the program calling model.xy, to adjust it appropriately. If MINSD==0, then the sd can
# collapse to a miniscule number and drive the likelihood very high, preventing parameter searches from ever escaping the sd.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
llike.GaussModelSD=function(testparam,allparam,whichtest,x,pred,obs,model,badsd,...)
{
 extra=list(...)
 if(is.null(extra$MINIMUM_SD)) MINIMUM_SD=0
 else MINIMUM_SD=extra$MINIMUM_SD

 param=arrangeParam.llike(testparam,allparam,whichtest)

 sd=model(x=x,param=param)
 if(length(which(sd<=MINIMUM_SD))>0) return(-Inf)
 if(!is.null(badsd)) if(badsd(x,param)) return(-Inf)
 
 llike=dnorm(obs,mean=pred,sd=sd,log=TRUE)
 total=sum(llike)
 if(is.na(total) | is.infinite(total) | is.null(total)) browser()
 
 return(total)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# BadParam
# </name>
# <description>
# This is a default for model.xy, never returning TRUE. To use model.xy, another function must be created to
# return TRUE for any illegal parameter combination (any that would, for instance, produce a predicted y out-of-range, or a would
# create an error in the likelihood function.)

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
BadParam=function(x,param,pred) return(FALSE)
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# graph.modeldiag
# </name>
# <description>
#  Graph diagnostics of model.xy

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
graph.modeldiag=function(fit,burn=500,llikelim=50,graphpar=1:3)
{
 dev=dev.set(2)
 if(dev==1) 
  {
   graphics.off()
   x11(height=9,width=9,xpos=200)
  }
 full=data.frame(fit$full[-(1:burn),])
 plot(full[,graphpar])
 
 dev=dev.set(3)
 if(dev!=3) x11(height=7,width=9,xpos=600)
 llikerange=c(max(fit$llike)-llikelim,max(fit$llike))
 plot(fit$llike,ylim=llikerange)
 
 dev=dev.set(4)
 if(dev!=4) x11(height=9,width=7,xpos=800)
 par(mfcol=c(length(graphpar),1),mai=c(.4,.4,0,0))
 for(p in graphpar) plot(fit$full[,p])
 
 dev=dev.set(5)
 if(dev!=5) x11(height=9,width=7,xpos=400)
 par(mfcol=c(length(graphpar),1),mai=c(.4,.4,0,0))
 for(p in graphpar) plot(fit$full[-(1:burn),p],fit$llike[-(1:burn)],ylim=llikerange)

}




# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# bootstrap.corr
# </name>
# <description>
#  Running bootstrap on a correlation. Any columsn can be chosen from the submitted dataset, by number or name.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
bootstrap.corr=function(dataset,xcol=(-1),ycol=(-1),xcolname="no",ycolname="no",boot=100)
{
 norecords=dim(dataset)[1]

 if(xcol<0) xcol=which(colnames(dataset)==xcolname)
 else xcolname=colnames(dataset)[xcol]
 if(ycol<0) ycol=which(colnames(dataset)==ycolname)
 else ycolname=colnames(dataset)[ycol]

 x=dataset[,xcol]
 y=dataset[,ycol]
 plot(x,y,pch=16,xlab=xcolname,ylab=ycolname)

 inter=corrcoef=rsq=prob=numeric()

 for(i in 1:boot)
  {
   s=sample(1:norecords,norecords,replace=T)

   fit=lm(y[s]~x[s])

   inter[i]=summary(fit)$coef[1,1]
   corrcoef[i]=summary(fit)$coef[2,1]
   prob[i]=summary(fit)$coef[2,4]
   rsq[i]=summary(fit)$r.squared

   if(i<=100) abline(fit)
  }

 if(boot<200)
  {
   interci=bootconf(inter)
   corrci=bootconf(corrcoef)
   probci=bootconf(prob)
   rsqci=bootconf(rsq)
  }
 else
  {
   interci=quantile(inter,prob=c(.025,.975))
   corrci=quantile(corrcoef,prob=c(.025,.975))
   probci=quantile(prob,prob=c(.025,.975))
   rsqci=quantile(rsq,prob=c(.025,.975))
  }

 return(list(inter=interci,corr=corrci,prob=probci,rsq=rsqci))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# bootconf
# </name>
# <description>
#  A simple calculation of confidence limits based on the SD of a vector.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
bootconf=function(x)
{
 lower=mean(x)-1.96*sd(x)
 upper=mean(x)+1.96*sd(x)

 return(c(lower,upper))
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# metrop1step
# </name>
# <description>
#  Takes a single metropolis step on a single parameter for any given likelihood function.
# The arguments start.param and scale.param are atomic (single values), as are adjust and target. 
# The ellipses handle all other arguments to the function. The function func must accept the test 
# parameter as the first argument, plus any additional arguments which come in the ellipses.

# Note the metropolis rule: if rejected, the old value is returned to be re-used. The return value
# includes a one if accepted, zero if rejected.

# The step size, refered to as scale.param, is adjusted following Helene's rule. 
# For every acceptance, scale.param is multiplied
# by adjust, which is a small number > 1 (1.01, 1.02, 1.1 all seem to work). For every rejection, scale.param
# is multiplied by (1/adjust); for every acceptance, by adjust^AdjExp, the latter based on the target acceptance rate.
# When the target acceptance rate is 0.25, which is recommended for any model with > 4 parameters,
# AdjExp=3. It's easy to see how this system arrives at an equilibrium acceptance rate=target.

# The program calling metrop1step has to keep track of the scaling parameter: submitting it each time
# metrop1step is called, and saving the adjusted value for the next call. Given many parameters, a
# scale must be stored separately for every one.

# Note the return value is a vector of 6:
# 1) the new parameter value;
# 2) the new scale (step size);
# 3) a zero or a one to keep track of the acceptance rate;
# 4) the likelihood of original parameter (if rejected) or new parameter (if accepted)
# 5) the likelihood of original parameter (if accepted) or new parameter (if rejected)
# 6) the new parameter tested (whether accepted or not)

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
metrop1step=function(func,start.param,scale.param,adjust,target,...)
{
 origlike=func(start.param,...)
 newval=rnorm(1,mean=start.param,sd=scale.param)
 newlike=func(newval,...)

 if(is.na(newlike)) browser()

 AdjExp=(1-target)/target

## It's possible with shifting parameters to get trapped where start.param is invalid.
# This test allows an escape  
 if(origlike==(-Inf))
  { likeratio=IfElse(newlike==(-Inf),0,1); browser }
 else likeratio=exp(newlike-origlike)

 if(is.na(likeratio)) browser()

 if(runif(1)<likeratio)   ## Accept
  {
   newscale=scale.param*adjust^AdjExp
   return(c(newval,newscale,1,newlike,origlike,newval))
  }
 else                     ## Reject
  {
   newscale=scale.param*(1/adjust)
   return(c(start.param,newscale,0,origlike,newlike,newval))
  }

}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# metrop1step.discrete
# </name>
# <description>
#  A version for metrop1step where the alternative values are character states with no numeric meaning.
# A random draw must be taken from all possible states, each with equal probability. There
# is no step-size thus no adjustment.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
metrop1step.discrete=function(func,start.param,alter.param,...)
{
 origlike=func(start.param,...)

 r=sample(x=1:length(alter.param),size=1)
 newval=alter.param[r]
 newlike=func(newval,...)

 if(is.na(newlike)) browser()

 likeratio=exp(newlike-origlike)
# if(is.na(likeratio)) browser()

 if(runif(1)<likeratio) 
  {
   newscale=NULL
   return(c(newval,newscale,1,newlike,origlike,newval))
  }
 else
  {
   newscale=NULL
   return(c(start.param,newscale,0,origlike,newlike,newval))
  }

}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# testmcmcfunc
# </name>
# <description>
#  For testing mcmc1step. No longer used. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
testmcmcfunc=function(x,mn,s)
{
 likel=dnorm(x,mean=mn,sd=s,log=TRUE)
 
 cat(x,mn,s,likel,"\n")
# browser()
 return(likel)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# CI
# </name>
# <description>
# Confidence limits (quantiles) from a vector at specified probabilities. Default is 95% confidence interval. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
CI=function(x,prob=c(.025,.975),na.rm=FALSE) 
     return(quantile(x,prob=prob,na.rm=na.rm))



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# hist.compare
# </name>
# <description>
# Compares two histograms with a Kolmogorov approach.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
hist.compare=function(x,y,div,breaks=NULL)
{
 y=y[!is.na(y)]
 x=x[!is.na(x)]
 obsrange=range(x)
 
 if(is.null(breaks)) breaks=seq(obsrange[1],obsrange[2]+div,by=div)

 xcat=cut(x,breaks=breaks,right=FALSE)
 # xcount=table(xcat)
 ycat=cut(y,breaks=breaks,right=FALSE)
 ycount=table(ycat)
 # value=tapply(y,ycat,mean)

 pred=ycount/sum(ycount)
 pred[pred==0]=1/(2*sum(ycount))
 pred=pred/sum(pred)

 m=match(xcat,names(pred))
 llike=log(pred[m])
  browser()

 return(sum(llike))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# harmonic.mean
# </name>
# <description>
#  Harmonic mean of a vector x. NAs and nonzero values can be ignored, and a constant can be added to every x.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
harmonic.mean=function(x,add=0,na.rm=TRUE)
{
 x=x+add

 missing=which(x<=0 | is.na(x))
 if(length(missing)>0)
  {
   if(!na.rm) return(NA)
   x=x[-missing]
  }

 logx=log(x)
 return(exp(mean(logx)))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# cumul.above
# </name>
# <description>
#  Given y values as a function of x, this seeks the x at which the curve passes through a given y. It sets
# a variable whichabove to 0 for all cases where y>cutoff, otherwise 0, then fits a logistic regression.
# The midpoint of the logistic regression is a good estimate. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
cumul.above=function(x,y,cutoff,logaxs="",graphit=TRUE,returnfull=FALSE)
{
 whichabove=y>cutoff
 if(logaxs=="x" | logaxs=="xy") runx=log(x)
 else runx=x

 fit=glm(whichabove~runx,family=binomial)

 if(logaxs=="x" | logaxs=="xy") mid=exp(-fit$coef[1]/fit$coef[2])
 else mid=(-fit$coef[1]/fit$coef[2])

 xord=x[order(x)]
 yord=y[order(x)]
 if(graphit)
  {
   plot(xord,yord,pch=16,log=logaxs)
   abline(v=mid)
   abline(h=cutoff)
  }

 return(mid)
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# sumsq
# </name>
# <description>
#  A trivial function used in minimizing sums of squares.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
sumsq=function(x) 
  return(sum(x^2,na.rm=T))



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# is.odd
# </name>
# <description>
#  A trivial function to test whether numbers (scalar or vector) are odd. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
is.odd=function(x)
{
 answer=rep(FALSE,length(x))
 y=which(x%%2!=0)
 answer[y]=TRUE
 return(answer)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# border.distance
# </name>
# <description>
#  Returns distance from a point to the nearest boundary of a rectangle (plot). Accepts either separate
# x-y coordinates, or an object where x is first column, y is second. The lower left corner of the plot is
# assumed to be 0,0. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
border.distance=function(x,y,plotdim=c(1000,500),pt=NULL)
{
 if(!is.null(pt)) 
  {
   x=pt[,1]
   y=pt[,2]
  }
  
 missingx = is.na(x) | x<0 | x>=plotdim[1]
 missingy = is.na(y) | y<0 | y>=plotdim[2]

 closestx=closesty=closest=numeric()

 lefthalf = x<plotdim[1]/2 & !is.na(x)
 bottomhalf = y<plotdim[2]/2  & !is.na(y)

 closestx[lefthalf]=x[lefthalf]
 closestx[!lefthalf]=plotdim[1]-x[!lefthalf]
 closesty[bottomhalf]=y[bottomhalf]
 closesty[!bottomhalf]=plotdim[2]-y[!bottomhalf]

 xcloser=closestx<closesty

 closest[xcloser]=closestx[xcloser]
 closest[!xcloser]=closesty[!xcloser]
 closest[missingx | missingy]=NA

 return(closest)
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# regsum
# </name>
# <description>
#  This carries out either first or second order polynomial regression,
# finds the x- and y-values at y's peak if its second order,
# otherwise the x-intercept.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
regsum=function(x,y,poly=1,graphit=F,yrange=c(-1,-1),yaxslab="multiplier",
                newgraph=F,add=F,pts=T,ptype=16,ltype="solid")
{
 x2=x^2
 x3=x^3
 if(poly==1) fit=lm(y~x)
 else if(poly==2) fit=lm(y~x+x2)
 else if(poly==3) fit=lm(y~x+x2+x3)

 a=summary(fit)$coef[1,1]
 b=summary(fit)$coef[2,1]
 c=d=0
 if(poly>1) c=summary(fit)$coef[3,1]
 if(poly>2) d=summary(fit)$coef[4,1]

 pred=a+b*x+c*x2+d*x3

 if(graphit)
  {
   if(yrange[2]<0) yrange=c(1,max(y))
   if(newgraph) win.graph(height=4,width=6)
   if(add)
    {
     if(pts) points(x-31,y,pch=ptype)
     lines(x-31,pred,lty=ltype)
    }
   else
    {
     if(pts)
      {
       plot(x-31,y,pch=ptype,ylim=yrange,xlab="date",ylab=yaxslab)
       lines(x-31,pred,lty=ltype)
      }
     else plot(x-31,pred,pch=ptype,ylim=yrange,xlab="date",ylab=yaxslab,type="l",lty=ltype)
    }
  }

 if(poly==1)
  {
   peakday=(-a/b)
   peak=NA
  }
 else if(poly==2)
  {
   peakday=(-b/(2*c))
   peak=a-b^2/(4*c)
  }

 return(list(pred=pred,peak=peak,peakday=peakday))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# colMedians
# </name>
# <description>
#  For convenient medians, like colMeans.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
colMedians=function(mat,na.rm=TRUE)
  return(apply(mat,2,median,na.rm=na.rm))
# </source>
# </function>
# 
# 
# <function>
# <name>
# midPoint
# </name>
# <description>
#  Midpoint of any vector.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
midPoint=function(x,na.rm=TRUE)
  return(min(x)+0.5*diff(range(x,na.rm=na.rm)))
# </source>
# </function>
# 
# 
