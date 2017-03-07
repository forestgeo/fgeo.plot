
# 
# <function>
# <name>
# doublenormal
# </name>

# <description>
# Simulate draws from a double normal
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
doublenormal=function(N=1e4,m=0,sd1=1,sd2=seq(0,2,by=.1))
{
 n1=rnorm(N,m,sd1)
 newsd=numeric()

 for(i in 1:length(sd2))
  {
   n2=rnorm(N,n1,sd2[i])
   newsd[i]=sd(n2)
  }

 sqrtdiff=1/sqrt(newsd-sd2)
 plot(sd2,sqrtdiff)
 cor=lm(sqrtdiff~sd2)
 abline(cor)

 return(data.frame(sd1,sd2,newsd))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dnormprod0
# </name>
# <description>
# Probability density of product of two normal variates, both with mean 0, SD sx and sy
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dnormprod0=function(z,sx,sy)
{
 K=besselK(abs(z)/(sx*sy),nu=0)
 
 return(K/(pi*sx*sy))
}
 
 

# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dgammadexp
# </name>
# <description>
# PDF of a function formed by adding a gamma distribution to a symmetrical exponential 
# distribution. This means simply adding a PDF for a gamma minus an exponential to the
# PDF for a gamma plus an exponential.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dgammadexp=function(z,mean,sd,lambda,draws=10000,div=.1,xrange=c(-10,30),graphit=F)
{
 sumpart=dgammaPlusdexp(z,mean,sd,lambda)
 diffpart=dgammaMinusdexp(z,mean,sd,lambda)
 result=0.5*sumpart+0.5*diffpart
 
 if(graphit)
  {
   r=mean/(sd^2)
   a=mean*r
   
   growth=rgamma(draws,shape=a,rate=r)
   error=rsymexp(draws,center=0,rate=lambda)
   
   obs=growth+error
   minx=min(obs)-div
   maxx=max(obs)+div
   x=seq(minx,maxx,by=div)
   hist(obs,breaks=x,xlim=xrange)
   lines(z,draws*div*result)
  }
 
 return(result)
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# dgammaMinusdexp
# </name>
# <description>
# The PDF of the difference between a gamma and a negative exponential distribution. The shape and rate of the gamma 
# are a and r; mean and sd are the mean and sd of the gamma. Lambda is the rate of the exponential. 
# This comes from the convolution of the two distributions, which is also a gamma, and the integral 
# of the new gamma evaluated with pgamma. Note that lambda is the rate of the exponential.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dgammaMinusdexp=function(z,mean,sd,lambda,draws=10000,div=.01,xrange=c(0,25),xmax=4,graphit=F)
{
 r=mean/(sd^2)
 a=mean*r
 
 part1=a*(log(r/(r+lambda)))+log(lambda)+lambda*z
 part2=pgamma(z,shape=a,rate=r+lambda,lower.tail=FALSE,log=TRUE)
 
 pdf.z=numeric()

 pdf.z[z>0]=exp(part1+part2)[z>0]
 pdf.z[z<=0]=exp(part1[z<=0])
 
 if(graphit)
  {
   gamma=rgamma(draws,shape=a,rate=r)
   expon=rexp(draws,rate=lambda)
 
   net=gamma-expon

   highvalue=max(max(z),max(gamma),max(expon),max(net))
   lowvalue=min(min(z),min(gamma),min(expon),min(net))
 
   x=seq(lowvalue-div,highvalue+div,by=div)
   dev.set(2)
   hist(gamma,breaks=x,xlim=xrange)
   dev.set(3)
   hist(expon,breaks=x,xlim=xrange)
   dev.set(4)
   hist(net,breaks=x,xlim=xrange)
  } 

# browser() 
 return(pdf.z)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dgammaPlusdexp
# </name>
# <description>
# The PDF of the sum of gamma and negative exponential distribution. The shape and rate of the gamma 
# are a and r; mean and sd are the mean and sd of the gamma. Lambda is the rate of the exponential. 
# This is only an appoximation based on the observation that the resulting distribution is very
# close to a gamma. So I simply work out a new gamma whose mean is the sum of the means of
# the initial gamma and exponential, and likewise for the new variance. 

# As long as gamma\'s rate > the exponential lambda, the distribution
# can be specified (using pgamma) as in dgammaMinusdexp. But if rate < lambda, this fails.

# The gamma approximation fails if the sd is sufficiently higher than the mean, and the mean
# is low. Then the gamma is absurdly skewed, and the shape of the sum is dominated by the exponential
# at low z, nothing like a true gamma. It appears to work reasonably well as long as the
# mean >= sd, or even if mean>=0.5*sd.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dgammaPlusdexp=function(z,mean,sd,lambda,draws=10000,div=.01,xrange=c(0,25),xmax=4,graphit=FALSE)
{
 r=mean/(sd^2)
 a=mean*r
 
 pdf.z=numeric()
 pdf.z[z<=0]=0
 part2=numeric()
   
 if(graphit)
  {
   gamma=rgamma(draws,shape=a,rate=r)
   expon=rexp(draws,rate=lambda)
 
   net=gamma+expon

   highvalue=max(max(z),max(gamma),max(expon),max(net))
   x=seq(0,highvalue+div,by=div)
   dev.set(2)
   hist(gamma,breaks=x,xlim=xrange)
   dev.set(3)
   hist(expon,breaks=x,xlim=xrange)
   dev.set(4)
   hist(net,breaks=x,xlim=xrange)
  }
 
 newmean=mean+1/lambda
 newvar=sd^2+1/(lambda^2)
 newshape=newmean^2/newvar
 newrate=newmean/newvar
 pdf.z[z>0]=dgamma(z[z>0],shape=newshape,rate=newrate)
 if(graphit) lines(z,draws*div*pdf.z)

 # browser() 
 return(pdf.z)
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# dgamma.meansd
# </name>
# <description>
# Probability distribution of gamma, parameterized with mean and sd instead of shape and scale.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dgamma.meansd=function(x,mean,sd,log=FALSE)
{
 k=(sd^2)/mean
 s=mean/k
 
 return(dgamma(x,shape=s,scale=k,log=log))
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# rgamma.meansd
# </name>
# <description>
# Random draws of gamma, parameterized with mean and sd instead of shape and scale.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
rgamma.meansd=function(n,mean,sd)
{
 k=(sd^2)/mean
 s=mean/k
 
 return(rgamma(n=n,shape=s,scale=k))
}
# </source>
# </function>
# 
#
# <function>
# <name>
# dgamma.mean
# </name>
# <description>
# A version of dgamma where the parameters are ordered so that the mean (mu = shape x scale) is the first argument.
# The second argument is x, the value at which dgammma is evaluated, and the third is scale. This is needed
# for use with mcmc1step, to do a metropolis step on the mean. Only mu can be a vector.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dgamma.mean=function(mu,x,k)
{
 s=mu/k
 if(k<=0 | x<=0) return(-Inf)
 if(length(mu[mu<=0])>0) return(-Inf)
 
 llike=dgamma(x,shape=s,scale=k,log=TRUE)

 return(llike)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dgamma.scale
# </name>
# <description>
# Like above, but with scale as the first parameter.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dgamma.scale=function(k,x,mu)
{
 s=mu/k
 return(dgamma(x,shape=s,scale=k))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dpower
# </name>
# <description>
# A probability distribution defined by a power function. There is a dpareto in R, but quite different, with
# two parameters. In this, the exponent is the only parameter, and it must be < (-1); x must be >= 0.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dpower=function(x,beta,log=FALSE)
{
 if(beta>=(-1)) return(rep(NA,length(x)))

 xpos=x>=0
 y=numeric()

 y[xpos]=log(-beta-1)+beta*log(x[xpos]+1)
 if(!log) y[xpos]=exp(y[xpos])

 return(y)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# rpower
# </name>
# <description>
# Random draws based on the integral.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
rpower=function(n,beta)
{
 r=runif(n)
 k=1/(beta+1)
 return((1-r)^k-1)
}
# </source>
# </function>
# 
# 

# <function>
# <name>
# dasympower
# </name>
# <description>
# A bilateral power distribution, centered at center, decaying with exponent rate1 for positive x and rate2 for negative x. Both rate1 and rate2
# must be < (-1). See dpower, this is analogous to dasymexp for dpower. By R. Chisholm. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dasympower=function(x,center,rate1,rate2,log=FALSE)
{
 logy=numeric()
 right=x>=center
 left=x<center
 a = 1/(1/(-rate1-1) + 1/(-rate2-1))

 logy[right]=log(a)+(rate1*log(-center+x[right]+1))
 logy[left]=log(a)+(rate2*log(+center-x[left]+1))
 
 if(log) return(logy)
 return(exp(logy))
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# rasympower
# </name>
# <description>
# Random draws from the bilateral power distribution, dasympower. By R. Chisholm. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
rasympower <- function( n, rate1, rate2, c )
   return (qasympower(runif(n),rate1,rate2,c))
# </source>
# </function>

# <function>
# <name>
# qasympower
# </name>
# <description>
# Quantiles from the bilateral power distribution, dasympower. By R. Chisholm. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
qasympower <- function(y, rate1, rate2, c)
{
 y=1-y
 a=1/(1/(rate1-1)+1/(rate2-1))
 return (ifelse( y < a/(rate1-1),
             1+c-((rate1-1)*y/a)^(1/(1-rate1)),
             ((a/(rate1-1)+a/(rate2-1)-y)*(rate2-1)/a)^(1/(1-rate2))+c-1 ))
}             
# </source>
# </function>

# <function>
# <name>
# dsymexp
# </name>
# <description>
# Probability distribution for a folded, symmetrical exponential. When x>=center, 
# it's just a standard exponential. When x<center, it's the mirror image of same one.
# Each must be divided by two, though, in order to integrate to one.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dsymexp=function(x,center,rate,log=FALSE)
{
 logy=numeric()
 right=x>=center
 left=x<center

 logy[right]=log(0.5)+log(rate)+(-rate*(x[right]-center))
 logy[left]=log(0.5)+log(rate)+(-rate*(center-x[left]))

 if(log) return(logy)
 
 #if(length(which(is.na(y)))>0) browser()
 #if(length(which(y<=0))>0) browser()
 
 return(exp(logy))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# psymexp
# </name>
# <description>
# The CDF for the symmetric exponential.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
psymexp=function(x,center,rate)
{
 y=numeric()
 right=x>=center
 left=x<center
 
 y[left]=0.5*exp(-rate*(center-x[left]))
 y[right]=1-0.5*exp(-rate*(x[right]-center))
 
 return(y)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# rsymexp
# </name>
# <description>
# Drawing a random variate on the symmetric exponential, based on the cumulative 
# probability, as given in psymexp. A random uniform number on (0,1) is plugged in 
# the inverse of the cumulative distribution.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
rsymexp=function(n,center,rate)
{
 y=numeric()
 
 r=runif(n)
 left=r<0.5
 right=r>=0.5
 
 y[left]=center+log(2*r[left])/rate
 y[right]=center+-log(2*(1-r[right]))/rate 
 
 return(y)
} 
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dasymexp
# </name>
# <description>
# Probability distributions for a folded but asymmetrical exponential. 
# When x>=center, it's a standard exponential. When x<center, it's the mirror image 
# of a different exponential; rate1 refers to the right half, rate2 to the
# left. The center is not the median: the section x>center has integral rate2/(rate1+rate2),
# and the section x<center rate1/(rate1+rate2). 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dasymexp=function(x,center,rate1,rate2,log=FALSE)
{
 logy=numeric()
 right=x>=center
 left=x<center
 k=rate1*rate2/(rate1+rate2)

 logy[right]=log(k)+(-rate1*(x[right]-center))
 logy[left]=log(k)+(-rate2*(center-x[left]))
 
 if(log) return(logy)
 return(exp(logy))
}
# </source>
# </function>
# 
# <function>
# <name>
# qasymexp
# </name>
# <description>
# Quantiles of dasymexp 
# </description>
# <arguments>
# y is the vector of desired quantiles; c is the center parameter; rate1 is the rate for the right half, and rate2 the left.
# </arguments>
# <sample>
# 
# </sample>
# <source>
qasymexp=function(y,rate1,rate2,c)
{
 lower=y<rate1/(rate1+rate2)
 result=rep(NA,length(y))
 
 result[lower] =c-(1/rate2)* log(y[lower]*(rate1+rate2)/rate1)
 result[!lower]=c+(1/rate1)*log(y[!lower]*(rate1+rate2)/rate2)
 
 return(result)
}
# </source>
# </function>

# 
# <function>
# <name>
# dasymexp
# </name>
# <description>
# Probability distributions for an asymmetrical Gaussian, that is with different standard deviations
# above and below the mode, or center. The mode is not the mean, though. The SD on the right is sigma1,
# and on the left, sigma2. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dasymnorm=function(x,center,sigma1,sigma2,log=FALSE)
{
 y=numeric()
 right=x>=center
 left=x<center

 sigma=(sigma1+sigma2)/2
 
 if(log) 
  {
   y[right]=log(sigma1/sigma)+dnorm(x[right],mean=center,sd=sigma1,log=log)
   y[left]=log(sigma2/sigma)+dnorm(x[left],mean=center,sd=sigma2,log=log)
  }
 else
  {
   y[right]=(sigma1/sigma)*dnorm(x[right],mean=center,sd=sigma1,log=log)
   y[left]=(sigma2/sigma)*dnorm(x[left],mean=center,sd=sigma2,log=log)
  }
  
 return(y)
}
# </source>
# </function>
# 

# <function>
# <name>
# minum.normal
# </name>
# <description>
# The likelihood function for use by fitnorm.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
minum.normal=function(param,x,y)
{
 m=param[1]
 s=param[2]
 k=param[3]
 SD=param[4]
 
 if(SD<=0 | s<=0) return(-Inf)

 pred=k*dnorm(x,mean=m,sd=s)
 llike=dnorm(y,mean=pred,sd=SD,log=TRUE)
 
 return(sum(llike))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# fitnorm
# </name>
# <description>
#  Fitting a normal distribution to data
# Parameters are a mean and sd of the normal being fitted, a scaling parameter k
# and the last SD is for the likelihood of the deviations.


# </description>
# <arguments>
#  y is vector data to be fitted, at points x

# </arguments>
# <sample>
# 
# </sample>
# <source>
fitnorm=function(start.param=c(-1,-1,1),x,y)
{
# y=y/sum(y)

 if(start.param[1]<0)
  {
   mstart=sum(x*y)
   varstart=sum(y*(x-mstart)^2)

   start.param[1]=mstart
   start.param[2]=sqrt(varstart)
  }

 fit=optim(start.param,minum.normal,x=x,y=y,control=list(fnscale=-1))

 return(fit$par)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# fit.pdf
# </name>
# <description>
# Fit a random variable x to any submitted probability distribution. The number of start parameters
# must match what the pdf needs.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
fit.pdf=function(start.param=c(1,1),data,pdf=dnorm,xrange=c(0,1),badpar=default.badpar)
{
 if(length(start.param)==1) 
  {
   optim.pdf   =    function(param,x,func,bad)
    {
     if(bad(par)) return(-Inf)
     return(sum(pdf(x,param,log=TRUE)))
    }
   fit=optimize(f=optim.pdf,x=data,interval=xrange,bad=badpar,maximum=TRUE)
   return(fit$maximum)
  }
 if(length(start.param)==2) 
  {
   optim.pdf   =     function(param,x,func,bad)
    {
     if(bad(param)) return(-Inf)
     return(sum(pdf(x,param[1],param[2],log=TRUE)))
    } 
   fit=optim(par=start.param,fn=optim.pdf,x=data,func=pdf,bad=badpar,control=list(fnscale=-1))
   return(fit$par)
  }

 return('start.param must be length 1 or 2')
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# default.badpar
# </name>
# <description>
#  None given.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
default.badpar=function(param)
  return(FALSE)


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# bad.paretopar
# </name>
# <description>
# Test whether parameters for the Pareto distribution are acceptable. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
bad.paretopar=function(param)
 {
  if(param[2]<=1) return(TRUE)
  return(FALSE)
 }


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# normalproduct
# </name>
# <description>
# A function which returns the product of 2 normal distributions, the first
# at x (a vector), the second at lag-x (lag is a scalar). The mean and SD 
# of the second normal are linear functions of x, with meanint being the
# intercept, meanslope the slope, and CV the coefficient of variation.  

# A convolution!


# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
normalproduct=function(x,lag,mean1,sd1,meanint,meanslope,CV)
{
 mean2=meanint+meanslope*x
 sd2=CV*mean2

 n1=dnorm(x,mean1,sd1)
 n2=dnorm(lag-x,mean2,sd2)

 return(n1*n2)
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dbeta.reparam
# </name>
# <description>
# This reparameterizes the beta distribution as a function of its mean and
# standard deviation. The mean must be between 0 and 1, and sd>0.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dbeta.reparam=function(x,mu,sd)
{
 a=(1-mu)*mu^2/(sd^2)
 b=(a/mu)-a

 return(dbeta(x,shape1=a,shape2=b))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# betaproduct
# </name>
# <description>
# This is equivalent to the normal product above.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
betaproduct=function(x,lag,mean1,sd1,meanint,meanslope,CV)
{
 mean2=meanint+meanslope*x
 sd2=CV*mean2

 p1=dbeta.reparam(x,mean1,sd1)
 p2=dbeta.reparam(lag-x,mean2,sd2)
 
 return(p1*p2)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# beta.normalized
# </name>
# <description>
#  Normalzing beta.total. No longer used. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
beta.normalized=function(x,par1,xmin,par2,xmax)
{
 intvalue=try(integrate(beta.total,lower=xmin,upper=xmax,
                        par1=par1,xmin=xmin,par2=par2,xmax=xmax)$value)
 if(!is.null(attributes(intvalue))) intvalue=0

 integral.beta=intvalue

 y=rep(0,length(x))
 if(integral.beta==0) return(y)

 inrange=x>=xmin & x<=xmax

 y[inrange]=beta.total(x[inrange],par1,xmin,par2,xmax)/integral.beta
 
 return(y)
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# beta.total
# </name>
# <description>
#  A beta distribution on the interval xmin to xmax, instead of 0 to 1. No longer used. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
beta.total=function(x,par1,xmin,par2,xmax)
{
 y=(x-xmin)^par1*(xmax-x)^par2

 if(length(y[is.infinite(y)])>0) return(rep(0,length(y)))

 return(y)
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# fit.beta.normal
# </name>
# <description>
#  Finding a normal distribution which most closely fits a given beta distribution.
# Parameters for a beta function are submitted, and the best fit mean and SD of a
# normal distribution returned.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
fit.beta.normal=function(x,par1,xmin,par2,xmax)
{
 y=beta.normalized(x,par1,xmin,par2,xmax)
 plot(x,y,type="l")

 fit=optim(c((xmax-xmin)/2,(xmax-xmin)/2),minum.beta.normal,x=x,y=y)

 pred=dnorm(x,mean=fit$par[1],sd=fit$par[2])
 lines(x,pred,col="red")

 return(fit$par)
}




# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# minum.beta.normal
# </name>
# <description>
#  Function to be minimized for fitting normal to beta.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
minum.beta.normal=function(param,x,y)
{
 pred=dnorm(x,mean=param[1],sd=param[2])
 dev=(y-pred)

 return(sum(dev^2))
}



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dbinomrev
# </name>
# <description>
#  A version of dbinom in which parameters are submitted in a different order. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dbinomrev=function(trial,p,success) 
  return(dbinom(x=success,size=trial,prob=p))
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dnormrev
# </name>
# <description>
#  This reverses the order of parameters to dnorm, so that outer can be used
# with a vector of x, and two vectors for mean and sd (the latter two equal in
# length). 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dnormrev=function(m,x,s,log=F) return(dnorm(x,mean=m,sd=s,log=log))


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dpois.rearrange
# </name>
# <description>
# This rearranges dpois so that it works on a single vector, with the first
# element being x and the remaining all being used as lambdas.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dpois.rearrange=function(v,log=F) 
  return(dpois(v[1],lambda=v[-1],log=log))
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# logit
# </name>
# <description>
#  Logit transformation for a probability >0 and < 1

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logit=function(p)
{
 result=p
 inc=(p>0 & p<1 & !is.na(p))
 inc[is.na(p)]=FALSE
 
 result[inc]=log(p[inc]/(1-p[inc]))
 result[!inc]=NA
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# invlogit
# </name>
# <description>
#  Inverse logit transformation, turns a logit back into a probability.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
invlogit=function(x) return(exp(x)/(1+exp(x)))
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# pweibull.3param
# </name>
# <description>
#  CDF of three-parameter Weibull
# (http://www.itl.nist.gov/div898/handbook/apr/section1/apr162.htm)

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
pweibull.3param=function(x,x0,shape,scale)
{
 z=(x-x0)/scale
 y=1-exp(-z^shape)
 y[x<=x0]=0
 
 return(y)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dweibull.3param
# </name>
# <description>
# PDF of three-parameter Weibull
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dweibull.3param=function(x,x0,shape,scale)
{
 z=(x-x0)/scale
 y=(shape/(x-x0))*(z^shape)*exp(-z^shape)
 y[x<=x0]=0
 
 return(y)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# weibull.median.3param
# </name>
# <description>
#  Median of three-parameter Weibull


# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
weibull.median.3param=function(x0,shape,scale)
{
 return(scale*(log(2))^(1/shape)+x0)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# weibull.mean.3param
# </name>
# <description>
# Mean of three-parameter Weibull
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
weibull.mean.3param=function(x0,shape,scale)
{
 return(scale*gamma(1+1/shape)+x0)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# weibull.sd.3param
# </name>
# <description>
# SD of three-parameter Weibull
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
weibull.sd.3param=function(x0,shape,scale)
{
 return(sqrt(scale*scale*gamma(1+2/shape)-(scale*gamma(1+1/shape))^2))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dexp.sin
# </name>
# <description>
# Four-parameter exponential sin, as a probability distribution
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dexp.sin=function(x,lo,hi,skew,tail)
{
 integral=integrate(exponential.sin,lower=lo,upper=lo+hi,asymp=1,b=lo,c=hi,d=skew,e=tail)$val
 
 return(exponential.sin(x,asymp=1,b=lo,c=hi,d=skew,e=tail)/integral) 
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# exponential.sin
# </name>
# <description>
# Five-parameter exponential sin
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
exponential.sin=function(x,asymp,b,c,d,e)
{
 z=pi*((x-b)/c)^d
 y=asymp*((sin(z))^e)
 
 y[x<b]=0
 y[x>b+c]=0
 
 return(y)
} 



# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# pexp.sin
# </name>
# <description>
#  CDF of four-parameter exponential sin
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
pexp.sin=function(x,b,c,d,e)
{
 return(cumsum(dexp.sin(x,b,c,d,e))/sum(dexp.sin(x,b,c,d,e)))
}
# </source>
# </function>
# 
# 
# 
# 
# <function>
# <name>
# mvrnormRC
# </name>
# <description>
# Function that takes a variance-covariance matrix and produces normal variates
# following it, but with means 0. The R function mvrnorm does this too; this was a 
# test of the algorithm from Tommaso Zillio. Sigma must be square. N is the number
# to draw.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
mvrnormRC=function(N,Sigma)
{
 dimension=dim(Sigma)[1]
 SVD=svd(Sigma)
 M = SVD$u %*% diag(sqrt(SVD$d))

 norm=x=matrix(nrow=N,ncol=dimension)

 for(i in 1:N) norm[i,]=rnorm(dimension)
 for(i in 1:N) x[i,] = t(M %*% norm[i,])
 
 return(x)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# dmixnorm
# </name>
# <description>
# Mixed normal distribution. The parameter f is the probability 
# of following the first, with mean1 and sd1; 1-f is the probability
# for the second normal
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dmixnorm=function(x,mean1,mean2,sd1,sd2,f,log=FALSE)
{
 y1=dnorm(x,mean=mean1,sd=sd1)
 y2=dnorm(x,mean=mean2,sd=sd2)
 
 result=f*y1+(1-f)*y2
 
 if(log) return(log(result))
 else return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# rmixnorm
# </name>
# <description>
#  Random draw on the mixed normal distribution.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
rmixnorm=function(n,mean1,mean2,sd1,sd2,f)
{
 r=runif(n)
 part1=which(r>f)
 part2=which(r<=f)
 
 y1=rnorm(length(part1),mean=mean1,sd=sd1)
 y2=rnorm(length(part2),mean=mean2,sd=sd2)
 
 return(c(y1,y2))
}


# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# minum.mixnorm
# </name>
# <description>
# Fit a mixture of 2 normals.


# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
minum.mixnorm=function(param,data)
{
 mean1=param[1]
 mean2=param[2]
 sd1=param[3]
 sd2=param[4]
 f=param[5]
 
 llike=dmixnorm(data,mean1=mean1,mean2=mean2,sd1=sd1,sd2=sd2,f=f,log=TRUE)
 return(sum(llike))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# logistic.inter
# </name>
# <description>
# Logistic function with intercept parameterization (ie, first parameter is y when all x=0). The input x are all independent variables, in a matrix
# with each column one of the variables. The number of rows is the number of datapoints. Just one inter, which is the value
# at all x=0, and passed as param[1]. Slope parameters follow, one per column of x. 
# This is identical to standard 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logistic.inter=function(x,param,log=FALSE,...)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 inter=param[1]
 if(inter<=0 | inter>=1) return(rep(NA,dim(x)[1]))

 slope=param[2:(nopredictor+1)]
 asymp=IfElse(length(param)>nopredictor+1,param[nopredictor+2],1)
 basement=IfElse(length(param)>nopredictor+2,param[nopredictor+3],0)
 
 X=x%*%slope
 
 d=(1-inter)/inter

 y=exp(X)/(d+exp(X))
 result=y*(asymp-basement)+basement

 infinite.pos=which(is.infinite(exp(X)))
 result[infinite.pos]=asymp
 
 if(log) return(log(result))
 return(result)
}
# </source>
# </function>
# 
# 

 
# <function>
# <name>
# logistic.standard
# </name>
# <description>
# This is standard logistic function, but with asymptote and basement allowed. The latter are only implemented
# if extra parameters are passed. Moved from calc.surviv.r on 25 July 2010 to provide the standard logistic. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logistic.standard=function(x,param,log=FALSE,...)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 a=param[1]
 b=param[2:(nopredictor+1)]
 
 asymp=IfElse(length(param)>nopredictor+1,param[nopredictor+2],1)
 basement=IfElse(length(param)>nopredictor+2,param[nopredictor+3],0)

 X=x%*%b
 pwr=a+X
 y=invlogit(pwr)
 prob=y*(asymp-basement)+basement

 infinite.pos=which(is.infinite(exp(pwr)))
 prob[infinite.pos]=basement+asymp

 if(log) return(log(prob))
 return(prob)
}


# </source>
# </function>
# 
# # <function>
# <name>
# logistic.power
# </name>
# <description>
# This is the Gaussian logistic function, where logit is a second-order polynomial of x; with asymptote and basement allowed. 
# There must be 1+2*nopredictors parameters; the asympotote and basement are only implemented
# if extra parameters are passed.  
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logistic.power=function(x,param,log=FALSE,...)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 a=param[1]
 slopeindex=1:(2*nopredictor+1)
 b=param[slopeindex][!is.odd(slopeindex)]
 b2=param[slopeindex][is.odd(slopeindex)][-1]
 
 asymp=IfElse(length(param)>2*nopredictor+1,param[2*nopredictor+2],1)
 basement=IfElse(length(param)>2*nopredictor+2,param[2*nopredictor+3],0)

 X=x%*%b
 X2=(x^2)%*%b2
 pwr=a+X+X2
 y=invlogit(pwr)
 prob=y*(asymp-basement)+basement

 infinite.pos=which(is.infinite(exp(pwr)))
 # prob[infinite.pos]=basement+asymp
 prob[infinite.pos]=NA

 if(log) return(log(prob))
 return(prob)
}
# </source>
# </function>
# 
# <function>
# <name>
# logistic.power.mode
# </name>
# <description>
# This is the Gaussian logistic function, where logit is a second-order polynomial of x, but with third parameter the position
# of the critical point (peak or trough). Given 3 parameters for standard logistic.power, the mode is at -param[2]/2*param[3]).
# Asymptote and basement are allowed. There must be 1+2*nopredictors parameters; the asympotote and basement are only implemented
# if extra parameters are passed.  
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logistic.power.mode=function(x,param,log=FALSE,...)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 a=param[1]
 slopeindex=1:(2*nopredictor+1)
 b=param[slopeindex][!is.odd(slopeindex)]
 b2=param[slopeindex][is.odd(slopeindex)][-1]
 b2=(-1)*b/(2*b2)
 param[slopeindex][is.odd(slopeindex)][-1]=b2
 
 prob=logistic.power(x=x,param=param,log=log)
 return(prob)
}
# </source>
# </function>

# 
# <function>
# <name>
# logistic.power_simple
# </name>
# <description>
# This is a mixture of logistic and logistic-standard models. The predictors n get a power model, the remaining a simple
# model. So if nopredictors==8, and n=c(1,7), then the first and seventh predictors use a power model, while the rest a simple model.
# There must be 1+length(n)+nopredictors parameters, plus additional 1 or 2 for asymptote and basement.  
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logistic.power_simple=function(x,param,log=FALSE,N=1,...)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 noparam=length(param)

 n=N
 non=length(n)
 if(non==nopredictor) return(logistic.power(x=x,param=param,log=log))
 
 x1=as.matrix(x[,n])
 x2=as.matrix(x[,-n])
 
 a=param[1] 
 slopes=param[-1]
 b=b2=standardparam=numeric()
 counter=1
 
 for(j in 1:nopredictor)
  {
   if(j %in% n) 
     {
      b=c(b,slopes[counter])
      b2=c(b2,slopes[counter+1])
      counter=counter+2
     }
   else 
     {
      standardparam=c(standardparam,slopes[counter])
      counter=counter+1
     }
  }
          
 asymp=IfElse(length(param)>non+nopredictor+1,param[noparam-1],1)
 basement=IfElse(length(param)>non+nopredictor+2,param[noparam-2],0)
 # browser()
 
 X=x1%*%b
 X2=x1^2%*%b2
 Xstan=x2%*%standardparam

 pwr=a+X+X2+Xstan
 y=exp(pwr)/(exp(pwr)+1)
 prob=y*(asymp-basement)+basement

 infinite.pos=which(is.infinite(exp(pwr)))
 # prob[infinite.pos]=basement+asymp
 prob[infinite.pos]=NA

 if(log) return(log(prob))
 return(prob)
}
# </source>
# </function>

# <function>
# <name>
# logistic.ctr
# </name>
# <description>
# This is logistic function with intercept parameterization (see logistic above), but with centering on x allowed. 
# If center==NA, then the x values are centered on their median.
# Or center can be a number. If NULL, no centering is done. 
# Moved from calc.surviv.r on 25 July 2010 to provide the standard logistic. 

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logistic.ctr=function(x,param,log=FALSE,center=NA)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 # inter=param[1]
 # if(inter<=0 | inter>=1) return(rep(NA,dim(x)[1]))

 slope=param[2:(nopredictor+1)]
 asymp=IfElse(length(param)>nopredictor+1,param[nopredictor+2],1)
 basement=IfElse(length(param)>nopredictor+2,param[nopredictor+3],0)

 if(is.null(center)) x=x
 else if(is.na(center)) x=sweep(x,2,colMedians(x),'-')
 else x=sweep(x,2,center,'-')
  
 X=x%*%slope

 d=(1-inter)/inter
 y=exp(X)/(d+exp(X))
 result=y*(asymp-basement)+basement
 
 infinite.pos=which(is.infinite(exp(X)))
 result[infinite.pos]=asymp

 if(log) return(log(result))
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# logistic.multiplicative
# </name>
# <description>
#  Logistic with a pair of parameters for each x; y=product of all the logistics. First set of parameters are intercepts, then
# an equal number of slopes. If there are additional parameters, they are asymptote and basement.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logistic.multiplicative=function(x,param,log=FALSE)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 noparam=length(param)

 inter=param[1:nopredictor]
 slope=param[(nopredictor+1):(2*nopredictor)]

 # if(length(which(inter<=0 | inter>=1))>0) return(rep(NA,dim(x)[1]))

 asymp=IfElse(length(param)>2*nopredictor,param[2*nopredictor+1],1)
 basement=IfElse(length(param)>2*nopredictor+1,param[2*nopredictor+2],0)
 
 result=rep(1,dim(x)[1])
 for(i in 1:nopredictor)
  {
   paramset=param[c(i,i+nopredictor)]
   result=result*logistic.standard(x[,i],paramset)
  }
  
 result=basement+asymp*result

 if(log) return(log(result))
 return(result)
}

# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# constant
# </name>
# <description>
# A function to return a constant at all predictors x. The predictors are a numeric vector, or a matrix of
# many predictors (each column a single predictor). This function is useful in modeling, where the name of a function
# is passed; this allows modeling where a response is a constant across all values of x. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
constant=function(x,param,...) 
{
 if(is.null(dim(x))) return(rep(param,length(x)))
 return(rep(param,dim(x)[1]))
}


# </source>
# </function>
# 
# 
# <function>
# <name>
# center.predictors
# </name>
# <description>
# Transform all data by subtracting a constant, either the mean, median value, or a submitted constant. 
# The input may be a vector or a matrix. If a matrix, each column is centered on its mean (median), or by passing a
# vector of constants. Note that setting by=0 amounts to no change. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# center.predictors(x=c(1,4,7,0),by='median')
# center.predictors(x=c(1,4,7,0),by=2)
# </sample>
# <source>
center.predictors=function(x,by='mean')
{
 error='Type must be mean, median, or numbers to be subtracted\n'
 
 if(is.null(dim(x))) 
  {
   if(by[1]=='mean') return(x-mean(x,na.rm=TRUE))
   if(by[1]=='median') return(x-median(x,na.rm=TRUE))
   if(is.numeric(by)) return(x-by)
   return(error)
  }
  
 if(by[1]=='mean') return(sweep(x,2,colMeans(x)))
 if(by[1]=='median') return(sweep(x,2,colMedians(x)))

 if(is.numeric(by)) 
  {
   k=by
   for(i in 2:dim(x)[1]) k=rbind(k,by)
   return(x-k)
  }

 return(error)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# fit.logistic
# </name>
# <description>
# A function to fit a set of data y, observed at the vector x, to a generalized
# logistic function, using least squares.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
fit.logistic=function(x,y,start.param,tech="Nelder-Mead")
{ 
 fit=optim(start.param,logistic.sum.squares,x=x,obs=y,method=tech,control=list(maxit=50000)) 
 
 return(fit)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# logistic.sum.squares
# </name>
# <description>
# Sets a prediction based on a generalized logistic, then returns the sum
# of squared deviations
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logistic.sum.squares=function(param,x,obs)
{
 pred=gen.logistic(x,param)
 if(length(which(pred<1e-5))>0) pred=rep(0,length(x))

 return(sum((obs-pred)^2)) 
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# asymp.ht
# </name>
# <description>
# The function from Sean Thomas which produces an asymptote for y as a function of x. 
# Original version: y=ymax*(1-exp(-a*x^b))
# This is the centered version, with x normalized by dividing by parameter k, which is the x value at which
# y is half ymax. This eliminates correlation between the a and b parameters in the above version, but
# not the correlation between parameters 1 and 2.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
asymp.ht=function(x,param,...)
{
 ymax=param[1]
 k=param[2]
 b=param[3]

 xcent=x/k
 return(ymax*(1-2^(-xcent^b)))
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# asymp.ht.fixmax
# </name>
# <description>
# Same formulation, but the asymptote is fixed, so only two parameters fitted.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
asymp.ht.fixmax=function(x,param,asymp)
{
 ymax=asymp
 k=param[1]
 b=param[2]

 xcent=x/k
 return(ymax*(1-2^(-xcent^b)))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# exp.2par
# </name>
# <description>
# An exponential distribution with an asymptote.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
exp.2par=function(x,param,asymp)
{
 ymax=asymp
 a=param[1]
 rate=param[2]

 return(ymax*(1-a*exp(-rate*x)))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# linear.model
# </name>
# <description>
# A simple linear model, where the first parameter is intercept, remaining parameters are slopes
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
linear.model=function(x,param)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 a=param[1]
 b=param[2:(nopredictor+1)]
 
 return(a+x%*%b)
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# simple.model
# </name>
# <description>
# A trivial model to return a different value at every x. If param is atomic, then that value is returned for every x. Otherwise, param must be a vector of same size as x, and param is returned. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
simple.model=function(x,param)
{
 N=length(x)
 if(length(param)==1) return(rep(param,N))
 if(length(param)!=N) return(rep(NA,N))
 return(param)
}
# </source>
# </function>

# <function>
# <name>
# simple
# </name>
# <description>
# An even more trivial model to return x unchanged. The argument param is passed but not used. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
simple=function(x,param)
{
 return(x)
}
# </source>
# </function>


# <function>
# <name>
# linear.model.ctr
# </name>
# <description>
# A simple linear model, where the first parameter is intercept, second the slope, and x can be centered on their median. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
linear.model.ctr=function(x,param,xcenter=NULL)
{
 if(is.null(xcenter)) x=x-median(x)

 return(param[2]*x+param[1])
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# expon.model
# </name>
# <description>
# Exponential model, y = a exp(b1*x1 + b2*x2) for any number of predictors x. Compare to linear.model. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
expon.model=function(x,param)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 a=param[1]
 b=param[2:(nopredictor+1)]
 expon=a+x%*%b
 
 return(exp(expon))
}
# </source>
# </function>
# 

# <function>
# <name>
# log.model
# </name>
# <description>
# Logarithmic model, y = a + b1 log(x1) + b2 log(x2) for any number of predictors x. Compare to linear.model. All x should be positive.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
log.model=function(x,param)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 a=param[1]
 b=param[2:(nopredictor+1)]
 y=a+log(x)%*%b
  
 return(y)
}
# </source>
# </function>
# 

# <function>
# <name>
# constant.linear
# </name>
# <description>
# A model which is constant for x<lim, and linear for x>lim. The first parameter is the slope, 
# second the x value of break point, third the lower limit.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
constant.linear=function(x,param,xcenter=NULL)
{
 line=drp(parallel.line(0,m=param[1],param[2],param[3])[1,])
  
 y=linear.model(x,line)
 y[x<param[2]]=param[3]
 
 return(y)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# linearmodel.bin
# </name>
# <description>
# Multiple bin model predicting y as a function of x in several bins. Within each bin, y is a linear function of x. 
# A model with B bins has B-1 parameters for breaks points (initial B-1 parameters), B parameters as slopes (next B parameters), and one intercept (last parameter).
# Intercept is assigned at x=0 by default, but argument LINEARBINMEDIAN can be used to change. 
# This function accepts one set of parameters, separates the bin, slope, and intercept, and submits to the
# general version of the function (linearmodel.bin.set). 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
linearmodel.bin=function(x,param,...)
{
 x=as.matrix(x)
 
 extra=list(...)
 if(is.null(extra$LINEARBINMEDIAN)) medv=0
 else medv=extra$LINEARBINMEDIAN
  
 noparam=length(param)
 bins=(noparam)/2
 if(is.null(medv)) medv=median(x)
 
 if(bins==1) return(linear.model(x-medv,param))
 
 b=param[1:(bins-1)]-medv
 v=x-medv
 N=length(b)
 
 m=param[bins:(noparam-1)]
 inter=param[noparam]
 
 pred=linearmodel.bin.set(v=v,binparam=b,param=c(m,inter))
 return(pred)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# linearmodel.bin.set
# </name>
# <description>
# This does the work of calculating predicted values at each independent variable, given bin and line parameters separately, 
# the latter being slope and intercept parameters in one vector. 
# Completely revised June 2011 to use geometry.r functions for lines.
# Create a list of lines, one for each bin, and an intersection (intercept), one for each bin:
# This function cannot handle one bin, and linearmodel.bin escapes with linear.model if only one bin is sought
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
linearmodel.bin.set=function(v,param,binparam)
{
 m=param[-length(param)]
 inter=param[length(param)]
 
 failed=rep(NA,length(v))
 if((length(binparam)+1)!=length(m)) return(failed)
 sorted=sort(binparam)
 if(length(which(sorted!=binparam))>0) return(failed)
  
 K=IfElse(diff(range(v))>diff(range(binparam)),diff(range(v)),diff(range(binparam)))
 if(K==0) K=1
 lower=IfElse(min(v)<min(binparam),min(v)-K,min(binparam)-K)
 upper=IfElse(max(v)>max(binparam),max(v)+K,max(binparam)+K)
 
 b=c(lower,binparam,upper)
 # browser()
 
 bins=length(m)
 N=length(b)
 pts=data.frame(x=b,y=rep(NA,N))
 
 if(upper<0)           ##   True if every binparam and every v < 0, so every b is < 0
  {
   pts$y[N]=inter+m[bins]*upper
   # browser()
   for(i in (N-1):1) pts$y[i]=pts$y[i+1]-m[i]*(b[i+1]-b[i])
  }
 else if(lower>0)      ##   True if every binparam and every v > 0, so very first b > 0
  {
   pts$y[1]=inter+m[1]*lower
   # browser()
   for(i in 2:N) pts$y[i]=pts$y[i-1]+m[i-1]*(b[i]-b[i-1])
  }
 else if(upper>0)                 ##   True when the b and the v span 0
  {
   z=which(b>0)[1]                ##   Nov 2012: Discovered that this fails if upper==0, which happens if length(v)==length(binparam)==1 and either==0
   pts$y[z]=inter+m[z-1]*(b[z])
   # browser()
   for(i in (z-1):1) pts$y[i]=pts$y[i+1]-m[i]*(b[i+1]-b[i])
   if(z<N) for(i in (z+1):N) pts$y[i]=pts$y[i-1]+m[i-1]*(b[i]-b[i-1])
  }
 else if(upper==0)                ##   True when the b and the v extend are below 0 except the max, which == 0
  {
   z=which(b==0)                
   pts$y[z]=inter+m[z-1]*(b[z])
   # browser()
   for(i in (z-1):1) pts$y[i]=pts$y[i+1]-m[i]*(b[i+1]-b[i])
   if(z<N) for(i in (z+1):N) pts$y[i]=pts$y[i-1]+m[i-1]*(b[i]-b[i-1])
  }
 
 pred=rep(NA,length(v))
 for(i in 1:bins)
  {
   start=b[i]
   end=b[i+1]
   
   insection=v>=start & v<=end   
   # browser()
   thisline=pts.to.interceptslope(pts[i,],pts[i+1,])
   pred[insection]=linear.model(v[insection],param=thisline)   
  } 

 return(pred)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# addBinParam
# </name>
# <description>
# Given parameters for a model with N linear bins, creates parameters for N+1 bins which produce the same model. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
addBinParam=function(x,best,bin)
{
 if(bin==1)
  return(c(median(x),best[2],best[2],best[1]))

 internal=best[1:(bin-1)]
 slope=best[bin:(length(best)-1)]
 intercept=best[length(best)]
 
 div=c(min(x),internal,max(x))
 widest=which.max(diff(div))
 
 newbreak=0.5*diff(div[c(widest:(widest+1))])+div[widest]
 newinternal=sort(c(internal,newbreak))
 
 if(widest<bin) newslope=slope[c(1:widest,widest,(widest+1):bin)]
 else newslope=slope[c(1:widest,widest)]
 
 return(c(newinternal,newslope,intercept))
}
# </source>
# </function>
#
#
# <function>
# <name>
# logisticmodel.bin
# </name>
# <description>
# Multiple bin model predicting y as a function of x, where each segment is modeled as a standard logistic.
# A model with B bins has B-1 parameters for breaks points, B parameters as slopes, and one intercept (y at x=0).
# Within each bin, y is a linear function of x. 
# Predictor can centered at medv. 
# This function accepts a set of parameters, submits to linearmodel.bin, then returns invlogit.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
logisticmodel.bin=function(x,param,...)
{
 pred=invlogit(linearmodel.bin(x=x,param=param,...))
 return(pred)
}
# </source>
# </function>
# 

# <function>
# <name>
# constant.bin
# </name>
# <description>
# A model like piecewise regression (linearmodel.bin), but y is a constant within each bin.
# With B bins, 2B-1 parameters are needed. First B-1 parameters are bin breaks. The remaining B
# parameters are the constant value of y in each bin. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
constant.bin=function(x,param)
{
 noparam=length(param)
 if(noparam<3 | !is.odd(noparam)) return(rep(NA,length(x)))
 
 bins=(noparam+1)/2
 b=param[1:(bins-1)]
 m=param[bins:noparam]

 fullbreaks=c(min(x)-1,b,max(x)+1)
 xcat=cut(x,breaks=fullbreaks,right=FALSE,labels=1:bins)
 y=rep(m[1],length(x))
 
 for(i in 2:bins) y[xcat==i]=m[i]
 
 return(y)
}
# </source>
# </function>

# 
# 
# 
# <function>
# <name>
# dpois.max
# </name>
# <description>
# A probability distribution which is simply a curtailed poisson: all probability above a maximum integer,
# maxx, is given to that maximum. For all x<maxx, the probability is just poission. No normalization is needed, due
# to this definition. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dpois.max=function(x,lambda,maxx,log=FALSE)
{
 result=standard=dpois(x,lambda=lambda)
 
 below=which(x<maxx)
 above=which(x>maxx)
 exact=which(x==maxx)
 
 if(length(below)>0) result[below]=standard[below]
 if(length(above)>0) result[above]=0
 if(length(exact)>0) result[exact]=1-sum(dpois(0:(maxx-1),lambda=lambda))
 
 if(log) return(log(result))
 else return(result)
}

# <function>
# <name>
# dpois.trunc
# </name>
# <description>
# A zero-truncated Poisson distribution. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dpois.trunc=function(x,lambda,log=FALSE)
{
 result=dpois(x,lambda=lambda)/(1-exp(-lambda))
 zero=x==0
 result[zero]=0
   
 if(log) return(log(result))
 else return(result)
}
# </source>
# </function>
# 

# <function>
# <name>
# dpois.maxtrunc
# </name>
# <description>
# A zero-truncated Poisson distribution with a ceiling (combining dpois.max and dpois.trunc). 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
dpois.maxtrunc=function(x,lambda,maxx,log=FALSE)
{
 result=dpois.max(x,lambda=lambda,maxx=maxx)/(1-exp(-lambda))
 zero=x==0
 result[zero]=0
   
 if(log) return(log(result))
 else return(result)
}
# </source>
# </function>

# 
# 
# <function>
# <name>
# rpois.max
# </name>
# <description>
#  Random draws on dpois.max

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
rpois.max=function(N,lambda,maxx)
{
 result=rpois(N,lambda=lambda)
 result[result>maxx]=maxx
 
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# rpois.trunc
# </name>
# <description>
# Random draws on dpois.trunc. This is taken unchanged from an answer Peter Dalgaard posted to a list serve in 2005. I checked
# by comparing to dpois.trunc and it was spot on. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
rpois.trunc=function(N,lambda)
{
 return(qpois(runif(N, dpois(0, lambda), 1), lambda))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# asymptote.exp
# </name>
# <description>
# A 3-parameter function which asymptotes as x->infinity. The 3rd param must be >=0 and x>=0. The asymptote is a, the intercept a-b.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
asymptote.exp=function(x,param,...)
{
 a=param[1]
 b=param[2]
 k=param[3]

 if(k<0) return(rep(NA,length(x)))
 
 y=a-b*exp(-k*x)
 y[x<0]=NA
 
 return(y)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# graph.mvnorm
# </name>
# <description>
# Graphs contours for an mvnorm, with parameters submitted as a
# vector, as described above, for a single 2D Gaussian.
# The probability has to be calculated on a grid so contours can be drawn.
# The argument exclude allows parts to be set to zero.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
graph.mvnorm=function(param,x,y,div=10,add=FALSE,clr="gray",lw=.5,pw=2,exclude=NULL,returnit=FALSE)
{
 arranged=composeParam.GaussianMap(drop(as.matrix(param)),N=1)

 pts=full.xygrid(x,y)
 probdensity=dmvnorm(pts,mean=arranged$center,sigma=arranged$sigma[[1]])
 if(!is.null(exclude)) probdensity[exclude]=0
 
 breaks=seq(0,max(probdensity),len=div)

 contour.quaddata(probdensity,x=x,y=y,breaks=breaks,add=add,clr=clr,lwidth=lw,w=pw)
 if(returnit) return(probdensity)
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# pospower
# </name>
# <description>
# Raise to any power, but with negative numbers converted to positive first, then reverted afterward. It thus follows what is normal behavior when
# the exponent were a negative integer, but works also for even integers or any real exponent. For example, pospower(-4,0.5)=-2.
# </description>
# <arguments>
# </arguments>
# <sample>
# pospower(-4,0.5)
# </sample>
# <source>
pospower=function(x,expon)
{
 result=sign(x)*(abs(x)^(expon))
 return(result)
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# linear.mortmodel
# </name>
# <description>
# A model for mortality as a function of one or more predictors, with the time interval for each individual incorporated (as a last predictor).
# The log(mortality parameter) is modeled as a linear function of x[,-nopred].  The return value is a survival probability. Nothing prevents the output from
# being outside (0,1); that must be handled in the likelihood function.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
linear.mortmodel=function(x,param)
{
 nopred=dim(x)[2]
 predictor=x[,-nopred]
 interval=x[,nopred]
 
 logpred=linear.model(x=predictor,param=param)
 pred=(-1)*exp(logpred)
 # browser()
 
 survprob=exp(pred*interval)
 return(survprob)
}
# </source>
# </function>
# 
# 
# <function>
# <name>
# discrete.mortmodel
# </name>
# <description>
# A model for mortality as a function of a single discrete predictor, with the time interval for each individual incorporated (as a second predictor).
# The predictor must be a factor, so the total number of levels is known. The log(mortality parameter) is modeled as a different value for each distinct predictor. The number of parameters must exceed the maximum value of the predictor. 
# The return value is a survival probability. Nothing prevents the output from being outside (0,1); that must be handled in the likelihood function.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
discrete.mortmodel=function(x,param)
{
 predictor=x[,1]
 interval=x[,2]

 allpred=levels(predictor)
 # browser()
 
 logpred=numeric()
 noparam=length(allpred)
 for(i in 1:noparam) 
  {
   found=predictor==allpred[i]
   logpred[found]=param[i]
  }
 # browser()
  
 pred=(-1)*exp(logpred)
 survprob=exp(pred*interval)
 return(survprob)
}
# </source>
# </function>
# 
#
# <function>
# <name>
# discrete.model
# </name>
# <description>
# A model for a numeric response to a single discrete predictor. The predictor must be a factor, so the total number of levels is known. The response is modeled as a different value for each distinct predictor. The number of parameters must exceed the maximum value of the predictor. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
discrete.model=function(x,param)
{
 if(!is.null(dim(x))) x=x[,1]
 allpred=levels(x)
 
 y=numeric()
 noparam=length(allpred)
 for(i in 1:noparam) 
  {
   found=x==allpred[i]
   y[found]=param[i]
  }
 # browser()
  
 return(y)
}
# </source>
# </function>
# 

