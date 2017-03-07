# <function>
# <name>
# model.littleR.Gibbs
# </name>
# <description>
# The main function for fitting the probability distribution of population growth rates. Accepts any two full census R Analytical Tables.
# Five different functional forms to the distribution can be fitted, as chosen with the argument modeltype:
# <ul>
# <li> Gaussian [modeltype="norm", with the quotes]
# <li> Asymmetric Gaussian (a different standard deviation on left and right of the mode) [modeltype="asymnorm", with the quotes]
# <li> Laplace (exponential distribution, with mirror image for negative values) [modeltype="symexp", with the quotes]
# <li> Asymmetric Laplace (different rate constant for left and right of the center) [modeltype="asymexp", with the quotes]
# <li> Asymmetric power distribution (different rate constant for left and right of the center) [modeltype="asympower", with the quotes]
# </ul>
# A Gibbs sampler is used to fit the parameter, with a hierarchical component for the distribution of species' mortality rates (mu) and
# species' rates of population change (r). 
# and be sure to set mindbh. Other parameters can be left at defaults. 
# Added the bad.modelparam option to accomodate dasympower Aug 2011. Now this has to be included for asymexp; before, the
# check for negative SD parameters was hard-coded. 

# Optionally, a table demog can be created separately and submitted. It must have columns N1, N2, S, time.
# </description>
# <arguments>
# <ul>
# <li> cns1 and cns2: the two census R Analytical Tables, with earlier census first
# <li> mindbh: minimum dbh to be included; all trees smaller than mindbh are excluded
# <li> demog: optional, must match exactly the table created within the function
# <li> abundrange: the default includes every species, but this can be set to a minimum and maximum abundance (first census); species with abundances outside the range are excluded
# <li> start.param: parameter values at the outset, 1) mean of log(mortality) rate, 2) SD of log(mortality), 3) center of distribution of little r, 4) rate (or SD) of the distribution of little r; if an asymmetric model is chosen, the latter is the initial value for both left and right rate 
# <li> modeltype, as listed above
# <li> bad.modelparam: name of a function which checks the model parameters for bad values; for modeltype asymexp, must be bad.asymexp.param, for modeltype asympower, must be bad.asympower.param
# <li> steps: number of steps to run the Gibbs sampler
# <li> burn: number of steps of sampler to exclude as burn-in
# <li> showstep: print hyperparameters and likelihood to the screen every showstep steps
# <li> debug: set to TRUE to call browser within the function
# </ul>
# </arguments>
# <sample>
# lambir.modelR=model.littleR.Gibbs(cns1=lambir.full3,cns2=lambir.full4,mindbh=1,bad.modelparam=bad.asymexp.param))
# palanan.modelR=model.littleR.Gibbs(cns1=palanan.full3,palanan.full4,mindbh=1,bad.modelparam=bad.asymexp.param)
# For graphic output, just pass the result to graph.abundmodel. There are many options, but the defaults will show the key results. 
# graph.abundmodel(fit=lambir.modelR)
# Alternate distributions for little r:
# power67=model.littleR.Gibbs(cns1=bci.full6,cns2=bci.full7,modeltype='asympower',mindbh=10,start.param=c(-3,.8,.01,-.5),  bad.modelparam=bad.asympower.param,showstep=25)
# gauss67=model.littleR.Gibbs(cns1=bci.full6,cns2=bci.full7,modeltype='asymnorm',mindbh=10,start.param=c(-3,.8,.01,100),  bad.modelparam=bad.asymexp.param,showstep=25)
# </sample>
# <source>
model.littleR.Gibbs=function(cns1,cns2,mindbh,demog=NULL,sptable,abundrange=c(1,1e6),start.param=c(-3,.8,.01,-.5),modeltype='asympower',
                             excludespp=NULL,useIDlevel=TRUE,bad.modelparam=bad.asympower.param,steps=10000,burn=1000,showstep=500,debug=FALSE)
{
 cat('start at ', date(), '\n')
 on.exit(cat('end at ', date(), '\n'))
 
 if(is.null(demog))
	{
	 exc=cns1$status=='M' | cns2$status=='M'
	 cns1=subset(cns1,!exc)
	 cns2=subset(cns2,!exc)
	 
	 allspp=sort(unique(c(cns1$sp,cns2$sp)))
	 demog=data.frame(matrix(0,nrow=length(allspp),ncol=6))
	 rownames(demog)=allspp
	 colnames(demog)=c('N1','N2','S','time','date1','date2')
	 
	 N1=table(subset(cns1,status=='A' & dbh>=mindbh)$sp)
	 N2=table(subset(cns2,status=='A' & dbh>=mindbh)$sp)
	 S=table(subset(cns2,status=='A' & dbh>=mindbh & cns1$status=='A' & cns1$dbh>=mindbh)$sp)
	 meandate1=tapply(cns1$date,cns1$sp,mean,na.rm=TRUE)
	 meandate2=tapply(cns2$date,cns2$sp,mean,na.rm=TRUE)
	 
	 demog[names(N1),]$N1=N1
	 demog[names(N2),]$N2=N2
	 demog[names(S),]$S=S
	 demog[names(meandate1),]$date1=meandate1
	 demog[names(meandate2),]$date2=meandate2
	  
	 Nch=assemble.demography(pop.change(cns1,cns2,split1=cns1$sp,mindbh=mindbh,type='abund'),type='a')
	 demog[rownames(Nch),]$time=Nch$interval
	 if(debug) browser()
	 
	 if(useIDlevel) exclude1=subset(sptable,tolower(IDlevel)!='species' & tolower(IDlevel)!='genus' & tolower(IDlevel)!='family' & tolower(IDlevel)!='subspeci' & tolower(IDlevel)!='none')$sp
	 else exclude1=c('')
	 
	 # Following should not be used if IDlevel is set correctly, because unidentified species have IDlevel=multiple
	 # if(is.null(excludespp)) exclude2=rownames(demog)[unidentified.species(rownames(demog))]
	 # else exclude2=rownames(demog)[unidentified.species(rownames(demog),exactstr=excludespp)]
	 
	 # This allows user to eliminate any other species
	 if(is.null(excludespp)) exclude2=''
	 else exclude2=excludespp
	 
	 exclude=(rownames(demog) %in% unique(c(exclude1,exclude2)))
	 
	 demog=subset(demog,!exclude & N1>=abundrange[1] & N1<abundrange[2] & !is.na(time))
    }

 demog$mortrate=(log(demog$N1)-log(demog$S))/demog$time
 demog$little.r=(log(demog$N2)-log(demog$N1))/demog$time
 cat('Finished creating table of demography\n')
 
 # Hyperparameters are logMu, LogSD, hyperR, sdRUpper, sdRLower
 hyper=matrix(nrow=steps,ncol=5)
 hyper[1,1]=start.param[1]
 hyper[1,2]=start.param[2]
 hyper[1,3]=start.param[3]
 hyper[1,4]=hyper[1,5]=start.param[4]
 hscale=numeric()
 hscale[1]=abs(start.param[1])
 hscale[2]=abs(start.param[2])
 hscale[3]=abs(start.param[3])
 hscale[4]=hscale[5]=abs(start.param[4])
 if(debug) browser()
 
 nospp=dim(demog)[1]
 sp.m=sp.r=matrix(nrow=steps,ncol=nospp)
 colnames(sp.m)=colnames(sp.r)=rownames(data)
 mscale=rscale=numeric(nospp)
 
 sp.m[1,]=demog$mortrate
 nomort=which(demog$S==0 | demog$S==demog$N1)
 sp.m[1,nomort]=.01
 mscale=sp.m[1,]
 
 sp.r[1,]=demog$little.r
 nor=which(demog$N1==demog$N2 | demog$N2==0 | demog$N1==0)
 sp.r[1,nor]=0.01
 rscale=abs(sp.r[1,])

 llike=numeric() 
 i=1
 llike[i]=full.abundmodel.llike(data=demog,param=hyper[i,],spmort=sp.m[i,],spR=sp.r[i,],type=modeltype,debug=debug,badparam=bad.modelparam)

 if(debug) browser()
 
 for(i in 2:steps)
  {
   for(j in 1:2)
    {
     OneHyperSet=arrangeParam.Gibbs(i,j,allparam=hyper)

     metropResult=
       metrop1step(func=hyper.mortGibbs,start.param=OneHyperSet[j],scale.param=hscale[j],adjust=1.02,target=0.25,
                   spmean=sp.m[i-1,],MuSD=OneHyperSet[1:2],whichtest=j)
     hyper[i,j]=metropResult[1]
     hscale[1]=metropResult[2]
    }

   if(debug) browser()
  
   #  cat(hyper[i-1,],'\n')
    
   ### The species mortality parameters; mortality parameter also depends on abundance change...
   for(j in 1:nospp)
    {
     # if(j==100) browser()
     nextmean=metrop1step(func=spmean.mort.abundGibbs,start.param=sp.m[i-1,j],scale.param=mscale[j],adjust=1.02,target=0.25,
                          spmean.r=sp.r[i-1,j],N1=demog$N1[j],S=demog$S[j],time=demog$time[j],
                          MuSD=hyper[i,1:2],Rsd=hyper[i-1,3:5],type=modeltype)
     sp.m[i,j]=nextmean[1]
     mscale[j]=nextmean[2]
    }
   if(debug) browser()

   # if(i%%100==0) browser()
   #### The hyperparameters for abundance. The final (hyper[5]) is only used in asymmetric model #####
   whichupdate=3:4
   if(modeltype=='asymexp' | modeltype=='asympower' | modeltype=='asymnorm') whichupdate=3:5
   else hyper[i,5]=hyper[i-1,5]
   
   for(j in whichupdate)
    {
     OneHyperSet=arrangeParam.Gibbs(i,j,allparam=hyper)
     # if(i>50 & j==3) browser()
     
     ## Messy -- function hyper.abundGibbs takes only the 3 abund hyper parameters, not all 5, hence whichtest=j-2
     metropResult=
       metrop1step(func=hyper.abundGibbs,start.param=OneHyperSet[j],scale.param=hscale[j],adjust=1.02,target=0.25,
                   spmean=sp.r[i-1,],Rsd=OneHyperSet[3:5],whichtest=j-2,type=modeltype,badparam=bad.modelparam)

     hyper[i,j]=metropResult[1]
     hscale[j]=metropResult[2]
    }
   if(debug) browser()
      
   ### The species little R parameters
   for(j in 1:nospp)
    {
     nextmean=metrop1step(func=prob.N1,start.param=sp.r[i-1,j],scale.param=rscale[j],adjust=1.02,target=0.25,type=modeltype,
                          N1=demog$N1[j],N2=demog$N2[j],time=demog$time[j],surv.ann=sp.m[i,j],Rsd=hyper[i,3:5])
     sp.r[i,j]=nextmean[1]
     rscale[j]=nextmean[2]
    }
  if(debug) browser()

  # if(i%%100==0) browser()
   #  llike[i]=full.abundmodel.llike(data=demog,param=hyper[i,],spmort=sp.m[i,],spR=sp.r[i,],type=modeltype,debug=TRUE)
  # else
     llike[i]=full.abundmodel.llike(data=demog,param=hyper[i,],spmort=sp.m[i,],spR=sp.r[i,],type=modeltype,debug=FALSE)
  
   if(i%%showstep==2) 
    {
     # browser()
     cat("step ", i, date(), ": ",round(hyper[i,],5),round(llike[i],2),"\n")
    }
  }
  
 whichuse=(burn+1):steps
 meantheta=apply(sp.m[whichuse,],2,mean)
 uppertheta=apply(sp.m[whichuse,],2,quantile,prob=.975)
 lowertheta=apply(sp.m[whichuse,],2,quantile,prob=.025)
 meanR=apply(sp.r[whichuse,],2,mean)
 upperR=apply(sp.r[whichuse,],2,quantile,prob=.975)
 lowerR=apply(sp.r[whichuse,],2,quantile,prob=.025)
  
 colnames(hyper)=c('hyperMu','hyperSD','hyperR','hyperSDup','hyperSDlow')
 return(list(hyper=hyper,fulltheta=sp.m,fullR=sp.r,llike=llike,keep=whichuse,
             theta=colMeans(sp.m[whichuse,]),R=colMeans(sp.r[whichuse,]),
             means=c(hyperMu=mean(hyper[whichuse,1]),hyperSD=mean(hyper[whichuse,2]),
                     hyperR=mean(hyper[whichuse,3]),hyperSDup=mean(hyper[whichuse,4]),hyperSDlow=mean(hyper[whichuse,5])),
             lower=c(hyperMu=quantile(hyper[whichuse,1],.025),hyperSD=quantile(hyper[whichuse,2],.025),
                     hyperR=quantile(hyper[whichuse,3],.025),hyperSDup=quantile(hyper[whichuse,4],.025),hyperSDlow=quantile(hyper[whichuse,5],.025)),
             upper=c(hyperMu=quantile(hyper[whichuse,1],.975),hyperSD=quantile(hyper[whichuse,2],.975),
                     hyperR=quantile(hyper[whichuse,3],.975),hyperSDup=quantile(hyper[whichuse,4],.975),hyperSDlow=quantile(hyper[whichuse,5],.975)),
             demog=data.frame(demog,fitmort=meantheta,lowermean=lowertheta,uppermean=uppertheta,fitr=meanR,lowermeanR=lowerR,uppermeanR=upperR)))
}
# </source>
# </function>
#
#
# <function>
# <name>
# full.abundmodel.llike
# </name>
# <description>
# With the table of abundances, hyper-parameter estimates, and estimated mortality rate and population growth for each species, calculates full model likelihood.
# Note use of spmean.mort.abundGibbs, not sppmean.mort.Gibbs; in the latter, the one originally used in mortality model, the likelihood of observing a 
# mortality parameter does not depend on the population growth. Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
full.abundmodel.llike=function(data,param,spmort,spR,type,debug=FALSE,badparam=NULL)
{
 nospp=length(spmort)
 mort.llike=abund.llike=numeric()
 if(debug) browser()
 if(!is.null(badparam)) if(badparam(param[3:5])) return(-Inf)
 
 hypermort.llike=hyper.mortGibbs(test=param[1],MuSD=param[1:2],spmean=spmort,whichtest=1)
 hyperabund.llike=hyper.abundGibbs(test=param[3],Rsd=param[3:5],spmean=spR,whichtest=1,type=type,badparam=badparam)

 for(j in 1:nospp)
  {
   # if(j==558) browser()
   # cat(j, '\n')
   mort.llike[j]=
     spmean.mort.abundGibbs(spmean.mu=spmort[j],spmean.r=spR[j],N1=data$N1[j],S=data$S[j],MuSD=param[1:2],time=data$time[j],Rsd=param[3:5],type=type)
   abund.llike[j]=
     prob.N1(spR[j],Rsd=param[3:5],N1=data$N1[j],N2=data$N2[j],surv.ann=spmort[j],time=data$time[j],type=type) 
  }

 if(debug) browser()
 return(hypermort.llike+hyperabund.llike+sum(mort.llike)+sum(abund.llike))
}
# </source>
# </function>
#
#
# <function>
# <name>
# prob.N1
# </name>
# <description>
# Calculates the probability of observing N2 given N1, assuming a community-wide
# distribution of little.r, log(N2/N1). It uses the normal approximation to the 
# binomial-poisson model (see dpopchange and testdpopchange in abundsim.r) as the
# error distribution around N2. Works with one species at a time, so all submitted values except lambda.ann are
# scalars. Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
prob.N1=function(spmean.ann,Rsd,N1,N2,surv.ann,time,type="norm")
 {
  if(N1==0) browser()

  spLambda=exp(spmean.ann*time)
  spmean=log(spLambda)
  surv=exp(-surv.ann*time)
  if(spLambda<=surv^2) return(-Inf)
    
  #hyperLambda.ann=exp(Rsd[1])
  #hyperLambda=hyperLambda.ann^time

  sp.llike=dnorm(N2,mean=spLambda*N1,sd=sqrt(N1*(spLambda-surv^2)),log=TRUE)
  
  if(type=="norm") hyper.llike=dnorm(spmean.ann,mean=Rsd[1],sd=1/Rsd[2],log=TRUE)
  else if(type=="asymnorm") hyper.llike=dasymnorm(spmean.ann,center=Rsd[1],sigma1=1/Rsd[2],sigma2=1/Rsd[3],log=TRUE)
  else if(type=="symexp") hyper.llike=dsymexp(spmean.ann,center=Rsd[1],rate=1/Rsd[2],log=TRUE)
  else if(type=="asymexp") hyper.llike=dasymexp(spmean.ann,center=Rsd[1],rate1=1/Rsd[2],rate2=1/Rsd[3],log=TRUE)
  else if(type=="asympower") hyper.llike=dasympower(spmean.ann,center=Rsd[1],rate1=1/Rsd[2],rate2=1/Rsd[3],log=TRUE)
  
  result=sp.llike+hyper.llike
  return(result)
 }
# </source>
# </function>
#
#
# <function>
# <name>
# spmean.mort.abundGibbs
# </name>
# <description>
# Likelihood function for a species mean (a scalar, one species at a time), given logMu and logSD and the data, N and S 
# (just one species here, so all parameters are scalars). In the abundance model, the mortality
# parameter is involved in the likelihood for population change, and it must thus depend on the
# the fitted little r. See spmean.mort.Gibbs in mortality.fit.CTFS.r for the original version, from mortality model,
# in which there is no dependence on little r. Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
spmean.mort.abundGibbs=function(spmean.mu,spmean.r,N1,S,time,MuSD,Rsd,type)
{
 if(is.na(spmean.mu)) browser()
 if(spmean.mu<=0) return(-Inf)
 
 theta=exp(-spmean.mu*time)
  
 spLambda=exp(spmean.r*time)
 spmean=log(spLambda)
 surv=exp(-spmean.mu*time)
 if(spLambda<=surv^2) return(-Inf)
 
 llike=dlnorm(x=spmean.mu,meanlog=MuSD[1],sdlog=MuSD[2],log=TRUE)+dbinom(x=S,size=N1,prob=theta,log=TRUE)

 if(type=="norm") abund.llike=dnorm(spmean.r,mean=Rsd[1],sd=1/Rsd[2],log=TRUE)
 else if(type=="asymnorm") abund.llike=dasymnorm(spmean.r,center=Rsd[1],sigma1=1/Rsd[2],sigma2=1/Rsd[3],log=TRUE)
 else if(type=="symexp") abund.llike=dsymexp(spmean.r,center=Rsd[1],rate=1/Rsd[2],log=TRUE)
 else if(type=="asymexp") abund.llike=dasymexp(spmean.r,center=Rsd[1],rate1=1/Rsd[2],rate2=1/Rsd[3],log=TRUE)
 else if(type=="asympower") abund.llike=dasympower(spmean.r,center=Rsd[1],rate1=1/Rsd[2],rate2=1/Rsd[3],log=TRUE)
 
 return(llike+abund.llike)
}
# </source>
# </function>
#
#
# <function>
# <name>
# hyper.abundGibbs
# </name>
# <description>
# Likelihood function for hyperparameters of abundance model, given the species values of little.r (latter a vector). Simply calculates
# the pdf of whatever modelfunc is requested. For symmetric models, norm and symexp, the third parameter (second hyperSD) is not used.
# Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
hyper.abundGibbs=function(test,Rsd,spmean,whichtest,type,badparam=NULL,returneach=FALSE)
{
 param=arrangeParam.llike(testparam=test,allparam=Rsd,whichtest=whichtest)
 if(!is.null(badparam)) if(badparam(param)) return(-Inf)
 
 hyperR=param[1]
 hyperSDUp=1/param[2]
 hyperSDLow=1/param[3]
   
 if(type=='norm') llike=dnorm(spmean,mean=hyperR,sd=hyperSDUp,log=TRUE)
 else if(type=='asymnorm') llike=dasymnorm(spmean,center=hyperR,sigma1=hyperSDUp,sigma2=hyperSDLow,log=TRUE)
 else if(type=='symexp') llike=dsymexp(spmean,center=hyperR,rate=hyperSDUp,log=TRUE)
 else if(type=='asymexp') llike=dasymexp(spmean,center=hyperR,rate1=hyperSDUp,rate2=hyperSDLow,log=TRUE)
 else if(type=='asympower') llike=dasympower(spmean,center=hyperR,rate1=hyperSDUp,rate2=hyperSDLow,log=TRUE)

 if(returneach) return(llike)
 return(sum(llike))
}
# </source>
# </function>
#
#
# <function>
# <name>
# hyper.mortGibbs
# </name>
# <description>
# Likelihood function for logMu and logSD, given the species means (latter a vector). Simply calculates
# log-normal probability of observing the species means given logMu and logSD. This is modernized to use arrangeParam.llike,
# and replaces the older mu.mortGibbs and sd.mortGibbs.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
hyper.mortGibbs=function(test,MuSD,spmean,whichtest)
{
 param=arrangeParam.llike(testparam=test,allparam=MuSD,whichtest=whichtest)
 logMu=param[1]
 logSD=param[2]
 if(logSD<=0) return(-Inf)
 
 llike=dlnorm(spmean,meanlog=logMu,sdlog=logSD,log=TRUE)
 return(sum(llike))
}
# </source>
# </function>
#
#
# <function>
# <name>
# bad.asympower.param
# </name>
# <description>
# The 3 parameters submitted to hyper.abundGibbs have to be checked, in case dasympower is used. The second and third, the
# two rate parameters, have to be < (-1). Since the parameters are inverted, they must be in (-1,0). 
# Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
bad.asympower.param=function(hyperparam)
{
 if(hyperparam[2]>=0) return(TRUE)
 if(hyperparam[3]>=0) return(TRUE)
 if(hyperparam[2]<=(-1)) return(TRUE)
 if(hyperparam[3]<=(-1)) return(TRUE)
 return(FALSE)
}
# </source>
# </function>
#
#
# <function>
# <name>
# bad.asymexp.param
# </name>
# <description>
# For either the Gaussian, or asymexp, the SD parameters must be > 0. Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
bad.asymexp.param=function(hyperparam)
{
 if(hyperparam[2]<=0) return(TRUE)
 if(hyperparam[3]<=0) return(TRUE)
 return(FALSE)
}
# </source>
# </function>
#
#
# <function>
# <name>
# fitSeveralAbundModel
# </name>
# <description>
# Run model.littleR.Gibbs for a series of census databases, for every successive pair, then the first to the last. Then repeat for 10 times the initial
# mindbh. All arguments except allcns are the same as those in model.littleR.Gibbs; allcns is a list of two more more census dataframes. 
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
fitSeveralAbundModel=function(allcns=list(bci.full1,bci.full2,bci.full3),sptable=bci.spptable,mindbh,excludespp=NULL,useIDlevel=TRUE,abundrange=c(1,1e6),start=c(-3,.8,.01,-.5),
                              modeltype='asympower',bad.modelparam=bad.asympower.param,steps=10000,burn=1000,show=250,debug=FALSE)
{
 result=result10=list()
 nocns=length(allcns)
 
 for(i in 1:(nocns-1))
  {
   result[[i]]=model.littleR.Gibbs(cns1=allcns[[i]],cns2=allcns[[i+1]],sptable=sptable,modeltype=modeltype,mindbh=mindbh,excludespp=excludespp,useIDlevel=useIDlevel,
                                   start.param=start,bad.modelparam=bad.modelparam,steps=steps,burn=burn,showstep=show,debug=debug)
                          
   result10[[i]]=model.littleR.Gibbs(cns1=allcns[[i]],cns2=allcns[[i+1]],sptable=sptable,modeltype=modeltype,mindbh=10*mindbh,excludespp=excludespp,useIDlevel=useIDlevel,
                                     start.param=start,bad.modelparam=bad.modelparam,steps=steps,burn=burn,showstep=show)
  }
  
 if(nocns>2)
  {
   result[[nocns]]=model.littleR.Gibbs(cns1=allcns[[1]],cns2=allcns[[nocns]],sptable=sptable,modeltype=modeltype,mindbh=mindbh,excludespp=excludespp,useIDlevel=useIDlevel,
                                       start.param=start,bad.modelparam=bad.modelparam,steps=steps,burn=burn,showstep=show)
   result10[[nocns]]=model.littleR.Gibbs(cns1=allcns[[1]],cns2=allcns[[nocns]],sptable=sptable,modeltype=modeltype,mindbh=10*mindbh,excludespp=excludespp,useIDlevel=useIDlevel,
                                         start.param=start,bad.modelparam=bad.modelparam,steps=steps,burn=burn,showstep=show)
  }
  
 fullnames=pst('dbh',round(c(mindbh,10*mindbh),0))
 cnsvect=1:nocns
 interval=paste(cnsvect[1:(nocns-1)],cnsvect[2:nocns],sep='.')
 if(nocns>2) interval=c(interval,paste(1,nocns,sep='.'))
 subnames=pst('census',interval)
 
 names(result)=names(result10)=subnames
 final=list(result,result10)
 names(final)=fullnames
 
 return(final) 
}
# </source>
# </function>
#
#
# <function>
# <name>
# graph.abundmodel
# </name>
# <description>
# Output histograms of little.r across species, observed and fitted, using the result of
# model.littleR.Gibbs. The histogram of black points is all species, blue points only those starting with N >= minabund. If the argument mortcorr=TRUE,
# a graph of mortality rate vs. population change for every species is also produced. Otherwise, a table of the species with biggest increases and biggest
# decreases in abundance is printed to the screen. 
# </description>
# <arguments>
# <ul>
# <li> fit: result of model.littleR.Gibbs
# <li> datafile: optional name of file where the fitted result is saved
# <li> div: width of bins for histogram of observed rate of population change
# <li> tinydiv: width of bins used to draw the fitted distribution
# <li> modeltype: form of probability distribution, matching what was used when fit was created by model.littleR.Gibbs
# <li> xrange, yrange: range of graph's x-axis and and y-axis
# <li> minabund: minimum abundance of species to be used in histogram of observed rates of population change
# <li> conf: number of alternate fits to graph, as indication of confidence; if conf=NULL, no confidence lines are added
# <li> returnextreme: whether to print a list of the fastest increases and decreases in abundance to the screen
# <li> xname, yname: axis names
# <li> graphit: if set to false, 
# <li> ltype, lwidth, modelclr: type, width, color of the line showing fitted distribution
# <li> bartype: if TRUE, histogram is bar graph
# <li> addpts: if TRUE, histogram is a point graph
# <li> makeleg: whether to add legend
# <li> add: just like all R graphs, whether to add points or line to an existing graph
# <li> ax: if FALSE, the axes are not added
# <li> mortcorr: whether to graph the correlation between mortality and population change across species
# <li> debug: if TRUE, call browser to debug
# </ul>
# </arguments>
# <sample>
# </sample>
# <source>
graph.abundmodel=function(fit,datafile=NULL,div=.01,tinydiv=0.001,modeltype='asympower',xrange=NULL,yrange=NULL,minabund=50,conf=25,returnextreme=TRUE,
						  xname='Rate of population change',yname='Frequency',ltype='solid',lwidth=2,modelclr='green',bartype=FALSE,addpts=TRUE,
                          makeleg=TRUE,add=FALSE,newgraph=FALSE,ax=TRUE,mortcorr=FALSE,debug=FALSE)
{
 if(!is.null(datafile)) 
  {
   pos=attach_if_needed(datafile)
   fit=gsp(pos)
  }
  
 obsR=fit$demog$little.r
 inc=is.finite(obsR)&!is.na(obsR)
 b=seq(min(obsR[inc]),max(obsR[inc])+div,by=div)
 # b=seq(min(c(xrange,obsR[inc])),max(c(xrange,obsR[inc]))+div,by=div)
 xhist=b[-length(b)]+0.5*div

 divR=cut(obsR[inc],breaks=b,right=FALSE)
 histR=table(divR)
 histR=histR/sum(histR) 
 if(debug) browser()

 inc=is.finite(obsR)&!is.na(obsR)&fit$demog$N1>=minabund
 divR=cut(obsR[inc],breaks=b,right=FALSE)
 histRab=table(divR)
 histRab=histRab/sum(histRab) 
  
 x=find.xaxis.hist(fit,tinydiv,xrange)
 y=abundmodel.fit(x,fit,modeltype=modeltype)
 y=div*y/sum(y)/tinydiv
 if(is.null(yrange)) yrange=c(0,max(c(y,histR,histRab)))

 if(newgraph) x11()
 if(mortcorr) par(mfcol=c(2,1))
 
 if(addpts)
  {
   if(!bartype)
    {
     if(!add) plot(xhist,histR,xlim=xrange,ylim=yrange,main='Histogram of rate of population change (r)',axes=ax,xlab=xname,ylab=yname)
     else points(xhist,histR)
     points(xhist,histRab,pch=16,col='blue')
    }
   else
    {
     if(!add) plot(xhist,histR,xlim=xrange,ylim=yrange,axes=ax,type='h',lwd=10,col='gray60',main='Histogram of rate of population change (r)',xlab=xname,ylab=yname)
     else plot(xhist,histR,xlim=xrange,axes=ax,type='h',add=TRUE)
    } 
  }
  
 if(!add & !addpts) plot(x,y,col=modelclr,lwd=lwidth,type='l',axes=ax,xlab=xname,ylab=yname,xlim=xrange,ylim=yrange)

 lines(x,y,col=modelclr,lwd=lwidth,lty=ltype)
 if(!add & makeleg)
  {
   if(is.null(conf)) legend('topleft',legend=c('all spp','abund spp','fitted'),pch=c(1,16,NA),lty=c(NA,NA,'solid'),col=c('black','blue',modelclr))
   else legend('topleft',legend=c('all spp','abund spp','fitted','confidence'),pch=c(1,16,NA,NA),lty=c(NA,NA,'solid','solid'),col=c('black','blue',modelclr,'gray70'))
  }
  
 if(!is.null(conf))
  {
    z=abundmodel.fit(x,fit,modeltype=modeltype,conf=conf)

    for(i in 1:conf) 
	    {
	     # browser()
	     resp=div*z[i,]/sum(z[i,])/tinydiv
	     lines(x,resp,col='gray70',lwd=0.85)
	    }
     # Following just redraws histogram and model to overlay confidence lines
	if(addpts)
	 {
	   if(!bartype)
	    {
	     if(debug) browser()
	     # if(!add) plot(xhist,histR,xlim=xrange,main='Histogram of rate of population change (r)',axes=ax)
	     points(xhist,histR)
	     points(xhist,histRab,pch=16,col='blue')
	    }
	   else
	    {
	     if(!add) hist(obsR,xlim=xrange,axes=ax,breaks=b,main='Histogram of rate of population change (r)')
	     else hist(obsR,xlim=xrange,axes=ax,breaks=b,add=TRUE)
	    } 
     }
    lines(x,y,col=modelclr,lwd=lwidth,lty=ltype)
  }

 ord=order(fit$demog$fitr)
 hipart=head(fit$demog[ord,],10)
 ord=order(-fit$demog$fitr)
 lowpart=head(fit$demog[ord,],10)

 if(!mortcorr) 
  {
   if(returnextreme) return(list(Fastest_increases=lowpart,Biggest_losses=hipart))
   else return(0)
  }

 # x11()
 logmort=log(fit$demog$fitmort)
 reg=lm(fit$demog$fitr~logmort)
 plot(fit$demog$fitmort,fit$demog$fitr,pch=16,main='Estimated pop. change vs. estimated mortality',log='x') 
 ord=order(logmort)
 lines(fit$demog$fitmort[ord],reg$fitted[ord])
 sig=summary(reg)$coef[2,4]<.05
 if(sig) ast='*'
 else ast=''
 text(x=min(fit$demog$fitmort),y=0.8*max(fit$demog$fitr),labels=paste('slope',ast,round(summary(reg)$coef[2,1],3)),pos=4)

 if(debug) browser()
 
 if(returnextreme) return(list(Fastest_increases=lowpart,Biggest_losses=hipart))
}
# </source>
# </function>
#
#
# <function>
# <name>
# find.xaxis.hist
# </name>
# <description>
# Given an abundance fit and x axis range and divisions, return a sequence of x values for drawing the histogram. Used as a subroutine inside graph.abundmodel.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
find.xaxis.hist=function(fit,tinydiv=.001,xrange)
{
  if(is.null(xrange)) xrange=range(fit$demog$fitr)
  xneg=seq(min(xrange),fit$means['hyperR'],by=tinydiv)
  xpos=seq(fit$means['hyperR'],max(xrange),by=tinydiv)
  return(c(xneg,xpos))
}
# </source>
# </function>
#
#
# <function>
# <name>
# abundmodel.fit
# </name>
# <description>
# Simply return the modeled histogram for any set of parameters. Used as a subroutine inside graph.abundmodel.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
abundmodel.fit=function(x,fit,modeltype,conf=NULL)
{
 fitcenter=fit$means['hyperR']
 fitSDlow=1/fit$means['hyperSDlow']
 fitSDhi=1/fit$means['hyperSDup']

 if(modeltype=='symexp') y=dsymexp(x,center=fitcenter,rate=fitSDhi)
 else if(modeltype=='asymnorm') y=dasymnorm(x,center=fitcenter,sigma1=fitSDhi,sigma2=fitSDlow)
 else if(modeltype=='asymexp') y=dasymexp(x,center=fitcenter,rate1=fitSDhi,rate2=fitSDlow)
 else if(modeltype=='asympower') y=dasympower(x,center=fitcenter,rate1=fitSDhi,rate2=fitSDlow)
 else if(modeltype=='norm') y=dnorm(x,mean=fitcenter,sd=fitSDhi)

 if(is.null(conf)) return(y)
 
 z=matrix(nrow=conf,ncol=length(x))
   
 keeppar=fit$hyper[fit$keep,]
 r=sample.int(length(fit$keep),conf)
 drawpar=keeppar[r,]
 
 if(modeltype=='symexp') for(i in 1:conf) z[i,]=dsymexp(x,center=drawpar[i,3],rate=1/drawpar[i,4])
 else if(modeltype=='asymexp') for(i in 1:conf) z[i,]=dasymexp(x,center=drawpar[i,3],rate1=1/drawpar[i,4],rate2=1/drawpar[i,5])
 else if(modeltype=='norm') for(i in 1:conf) z[i,]=dnorm(x,mean=drawpar[i,3],sd=1/drawpar[i,4])
 else if(modeltype=='asympower') for(i in 1:conf) z[i,]=dasympower(x,center=drawpar[i,3],rate1=1/drawpar[i,4],rate2=1/drawpar[i,5])
 else if(modeltype=='asymnorm') y=dasymnorm(x,center=drawpar[i,3],sigma1=1/drawpar[i,4],sigma2=1/drawpar[i,5])
    
 return(z)
}
# </source>
# </function>
#


