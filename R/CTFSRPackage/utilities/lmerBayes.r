# <function>
# <name>
# lmerBayes
# </name>
# <description>
# A Metropolis MCMC version of lmer. A single independent variable, y, can be fit against any number of predictors, x, 
# with one random effect. Like lmer, the model error can be binomial or Gaussian, 
# but there are two alternatives for the Gaussian (described below). Relative to lmer, the key advantage offered is that 
# y can be any function of the x. A second advantage is that the MCMC produces 
# posterior distributions on every parameter, so full confidence limits are available. The principal limitation relative to lmer is 
# that only one random effect is allowed. In addition, the Bayesian MCMC approach is quite a bit slower.
# <br><br>
# Data are submitted the way lm or lmer require, with one single table, one row per observation; the random effects 
# are in one column. The formula, however, is not submitted using the R-style 'squiggle' ~. Rather, the names of x, y, and 
# random columns, are given. The model describing y's function of the x's is passed, and must be provided by the user 
# (several are available within the CTFS R Package, though, in the Utilities topic). Examples below will serve to explain.
# <br><br>
# As in lmer, all parameters of the model follow a Gaussian hyperdistribution across the random effects. There is an 
# option to include a full covariance matrix as the hyperdistribution, otherwise, only the variances are fit (ie, the
# covariance matrix has only zeroes off-diagonal). There is also an option to use the conjugate inverse-gamma or inverse-wishart for the variances and covariances; otherwise, Metropolis steps are used.
# <br><br>
# A starting set of parameters for the model must be submitted. It can be a vector as long as the number of
# parameters required by the model, or it can be a full matrix, with one row of parameters for each of the random
# effects. The latter requires knowing in advance the names of all the random effects. 
# <br><br>
# There is a further complication included whose purpose is reducing memory demand in big models with many MCMC steps. option paramfile allows the full parameter matrix to be written into a text file every savestep steps, then erased from memory. 
# This is to reduce memory needs. The function summaryMCMC restores the parameters from the text file into an giant R array. 
# <br><br>
# The return value is a list with several components:
# <ul>
# <li> mu: A 2D array with the entire chain of model parameters (ie, fixed effects) from the Gibbs sampler
# <li> sigma: A 3D array with the entire chain of covariances from the Gibbs sampler; if includeCovar==FALSE, only the diagonal is non-zero
# <li> bestmu: Best estimate of the model parameters for the entire data (ie, fixed effect) 
# <li> bestsigma: Best estimate of the covariance (ie, group-level variance or error)
# <li> resid: The entire chain parameters for the model of residuals
# <li> bestresid: The best estimate of parameters for the model of residuals
# <li> CIresid: Credible intervals for the parameters for the model of residuals
# <li> best: The best estimates of model parameters for each random effect
# <li> lower: Lower credible intervals of model parameters for each random effect
# <li> upper: Uower credible intervals of model parameters for each random effect      
# <li> burn: The burn-in
# <li> llike: Full log-likelihood of the model at each step of the Gibbs' sampler
# <li> bestlike: The log-likelihood of the optimal parameter combination (means of the posterior distribution)
# <li> DIC: Deviance information criterion of the model
# <li> obs: The original y (dependent) variable, just as submitted
# <li> data: The original x (independent) variables, just as submitted
# <li> model: The model's predictions, as a list with one element per random effect
# <li> randlike: The log-likelihood of observations for each random effect given the optimal parameters (a vector, one per random effect)
# <li> keep: The steps of the Gibbs sampler after burn-in, as a vector of negative numbers 
# <li> start: The start parameters submitted
# <li> randeffects: The names of all the random effects
# <li> parnames: The names of the model parameters
# <li> fullparam: A 3D array with all parameters of the Gibbs sampler; one dimension if for all the random effects, with each random effect having a matrix of model parameters for every step of the Gibbs's sampler
# <\ul>
# <br><br>
# Further details are given in the description of all the arguments and the sample here, plus a tutorial on 'Mortality changes' <br>
# (http://ctfs.arnarb.harvard.edu/Public/CTFSRPackage/index.php/web/tutorials/MortalityChange/index.html) <br>
# offers a worked example.
# </description>
# <arguments>
# <ul>
# <li> data: The table of data, in lmer-style, including one column to be modeled (dependent variable, y), one or more predictors (independent variables, x), and one random effect, using any column names.
# <li> ycol: The name of the column holding the y variable, with quote marks; this variable must be numeric.
# <li> ycol: The name of one or more columns holding the x variables, with quote marks; these can be numeric or character variables.
# <li> randcol: The name of one column holding the random variable; must be a character variable.
# <li> start: Starting parameter values, either a vector with as many parameters as the model needs, or a matrix of such vectors, one per random effect
# <li> startSD: A single starting value for the residual standard deviation, only used with Gaussian and Negative Binomial error models.
# <li> startCov: Starting values of the diagonal of the covariance matrix; ignored if a full matrix of start parameters is submitted. Required even if covariance matrix is not fitted, because needed as starting hyperSD.
# <li> model: The function name holding the model describing y's relationship to all the x's, without quote marks. The first argument of the function must be named x, the second param, with additional arguments allowed. The model may accept as x either a vector or a matrix, the latter for a multiple regression. There can be any number of parameters, but the number must match the number given as start parameters. The return value must be a numeric vector with the same size as x. 
# <li> error: A character variable with 6 possible values, Binom, NegBinom, Pois, Gauss, GaussMultResid, or Flat, with quote marks. 
# <ul>
# <li> 'Binom' uses binomial error for residuals
# <li> 'NegBinom' uses negative binomial error for residuals; the SD is then the dispersion parameter (k) of the negative binomial
# <li> 'Poisson' uses Poisson error for residuals
# <li> 'Gauss' uses Gaussian error for residuals with constant standard deviation across groups
# <li> 'GaussMultResid' uses Gaussian error for residuals, with standard deviation a constant fraction of the model's prediction
#         (and thus only appropriate if predictions are strictly positive)
# <li> 'Flat' is a trivial model where the same likelihood is returned regardless of parameters or data. It is for testing how parameter search behaves in absence of data, as for describing an implied prior. 
# </ul>
# <li> includeCovar: TRUE or FALSE, whether to fit the full covariance matrix, vs. variances alone.
# <li> update: 'conjugate' or 'metropolis', whether to use inverse-gamma (or inverse-Wishart for full covariance) vs. metropolis steps for updating covariances.
# <li> badparam: The name of a function (unquoted) that tests a set of model parameters for validity; must return TRUE if parameters are valid, otherwise FALSE.
# <li> sdfunc: The name of a function (unquoted) that models the residual standard deviation as a function of the x's, just like the model function. The default uses the function named constant, meaning the standard deviation is the same for all values of x. Parameters for this function are estimated, just as parameters for the model function are.
# <li> badSDparam: The name of a function which tests for invalid parameters for sdfunc, returning TRUE or FALSE (analogous to badparam); a simple version is provided, called badSD, which rejects a single parameter if it is < 0. 
# <li> paramfile: The name of a file where the entire MCMC chain of parameter values is stored at regular intervals; when parameters are written to the file, they are erased from memory, thus removing the need for the entire chain of all parameters being stored at once while the model is running.
# <li> savestep: Parameters are appended to paramfile every savestep steps; must be < steps.
# <li> steps: The number of steps to run the Gibbs sampler.
# <li> showstep: Information is printed to the screen every showstep steps.
# <li> burnin: The number of steps to remove as burn-in before calculating posterior distributions; not that all parameters are saved and returned regardless.
# <li> debug: TRUE or FALSE, whether to pause and debug; for advanced users and developers.
# <li> ...: The typical R means for submitting additional parameters for various functions used in the model (model, sdfunc, badparam, badSDparam).
# </ul>
# </arguments>
# <sample>
# Assume two plot datasets from BCI are attached, bci.full6 and bci.full7. Subset to trees above 10 cm dbh and just 10 species for illustration (the model will run much faster). The fixed effect, species-level variation (or error), and the model parameters for each species are shown below. Check the names of the result to see what else lmerBayes returns.<br>
# gtable=growth.indiv(bci.full6,bci.full7,mindbh=100) <br>
# a_few_species=c('termam','tachve','pri2co','gustsu','cecrin','tet2pa','guatdu', 'vochfe','virose','maquco') <br>
# gtable=subset(gtable,!is.na(incgr) & sp %in% a_few_species) <br>
# mod=lmerBayes(data=gtable,ycol='incgr',xcol='dbh1',randcol='sp',start=c(1,0),startSD=1,startCov=1,model=linear.model,error='Gauss', includeCovar=FALSE,badSDparam=badSD,steps=1100,showstep=50,burnin=100) <br>
# mod$bestmu <br>
# diag(sqrt(mod$bestsigma)) <br>
# mod$best  <br>
# names(mod)
# </sample>
# <source>
lmerBayes=function(data,ycol,randcol,xcol,start,fixef=NULL,startSD,startCov,model=logistic.standard,error='Binom',includeCovar=TRUE,update='conjugate',badparam=NULL,
                   sdfunc=constant,badSDparam,paramfile=NULL,savestep=500,steps=1000,showstep=100,burnin=100,debug=FALSE,...)
{
 data=subset(data,!is.na(data[,ycol]))
 data=subset(data,!is.na(data[,randcol]))
 for(onex in xcol) data=subset(data,!is.na(data[,onex]))
 
 y=split.data(data[,c(randcol,ycol)],splitcol=randcol)
 ally=data[,ycol]
 x=split.data(data[,c(randcol,xcol)],splitcol=randcol)
 allx=data[,xcol]
 if(debug) browser()
 
 randeffects=names(y)
 norand=length(randeffects)

 if(!is.null(names(start))) parnames=names(start)
 else parnames=c('Inter',xcol)
 if(is.null(dim(start))) noparam=length(start)
 else noparam=dim(start)[2]

 if(is.null(dim(start))) start=matrix(start,nrow=norand,ncol=noparam,byrow=TRUE)
 else start=as.matrix(start)                       ## In case it's a data.frame
   
 if(is.null(paramfile)) param=array(dim=c(norand,steps,noparam),dimnames=list(randeffects,NULL,parnames))
 else param=array(dim=c(norand,savestep,noparam),dimnames=list(randeffects,NULL,parnames))
 param[,1,]=start
 scale=start
 scale[scale<=0]=1
 
 # To include fixed effects.
 if(!is.null(fixef))
  {
   nofix=length(fixef)
   fixparam=matrix(nrow=steps,ncol=nofix)
   fixparam[1,]=fixscale=fixef
   fixscale[fixscale<=0]=1
  }
 else fixparam=NULL
  
 # Residual standard deviation is required if error=Gauss, GaussMultResid, or NegBinom. It is calculated from sdfunc and sdpar. 
 # If error==Binom or Pois, resid is ignored.
 if(error=='Gauss' | error=='GaussMultResid' | error=='NegBinom')
    {
     noSDparam=length(startSD)
     resid=matrix(ncol=noSDparam,nrow=steps)
     resid[1,]=startSD
     scale.resid=startSD
     scale.resid[scale.resid<=0]=1
	  }
 else if(error=='Binom' | error=='Pois' | error=='Flat') resid=NULL 
  
 hypermean=matrix(nrow=steps,ncol=noparam)
 colnames(hypermean)=parnames
 hypermean[1,]=colMeans(start)
 hyper.scale=hypermean[1,]
 hyper.scale[hyper.scale<=0]=1
 if(debug) browser()
 
 covar=array(dim=c(noparam,steps,noparam),dimnames=list(parnames,NULL,parnames))
 if(includeCovar) covar[,1,]=cov(start)
 else covar[,1,]=0
 if(!includeCovar | length(which(diag(array(covar[,1,],dim=c(noparam,noparam)))==0)>0)) covar[,1,]=AssignDiag(covar[,1,],startCov)
 covar.scale=covar[,1,]
 covar.scale[covar.scale<=0]=startCov[1]

 sp.llike=matrix(nrow=norand,ncol=steps)
 rownames(sp.llike)=randeffects
 if(debug) browser()

 startfile=FALSE
 llike=numeric()
 fc=pc=1
 llike[fc]=
   full.likelihood.lmerBayes(one.covar=covar[,fc,],one.param=param[,pc,,drop=FALSE],one.hyper=hypermean[fc,],model=model,fixpar=fixparam[fc,],errormodel=error,
                             data=x,obs=y,sdmodel=sdfunc,sdpar=resid[fc,],bad=badparam,...)
 if(debug) browser()

 #### Two different counters, so parameter matrix can be saved and emptied every savestep steps. The counter pc is reset to 2 when saving happens.
 # The hyperparameters are never emptied, so the counter fc augments steadily. 
 pc=2
 
 for(fc in 2:steps)
  {
   ##### Update the parameters for every one of the random effects ####
	 for(k in 1:norand) 
	   {
	    for(j in 1:noparam)
		    {
		     testparam=arrangeParam.Gibbs(pc,j,param[k,,])
             if(!is.null(fixef)) testparam=c(testparam,fixparam[fc-1,])
             
		     metropResult=metrop1step(func=llike.model.lmer,start.param=testparam[j],scale.param=scale[k,j],allparam=testparam,whichtest=j,
		                              data=drp(x[[k]]),trueN=drp(y[[k]]),sdmodel=sdfunc,sdpar=resid[fc-1,],model=model,errormodel=error,
                                  mu=hypermean[fc-1,],covar=covar[,fc-1,],fixed=fixef,
		                              badparam=badparam,adjust=1.02,target=0.25,...)
		     param[k,pc,j]=metropResult[1]
		     scale[k,j]=metropResult[2]
		    }

	   }
   if(debug) browser()
   
   ##### Update any fixed effects using full data. Parameters for each random effect are needed for the model.
   if(!is.null(fixef))
     {
      for(j in 1:nofix)
          {
           testparam=arrangeParam.Gibbs(fc,j,fixparam)

		   # browser()
		   metropResult=
             metrop1step(func=llike.fixef.lmer,start.param=fixparam[fc-1,j],scale.param=fixscale[j],fixpar=testparam,one.param=param[,pc,,drop=FALSE],whichtest=j,
		                 data=x,obs=y,sdmodel=sdfunc,sdpar=resid[fc-1,],model=model,errormodel=error,bad=badparam,adjust=1.02,target=0.25,...)
		   fixparam[fc,j]=metropResult[1]
		   fixscale[j]=metropResult[2]
          }
     }
      
   ##### Update the hypermeans using posterior Gaussian ####
   ## Need to add the option of a transformation such as logit...
   # hypermean[fc,]=rmvnorm(1,mean=colMeans(logit(param[,pc,,drop=FALSE])),sigma=array(covar[,fc-1,],dim=c(noparam,noparam))/norand)
   hypermean[fc,]=rmvnorm(1,mean=colMeans(param[,pc,,drop=FALSE]),sigma=array(covar[,fc-1,],dim=c(noparam,noparam))/norand)
   
   #### If includeCovar is set all but the upper triangle, which is fixed by symmetry, is updated; else just the diagonal) ####
   # If no covariance in the model, this accomplishes setting all non-diagonal elements to 0. Otherwise, they are NA.
   if(!includeCovar) { covar[,fc,]=covar[,fc-1,]; covar[,fc,]=AssignDiag(covar[,fc,],NA) }  ## Cannot use diag(covar[,fc,])=NA
	      
   #### Update covariance matrix using conjugate inverse-Wishart or inverse-gamma
   if(update=='conjugate') 
    {
     if(includeCovar) covar[,fc,]=riwish(noparam+norand,diag(rep(1,noparam))+norand*cov(param[,pc,]))
     else for(j in 1:noparam) covar[j,fc,j]=Gibbs.normalvar(param[,pc,j])
     # else for(j in 1:noparam) covar[j,fc,j]=Gibbs.normalvar(logit(param[,pc,j]))
    }
   #### Or Update the covariance matrix with Metropolis. 
   else if(update=='metropolis')
	  for(j in 1:noparam) 
	    {
         if(includeCovar) testcol=1:j
         else testcol=j
       
         for(k in testcol) 
         {
          testparam=arrangeParam.Gibbs.2D(j,k,covar[,fc-1,],covar[,fc,])
          ## This is tested inside the likelihood function, so no need here
          # evect=eigen(testparam,symmetric=TRUE)$values; if(length(which(evect<=0))>0) browser()
          
          metropResult=metrop1step(func=lmerBayes.hyperllike.sigma,start.param=testparam[j,k],scale.param=covar.scale[j,k],fullcov=testparam,
                                   whichrow=j,whichcol=k,hypermean=hypermean[fc,],modelpar=param[,pc,],adjust=1.02,target=0.25)
          covar[j,fc,k]=metropResult[1]
          if(j!=k) covar[k,fc,j]=metropResult[1]
          covar.scale[j,k]=metropResult[2]
          # browser()
         }
         if(j<noparam) for(k in (j+1):noparam) covar[j,fc,k]=covar[k,fc-1,j]
      }

   if(debug) browser()

   #### Update the residual standard deviation (not used for Binom error). Since the residual SD is calculated from sdfunc, each of the sdparams must be updated. 
   if(error=='Gauss' | error=='GaussMultResid' | error=='NegBinom')
      {
       for(j in 1:noSDparam)
        {
         testparam=arrangeParam.Gibbs(fc,j,resid)
         if(is.null(fixef)) modelparam=param[,pc,]
         else modelparam=cbind(param[,pc,],fixparam[fc,])
         
         metropResult=metrop1step(func=residual.llike.lmerBayes,start.param=testparam[j],scale.param=scale.resid[j],sdpar=testparam,whichtest=j,
                                  fullpar=modelparam,data=x,trueN=y,model=model,errormodel=error,sdmodel=sdfunc,badparam=badSDparam,
                                  adjust=1.02,target=0.25,...)
         resid[fc,j]=metropResult[1]
         scale.resid[j]=metropResult[2]      
        }
      }
     
   #### Calculate full likelihood for current set of parameters ####
   llike[fc]=
	    full.likelihood.lmerBayes(one.covar=covar[,fc,],one.param=param[,pc,,drop=FALSE],one.hyper=hypermean[fc,],model=model,
                                  fixpar=fixparam[fc,],errormodel=error,data=x,obs=y,bad=badparam,sdmodel=sdfunc,sdpar=resid[fc,],...)
		
   #### Display progress to the screen every showstep steps ####
	 if(fc%%showstep==2)
		  {
		   showrows=IfElse(norand>5,5,norand)
		   cat("Full counter ", fc," and param counter ", pc, " at time ", date(), "\n")
		   for(k in 1:showrows) cat(randeffects[k],"... ",round(param[k,pc,],5),"\n")
		   cat(" ...and likelihood...",round(llike[fc],1),"  ")
		   cat(" ...and hypermu...",round(hypermean[fc,],4),"  ")
		   cat(" ...and hyperSD...",round(sqrt(diag(array(covar[,fc,],dim=c(noparam,noparam)))),4))
           if(!is.null(fixparam)) cat(" ...and fixef...",round(fixparam[fc,],4),"\n")
           else cat("\n")
           # browser()
		  }

   if(!is.null(paramfile) & pc%%savestep==0) 
		{ 
		 saveParamFile(param,paramfile,randeffects,firsttime=startfile)
		 param=resetParam(param,randeffects,savestep,parnames)
		 startfile=TRUE
		 pc=1
		}
   pc=pc+1
	 # browser()

  }

 #### Summary calculations are moved to a separate function
 result=list(mu=hypermean,sigma=covar,resid=resid,fullparam=param,fixef=fixparam,steps=steps,burn=burnin,llike=llike,obs=y,
             data=x,parnames=parnames,randeffects=randeffects,start=start)

 if(!is.null(paramfile)) return(result)
 return(summaryMCMC(fit=result,model=model,error=error,sdmodel=sdfunc,badparam=badparam,paramfile=NULL,...))
}
# </source>
# </function>
# 
#
# <function>
# <name>
# lmerBayes.hyperllike.sigma
# </name>
# <description>
# This is the hyper-likelihood for updating the covariances. It is always based on dmvnorm. The par is a matrix of parameters, one row per random effect,
# one column the set of parameters. It allows the Gibbs sampler to work by passing a single scalar parameter as the first
# argument.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
lmerBayes.hyperllike.sigma=function(testcov,fullcov,hypermean,modelpar,whichrow,whichcol)
{
 covar=arrangeParam.llike.2D(testcov,fullcov,whichrow,whichcol,forcesymmetry=TRUE)
 
 eigen.vector=eigen(covar,symmetric=TRUE,only.values=TRUE)$values
 if(!isSymmetric(covar)) browser()
 if(length(which(eigen.vector<=0))>0) { return(-Inf) }  # A covariance matrix must have strictly positive eigenvalues
 
 # In addition to testing eigenvectors. Problem is, I believe, that a nearly zero eigenvector may also fail to invert
 test=try(solve(covar))
 if(class(test)=='try-error') return(-Inf)
 
 llike=dmvnorm(modelpar,mean=hypermean,sigma=covar,log=TRUE)
 
 if(length(which(is.na(llike)))>0) browser()
 return(sum(llike))
} 
# </source>
# </function>
# 
#
# <function>
# <name>
# lmerBayes.hyperllike.mean
# </name>
# <description>
# This is the hyper-likelihood for updating the hypermeans, based on dmvnorm. The vector full.hypermean is the entire set; one of them, defined by the index whichtest, 
# is to be tested; covarSD is the covariance matrix. The modelpar is a matrix of parameters, one row per random effect, one column for each parameter. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
lmerBayes.hyperllike.mean=function(testmn,full.hypermean,whichtest,modelpar,covar)
{
 hypermean=arrangeParam.llike(testmn,full.hypermean,whichtest)
 
 llike=dmvnorm(modelpar,mean=hypermean,sigma=covar,log=TRUE)
 
 if(length(which(is.na(llike)))>0) browser()
 return(sum(llike))
} 
# </source>
# </function>
# 
#
# <function>
# <name>
# full.likelihood.lmerBayes
# </name>
# <description>
# Calculate full likelihood for any complete set of parameters, including every set for each random effect and hypermeans and covariances ####
# Further thought: the call to lmerBayes.hyperllike.sigma doesn't make sense, since llike.model.lmer already does this; the probability of 
# each set of parameters given the hyperparameters is already calculated. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
full.likelihood.lmerBayes=function(one.covar,one.param,one.hyper,fixpar=NULL,errormodel,data,obs,model,sdmodel='constant',sdpar,bad=NULL,duphyper=FALSE,...)
{
 norand=length(data)
 llike=numeric()
 
 ## one.param is a 3D array in all cases
 for(k in 1:norand) 
  {
    if(!is.null(fixpar)) thispar=c(one.param[k,1,],fixpar)
    else thispar=one.param[k,1,]
    
	llike[k]=llike.model.lmer(test=one.param[k,1,1],allparam=thispar,whichtest=1,data=drp(data[[k]]),trueN=drp(obs[[k]]),
	                          model=model,errormodel=errormodel,sdmodel=sdmodel,sdpar=sdpar,badparam=bad,mu=one.hyper,covar=one.covar,fixed=fixpar,...)
  }
 # browser()
 return(sum(llike))
}
# </source>
# </function>
# 
#

# To update fixed effects, can use full.llikelihood.lmerBayes. The fixed effects are used identically for every random effect.  
llike.fixef.lmer=function(testpar,fixpar,whichtest,one.param,errormodel,data,obs,model,sdmodel,sdpar,bad=NULL,...)
{
 allfixpar=arrangeParam.llike(testpar,fixpar,whichtest)
 llike=
  full.likelihood.lmerBayes(one.covar=NULL,one.param=one.param,one.hyper=NULL,fixpar=allfixpar,errormodel=errormodel,
                            data=data,obs=obs,model=model,sdmodel=sdmodel,sdpar=sdpar,bad=bad,...)
 return(llike)
}
# </source>
# </function>
# 
#
#
#
#
# <function>
# <name>
# llike.model.lmer
# </name>
# <description>
# A llikelihood function for one set of parameters, for a single random effect. The error is specified by errormodel, typically dbinom or dnorm.  
# It includes the likelihood of observing data given a response model (model) and its parameters (allparam), plus the hyper-likelihood of observing allparam
# given the hyperparameters, including hypermeans and covariance matrix. This is based off llike.model.occur.hierarch in fitLogisticMap.r, 
# but differs in including the covariance in the hyper-model. If the argument mu, for hypermeans, is set NULL, the likelihood without the hyper-likelihood is returned.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
llike.model.lmer=function(test,allparam,whichtest,data,trueN,model,sdmodel=constant,sdpar,errormodel='Gauss',badparam=NULL,
                          returnmodel=FALSE,mu,covar,fixed=NULL,debug=FALSE,...)
{
 param=arrangeParam.llike(test,allparam,whichtest)
 if(!is.null(badparam)) if(badparam(x=data,param=param,...)) return(-Inf)

 modeled=model(x=data,param=param,...)
 if(returnmodel) return(modeled)
 
 if(!is.null(sdpar)) withinSD=sdmodel(x=data,param=sdpar)
 if(!is.null(sdpar) & errormodel=='GaussMultResid') withinSD=sdmodel(x=modeled,param=sdpar)
 
 if(!is.null(sdpar)) if(length(which(withinSD<=0))>0) return(-Inf)  ## With GaussMultResid, many model predictions could lead to negative SD
 
 if(errormodel=='Binom' & length(which(modeled<0 | modeled>1))>0) return(-Inf) 
 if(errormodel=='NegBinom' & length(which(modeled<0))>0) return(-Inf) 
 if(length(which(is.na(modeled)))>0) return(-Inf)
  
 if(errormodel=='Binom') llike=dbinom(x=trueN,size=1,prob=modeled,log=TRUE)
 else if(errormodel=='Pois') llike=dpois(x=trueN,lambda=modeled,log=TRUE)
 else if(errormodel=='NegBinom') llike=dnbinom(x=trueN,mu=modeled,size=withinSD,log=TRUE)
 else if(errormodel=='Gauss' | errormodel=='GaussMultResid') llike=dnorm(x=trueN,mean=modeled,sd=withinSD,log=TRUE)
# else if(errormodel=='GaussMultResid') 
#  {
#   if(length(which(modeled<=0))>0) return(-Inf)       ## With this option, model must always be > 0
#   llike=dnorm(x=trueN,mean=modeled,sd=withinSD*modeled,log=TRUE)
#  }
# else if(errormodel=='Flat') llike=rep(0,length(modeled))
 if(is.na(sum(llike))) browser()

 if(is.null(fixed)) noparam=length(allparam) 
 else noparam=length(allparam)-length(fixed)
 
 # Clumsy due to R's dropped dimensions. Must force covar to 2D array, necessary when there is only one parameter
 if(!is.null(mu)) adjustcov=array(covar,dim=c(noparam,noparam))    

 # browser()
 if(!is.null(mu)) llike.hyper=dmvnorm(param[1:noparam],mean=mu,sigma=adjustcov,log=TRUE)
 else llike.hyper=0
 if(is.na(sum(llike.hyper))) browser()
 
 return(sum(llike)+sum(llike.hyper))
}
# </source>
# </function>
# 
#
# <function>
# <name>
# residual.llike.lmerBayes
# </name>
# <description>
# Calculate likelihood of residual standard deviation, given observations plus the predicting model and data (to make predictions).
# This likelihood does not depend on the hyperparameters. It does require data and prediction for every single random effect. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
residual.llike.lmerBayes=function(test,whichtest,data,trueN,model,sdpar,fullpar,sdmodel,badparam,errormodel='Gauss',...)
{
 param=arrangeParam.llike(test,sdpar,whichtest)
 norand=length(data)
 if(!is.null(badparam))  for(k in 1:norand) if(badparam(x=data[[k]],param=param,...)) return(-Inf)

 llike=numeric(norand)

 for(k in 1:norand)
  {
   modeled=model(x=drp(data[[k]]),param=fullpar[k,],...)
   withinSD=sdmodel(x=drp(data[[k]]),param=param)
   if(length(which(withinSD<=0))>0) return(-Inf)
   
   if(errormodel=='Gauss') all.llike=dnorm(x=drp(trueN[[k]]),mean=modeled,sd=withinSD,log=TRUE)
   else if(errormodel=='NegBinom') all.llike=dnbinom(x=drp(trueN[[k]]),mu=modeled,size=withinSD,log=TRUE)
   # else if(errormodel=='GaussMultResid') all.llike=dnorm(x=drp(trueN[[k]]),mean=modeled,sd=test*modeled,log=TRUE)
   if(is.na(sum(all.llike))) browser()
   llike[k]=sum(all.llike)
  }

 return(sum(llike))
}
# </source>
# </function>
# 
#
# Simply check a single SD parameter for sign.
# <function>
# <name>
# badSD
# </name>
# <description>
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
badSD=function(x,param,...)
{
 if(param<=0) return(TRUE)
 return(FALSE)
}
# </source>
# </function>
# 
#
# <function>
# <name>
# arrangeParam.llike.2D
# </name>
# <description>
# Used in likelihood function of a Gibbs sampler for lmerBayes, but for parameters submitted as a matrix, not a vector. 
# Allows any of a set of parameters to be submitted to metrop1step; whichrow, whichcol are the indices of the single parameter to test. 
# The argument forcesymmetry is needed for a covariance matrix: if one column is updated, the transposed column must also be.
# If NULL, zero, or NA, it simply returns allparam. This is based on arrangeParam.llike.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
arrangeParam.llike.2D=function(testparam,allparam,whichrow,whichcol,forcesymmetry=FALSE)
{
 param=allparam
 if(is.null(whichrow) | is.null(whichcol)) return(param)
 if(whichrow<=0 | is.na(whichrow) | whichcol<=0 | is.na(whichcol)) return(param)

 param[whichrow,whichcol]=testparam
 if(forcesymmetry) param[whichcol,whichrow]=testparam
 return(param)
}
# </source>
# </function>


# <function>
# <name>
# arrangeParam.Gibbs.2D
# </name>
# <description>
# Used in the loop of a Gibbs sampler, setting parameters not yet tested (j and above) to previous value (i-1), and other parameters (<j) to new value (i). 
# But unlike arrangeParam.Gibbs, on which this is based, this is for a matrix, not a vector. Also unlike arrangeParam.Gibbs, this accepts only two versions
# of the parameter matrix, the previous and the next. This is crucial for memory: otherwise, allparam would be a 3D array of large size.
# The scalar j refers to one row, and the scalar k a column. The function assumes that Gibbs updates occur by row, so every element on rows < j is set to the next parameter,
# plus all those in row j up to column k.  
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
arrangeParam.Gibbs.2D=function(j,k,allparam.prev,allparam.next)
{
 if(is.null(dim(allparam.prev))) return(allparam.prev)
 param=allparam.prev
 
 if(j>1) param[1:(j-1),]=allparam.next[1:(j-1),]
 if(k>1) param[j,1:(k-1)]=allparam.next[j,1:(k-1)]
 
 return(param)
}
# </source>
# </function>
#
#
# <function>
# <name>
# saveParamFile
# </name>
# <description>
# This saves a run of the full parameters into a text file, reducing the amount of memory needed.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
saveParamFile=function(param,paramfile,rand,firsttime=FALSE)
{
 norand=length(rand)
 
 for(k in 1:norand)
  {
   oneset=(param[k,,])
   fullset=data.frame(randeffect=I(rand[k]),oneset)
   
   if(k==1) fullparam=fullset
   else fullparam=rbind(fullparam,fullset)
  }
 # browser()
 
 write.table(fullparam,file=paramfile,append=firsttime,quote=FALSE,row.names=FALSE,col.names=!firsttime,sep='\t')
}
# </source>
# </function>
# 
#
# <function>
# <name>
# restoreParamFile
# </name>
# <description>
# Reverses the steps of saveParamFile, back to a 3D array. This requires the entire parameter set to be moved into memory, but it only happens
# once at the very end of the run. Since paramfile is gigantic, this is slow. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
restoreParamFile=function(paramfile)
{
 par=read.delim(paramfile,as.is=TRUE)
 param.list=split.data(par,splitcol='randeffect',showOutput=25)
 # browser()
 
 param=array(dim=c(length(param.list),dim(param.list[[1]])),dimnames=list(names(param.list),NULL,colnames(param.list[[1]])))
 for(k in 1:length(param.list)) param[k,,]=as.matrix(param.list[[k]])
 
 return(param)
}
# </source>
# </function>
# 
#
# <function>
# <name>
# resetParam
# </name>
# <description>
# This starts a new 3D parameter array whose first element is the last element of the current one. The rest of the new one is empty, to hold
# the next set of savestep steps from the Gibbs sampler. This only happens after the current one has been saved to a text file. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
resetParam=function(param,rand,savestep,pars)
{
 norand=length(rand)
 noparam=length(pars)
 # browser()
 
 newparam=array(dim=c(norand,savestep,noparam),dimnames=list(rand,NULL,pars))
 newparam[,1,]=param[,savestep,]
 
 return(newparam)
}
# </source>
# </function>
# 
#
# <function>
# <name>
# summaryMCMC
# </name>
# <description>
# Make summary calculations based on the full Gibbs sampler. The argument fit is an object holding all steps of the sampler, plus data, observations,
# and likelihood. However, if parameters were saved along the way to a text file, then the argument paramfile is used to name the file and restore them
# into a 3D array. Estimates of confidence limits of all parameters are returned. If returnfull is set TRUE, then the entire 3D array of parameters is
# also returned. Full likelihood at the best parameters is calculated and likelihood at each step in sampler are used to calculate DIC.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
summaryMCMC=function(fit,model,error,sdmodel,badparam,paramfile=NULL,returnfull=TRUE,...)
{
 if(!is.null(paramfile)) param=restoreParamFile(paramfile)
 else param=fit$fullparam
 
 noparam=length(fit$parnames)
 norand=length(fit$randeffects)
 
 keep=(1+fit$burn):fit$steps
 good=length(keep)
 
 best=hi=low=data.frame(matrix(nrow=norand,ncol=noparam))
 colnames(best)=colnames(hi)=colnames(low)=fit$parnames
 rownames(best)=rownames(hi)=rownames(low)=fit$randeffects
 
 for(k in 1:norand) 
  {
   # Must force a set of parameters to have 2D in case there is only one param
   oneset=array(param[k,keep,],dim=c(good,noparam))
	 best[k,]=colMeans(oneset)
	 low[k,]=apply(oneset,2,CI)[1,]
	 hi[k,]=apply(oneset,2,CI)[2,]
	 if(k%%1000==0) cat('Calculated parameters for random effect ', fit$randeffects[k], '\n')
  }

 if(!is.null(fit$resid))
  {
   useresid=fit$resid[keep,]
   if(is.null(dim(useresid)))
    {
     bestresid=mean(useresid)
     CIresid=CI(useresid)
    }
   else
    {
     bestresid=colMeans(useresid)
     CIresid=CI(useresid)
    } 
  }
 else bestresid=CIresid=NULL
 
 if(!is.null(fit$fixef))
  {
   usefix=fit$fixef[keep,]
   if(is.null(dim(usefix)))
    {
     bestfix=mean(usefix)
     CIfix=CI(usefix)
    }
   else
    {
     bestfix=colMeans(usefix)
     CIfix=CI(usefix)
    } 
  }
 else bestfix=CIfix=NULL
  
 # browser()
 
 # Calculate predictions of observations for each random effect given the best model, and likelihood of data given those predictions
 pred=list()
 rand.llike=numeric()
 for(k in 1:norand) 
  {
   if(!is.null(bestfix)) useparam=c(drp(best[k,]),bestfix)
   else useparam=drp(best[k,])

   pred[[k]]=model(param=useparam,x=fit$data[[k]])

   rand.llike[k]=
     llike.model.lmer(test=best[k,1],allparam=drp(best[k,]),whichtest=1,data=fit$data[[k]],trueN=drp(fit$obs[[k]]),
                      model=model,sdmodel=sdmodel,sdpar=bestresid,errormodel=error,badparam=badparam,mu=NULL,covar=NULL,fixed=bestfix,...)
  }
 names(pred)=names(rand.llike)=fit$randeffects
 
 bestmu=colMeans(fit$mu[keep,,drop=FALSE])
 names(bestmu)=fit$parnames
 bestsigma=apply(fit$sigma[,keep,,drop=FALSE],c(1,3),mean)

 CImu=apply(fit$mu[keep,,drop=FALSE],2,CI)
 CIsigma=apply(fit$sigma[,keep,,drop=FALSE],c(1,3),CI)
 # browser()
 
 # This bizarre work around has been necessary due to clumsiness required to handle a single parameter, so that arrays have dimensions dropped
 best3D=array(dim=c(norand,1,noparam))
 for(j in 1:noparam) best3D[,1,j]=best[,j]
 # Full likelihood of best parameter combination, covering hyperparameters and species parameters
 bestllike=full.likelihood.lmerBayes(one.covar=bestsigma,one.param=best3D,one.hyper=bestmu,model=model,sdmodel=sdmodel,sdpar=bestresid,fixpar=bestfix,
                                     errormodel=error,data=fit$data,obs=fit$obs,bad=badparam,...)
                                     
 meanDeviance=(-2)*mean(fit$llike[keep])
 devianceMeanParam=(-2)*bestllike
 effectNoPar=meanDeviance-devianceMeanParam
 DIC=2*effectNoPar+meanDeviance

 result=list(mu=fit$mu,sigma=fit$sigma,bestmu=bestmu,bestfix=bestfix,CIfix=CIfix,CImu=CImu,bestsigma=bestsigma,CIsigma=CIsigma,
             resid=fit$resid,bestresid=bestresid,CIresid=CIresid,
             best=best,lower=low,upper=hi,steps=fit$steps,burn=fit$burn,llike=fit$llike,bestlike=bestllike,DIC=DIC,obs=fit$obs,data=fit$data,
             model=pred,randlike=rand.llike,keep=keep,start=fit$start,randeffects=fit$randeffects,parnames=fit$parnames,DIC=DIC,effectivePar=effectNoPar)

 # browser()
 if(returnfull) result$fullparam=param
 if(returnfull) result$fixparam=fit$fixed
 
 return(result)
}
# </source>
# </function>
# 
#
# <function>
# <name>
# recalculate.lmerBayesllike
# </name>
# <description>
# Walk through entire chain of parameters to calculate full likelihood at each step, as was done during the model run. The argument keep
# defines the elements to be used, or if NULL, fit$keep is used. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
recalculate.lmerBayesllike=function(fit,model,error,bad=NULL,keep=NULL,dup=TRUE,...)
{
 full.llike=numeric()
 
 for(i in keep)
  {
   full.llike[i]=
       full.likelihood.lmerBayes(one.covar=fit$sigma[,i,],one.param=fit$fullparam[,i,],one.hyper=fit$mu[i,],model=model,withinSD=fit$resid[i],
                                 errormodel=error,data=fit$data,obs=fit$obs,bad=bad,duphyper=dup,...)
   if(i%%25==0) cat("Log-likelihood at step ", i, "=", full.llike[i], " at ", date(), "\n")
  }
                                 
 return(full.llike)
}
# </source>
# </function>
# 
#
# <function>
# <name>
# covTocorr
# </name>
# <description>
# Convert covariance matrix to correlation matrix. Each element is divided by the square root of the product of the corresponding diagonal terms.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
covTocorr=function(cov)
{
 result=cov
 nopar=dim(cov)[1]
 
 for(i in 1:nopar) for(j in 1:nopar) result[i,j]=cov[i,j]/sqrt(cov[i,i]*cov[j,j])
 return(result)
}
# </source>
# </function>
