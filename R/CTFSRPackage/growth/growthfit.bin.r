# <function>
# <name>
# extract.growthdata
# </name>
# <description>
# Extract data for growth rates from plot databases and 2 censuses in CTFS R format. Returns a table with 
# growth, size (ie dbh), and species name. Default is to return log-transformed growth, with negative and zero
# growth set to a mingrow, but with logit=TRUE, growth and dbh are not log-transformed.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
extract.growthdata=function(census1,census2,growcol="incgr",mingrow=0.1,logit="x",growthfunc=growth.biomass.indiv,pomcut=10000,
                            rounddown = FALSE, mindbh = 10, dbhunit = "mm", err.limit = 4, maxgrow = 75, exclude.stem.change=TRUE, returnfull=FALSE)
{   
 growthtable=growthfunc(census1,census2,rounddown=rounddown,mindbh=mindbh,dbhunit=dbhunit,err.limit=err.limit,maxgrow=maxgrow,
                        pomcut=pomcut,exclude.stem.change=exclude.stem.change)
 growthrate=growthtable[,growcol]
 dbh=growthtable$dbh1
 treeID=growthtable$treeID
 agb=growthtable$agb1
 
 if(logit=="x" | logit=="xy") 
  {
   dbh=log(growthtable$dbh1)
   agb=log(growthtable$agb1)
  }
  
 if(logit=="y" | logit=="xy")
  {
   growthrate[growthrate<=0]=mingrow
   growthrate=log(growthrate)
  }

 result=data.frame(sp=I(growthtable$sp),treeID,dbh=dbh,agb=agb,growth=growthrate)
 result=subset(result,!is.na(growth) & !is.na(dbh) & !is.na(agb) & !is.na(sp))
 if(!returnfull) return(result)
 
 full=subset(growthtable,!is.na(growthrate) & !is.na(dbh1) & !is.na(agb1) & !is.na(sp),select=c('sp','treeID','dbh1','dbh2','agb1','agb2','time','incgr'))
 colnames(full)[which(colnames(full)=='incgr')]='growth'
 
 if(returnfull) return(full)
 
}
# </source>
# </function>


# <function>
# <name>
# run.growthbin.manyspp
# </name>
# <description>
# Run the model to fit growth rate in bins for many species, 1-4 bins. It takes a list of species, extracts
# growth rates for each, one at a time, from the table of growth rates, then calls run.growthfit.bin to fit the model for
# the 4 bin options. 
# Sample species vector from BCI: <br>
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# spp20=c('tri2tu','alsebl','tet2pa','tachve','beilpe','pri2co','quaras','ocotwh','hirttr','gar2in','protpa','protte',
#         'eugeoe','virose','guargu','maquco','jac1co','cecrin','cordbi','micoar') <br>
# Creating the complete table of biomass growth for all individuals in a plot: <br>
# agb.growth=extract.growthdata(bci.full5,bci.full6,growthfunc=growth.biomass.indiv,logit='x',<br>
#                               rounddown = FALSE,mindbh = 100,dbhunit = 'mm',err.limit = 4,maxgrow = 75) <br>
# Creating a vector of all species names in the agb.growth table. <br>
# allspecies=sort(unique(agb.growth$sp)) <br>
# Fitting the model for all species, 1-4 bins: <br>
# fit=run.growthbin.manyspp(growthdata=agb.growth,size='agb',spp=allspecies,minabund300=15,minTotal=40,startpar=c(.03,.005),startsdpar=c(.04,0))

# </sample>
# <source>
run.growthbin.manyspp=function(growthdata,size='dbh',spp=spp20,minabund300=15, minTotal=40, dbhunit='mm', sdmodel=linear.model.ctr,
                               startpar=c(.03,.005), startsdpar=c(.04,0), badsdfunc=NULL, binoption=1:4, noreps=5000,noburn=2500,noshow=500,
                               outputname='linear.fit',path='',...)
{ 
 result=list()
 on.exit(cat(onesp,'\n'))
 outputfile=pst(path,outputname,'.rdata')
 
 for(onesp in spp)
  {
   spdata=subset(growthdata,sp==onesp)
   total=dim(spdata)[1]
   
   if(dbhunit=='mm') nobig=dim(subset(spdata,dbh>=log(300)))[1]
   else if(dbhunit=='cm') nobig=dim(subset(spdata,dbh>=log(30)))[1]
   
   if(total>=minTotal & nobig>=minabund300)
    {
     result[[onesp]]=
       run.growthfit.bin(growthdata=spdata,size=size,binoption=binoption,startpar=startpar,
                         sdmodel=sdmodel,startsdpar=startsdpar,badsdfunc=badsdfunc,
                         norep=noreps,noburn=noburn,noshow=noshow,...)
     
     for(j in 1:length(result[[onesp]])) 
       result[[onesp]][[j]]$summary=list(dbhunit=dbhunit, totalsample=total, totalbig=nobig)
     cat('Finished species ', onesp, ' with ', nobig, ' big trees\n') 
    }
  
	assign(outputname,result)
	save(list=outputname,file=outputfile)
  }

 return(result)
}
# </source>
# </function>


# <function>
# <name>
# run.growthfit.bin
# </name>
# <description>
# Find best fits for linearmodel.bin, with one set of data and a series of bins. This starts fit for a bin
# based on the best fit for the prior bin, thus always assuring an improved fit. This works best if the first bin=1,
# which is just a linear model and easily fit by optim and the Gibbs sampler.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
run.growthfit.bin=function(growthdata,size="dbh",startpar=c(.03,.005),startsdpar=c(.04,0),sdmodel=linear.model.ctr,badsdfunc=NULL,binoption=1:4,
                           noreps=5000,noburn=2500,noshow=500,...)
{ 
 result=vector("list",length(binoption))
 
 nextModelpar=startpar
 nextSDpar=startsdpar

 for(i in 1:length(binoption))
  {
   result[[i]]=
     growth.flexbin(growthtable=growthdata,sizecol=size,nobin=binoption[i],start=nextModelpar,startsd=nextSDpar,sdmodel=sdmodel,badsdfunc=badsdfunc,
                    rep=noreps,burn=noburn,show=noshow,...)

   nextModelpar=addBinParam(x=growthdata[,size],result[[i]]$max.param,bin=binoption[i])
   nextSDpar=result[[i]]$bestSD
   
   cat("Finished bin ", binoption[i], "\n")
  }
  
 return(result)
}
# </source>
# </function>

                   
# <function>
# <name>
# growth.flexbin
# </name>
# <description>
# Fitting a regression line through log growth against log dbh, binning on dbh. Growth table has 2 columns,
# one named size (ie, dbh), other growth. The function now calls optim first to find a set of parameters, then, if Gibbs
# is selected, runs the Gibbs sampler starting with those parameters. Since optim fails if the initial likelihood is -Inf,
# it became necessary to check the parameters with bad.binparam before running. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
growth.flexbin=function(growthtable,sizecol="dbh",nobin=2,start=NULL,startsd=NULL,sdmodel=linear.model.ctr,badsdfunc=NULL,
                        method='Gibbs',rep=1200,show=100,burn=200,...) 
{
 size=growthtable[,sizecol]
 growthtable$growth

 if(is.null(start)) start=c(defineBinBreaks(size,nobin,...),rep(0.1,nobin),0)
 if(is.null(startsd)) startsd=c(0.02,0)

 bad=bad.binparam(param=start,x=size,pred=NULL,...)
 if(bad) start=c(defineBinBreaks(size,nobin,...),rep(0.1,nobin),0)
 bad=FALSE
 if(!is.null(badsdfunc)) bad=badsdfunc(size,param=startsd)
 if(bad) startsd=defineSDpar(size,length(startsd))

 fit=optim(par=c(start,startsd),fn=llike.linearbin.optim,control=list(fnscale=(-1)),
           x=size,y=growthtable$growth,predfunc=linearmodel.bin,nomainpar=length(start),
           llikefunc=llike.GaussModel,sdfunc=sdmodel,badpredpar=bad.binparam,badsdpar=badsdfunc,...)

 fit$best=best.optim=fit$par[1:length(start)]
 fit$bestSD=bestSD.optim=fit$par[-(1:length(start))]
 bestllike.optim=fit$value
 bestpred=linearmodel.bin(size,param=best.optim,...)
 fit$model=data.frame(x=size,y=growthtable$growth,pred=bestpred)
 fit$model=fit$model[order(fit$model$x),]
 
 if(method=='Gibbs')            
  fit=model.xy(x=size,y=growthtable$growth,predfunc=linearmodel.bin,llikefunc=llike.GaussModel,badpredpar=bad.binparam,
               start.predpar=best.optim,sdfunc=sdmodel,start.sdpar=bestSD.optim,llikefuncSD=llike.GaussModelSD,badsdpar=badsdfunc,
               steps=rep,burnin=burn,showstep=show,...)
 
 fit$llike.best=
   llike.linearbin.optim(param=c(fit$best,fit$bestSD),x=size,y=growthtable$growth,predfunc=linearmodel.bin,nomainpar=length(start),
                         llikefunc=llike.GaussModel,sdfunc=sdmodel,badpredpar=bad.binparam,badsdpar=badsdfunc,...)  
            
 fit$growth=growthtable
 fit$bins=nobin
 fit$optimpar=best.optim
 fit$optimSD=bestSD.optim
 fit$optimllike=bestllike.optim
 optimal=which.max(fit$llike[fit$keep])
 fit$max.param=fit$fullparam[fit$keep,][optimal,]
 
 return(fit)
}
# </source>
# </function>


# MINIMUM_SD=0.00

# <function>
# <name>
# llike.linearbin.optim
# </name>
# <description>
# This is for optim, a single function taking all parameters at once, including sd, to get single likelihood. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
llike.linearbin.optim=function(param,x,y,predfunc,nomainpar,badpredpar,llikefunc,sdfunc,badsdpar,...)
{
 extra=list(...)
 if(is.null(extra$MINIMUM_SD)) MINIMUM_SD=0.00
 else MINIMUM_SD=extra$MINIMUM_SD
 
 predpar=param[1:nomainpar]
 binparam=predpar[1:(nomainpar/2-1)]
 sdpar=param[-(1:nomainpar)]
 
 SD=sdfunc(x,sdpar)
 
 if(length(which(SD<=MINIMUM_SD))>0) return(-Inf)
 if(!is.null(badsdpar)) if(badsdpar(x,sdpar)) return(-Inf)

 llike=llikefunc(testparam=predpar[1],allparam=predpar,whichtest=1,x=x,obs=y,model=predfunc,badpred=badpredpar,SD=SD)
 
 total=sum(llike)
 if(is.na(total)) browser()

 return(total)
}
# </source>
# </function>


# <function>
# <name>
# defineBinBreaks
# </name>
# <description>
# This finds divisions of over the vector size which produce equal number of elements per division. In case
# any of those divisions are too short, it tries equal sized divisions. A default to use for
# start parameters when none are supplied. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
defineBinBreaks=function(size,nobin,...)
{
 extra=list(...)
 if(is.null(extra$MINBINWIDTH)) MINBINWIDTH=0.1
 else MINBINWIDTH=extra$MINBINWIDTH
 if(is.null(extra$MINBINSAMPLE)) MINBINSAMPLE=5
 else MINBINSAMPLE=extra$MINBINSAMPLE

 if(nobin==1) return(numeric(0))
 
 quant=(1:(nobin-1))/nobin 
 internal=quantile(size,prob=quant)
 breaks=c(min(size),internal,max(size))
 if(wideEnoughBins(x=size,b=breaks,...)) return(internal)

 breaks=seq(min(size),max(size),length=nobin+1)
 internal=breaks[2:nobin] 
 if(enoughSamplePerBin(x=size,b=breaks,...)) return(internal)

 internal=breaks=numeric()
 breaks[1]=min(size)
 minwidth=MINBINWIDTH*diff(range(size))
 size=sort(size)
 for(i in 1:(nobin-1))
  {
   whichnext=MINBINSAMPLE*i+1
   if(whichnext<=length(size)) enough=size[whichnext]
   else enough=max(size)
   
   wide=breaks[i]+minwidth
   internal[i]=breaks[i+1]=IfElse(enough>wide,enough,wide)
  }
  
 return(internal)
}
# </source>
# </function>

# <function>
# <name>
# defineSDpar
# </name>
# <description>
# For default SD parameters, if nothing else works. Choose the midpoint of x for the middle of 3 parameters.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
defineSDpar=function(x,nopar)
{
 if(nopar==2) return(c(0.02,0))
 midpoint=0.5*(min(x)+max(x))
 return(c(0,midpoint,0.001))
}
# </source>
# </function>

# <function>
# <name>
# enoughSamplePerBin
# </name>
# <description>
# Test whether the number of elements in a vector x between successive breaks exceeds a minimum. If any
# bins have too few, it returns FALSE. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
enoughSamplePerBin=function(x,b,...)
{
 extra=list(...)
 if(is.null(extra$MINBINSAMPLE)) minsample=5
 else minsample=extra$MINBINSAMPLE

 dbhcat=cut(x,b,right=FALSE)
 Npercat=table(dbhcat)
 if(length(which(Npercat<minsample))>0) return(FALSE) 
 else return(TRUE)
}
# </source>
# </function>

# <function>
# <name>
# wideEnoughBins
# </name>
# <description>
# Test whether all the bin widths exceed a minimum. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
wideEnoughBins=function(x,b,...)
{
 extra=list(...)
 if(is.null(extra$MINBINWIDTH)) minwidth=0.1
 else minwidth=extra$MINBINWIDTH

 delta=diff(b)
 if(length(which(delta<minwidth*diff(range(x))))>0) return(FALSE)
 else return(TRUE)
}
# </source>
# </function>

# MINBIN=0.1
# MINBINSAMPLE=5

# <function>
# <name>
# bad.binparam
# </name>
# <description>
# This prevents the bin parameters from moving outside the x range, and keeps the minimum bin width wider than MINBIN of the
# xrange. It also requires at least MINSAMPLE individuals per bin. The ellipsis handles the submission of MINBIN and MINBINSAMPLE, if
# they are not submitted, default values are assigned.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
bad.binparam=function(x,param,...)
{
 x=as.matrix(x)
 
 # browser()
 
 noparam=length(param)
 bins=(noparam)/2
 
 if(bins==1) return(FALSE)

 internal=param[1:(bins-1)]
 if(length(which(internal<=min(x)))>0) return(TRUE)
 if(length(which(internal>=max(x)))>0) return(TRUE)

 breaks=c(min(x),internal,max(x))
 
 if(!enoughSamplePerBin(x=x,b=breaks,...)) return(TRUE)
 if(!wideEnoughBins(x=x,b=breaks,...)) return(TRUE)
 
 return(FALSE)
}
# </source>
# </function>

# <function>
# <name>
# bad.binsdpar
# </name>
# <description>
# 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
bad.binsdpar=function(x,param,...)
{
 if(length(param)<=2) return(FALSE)

 if(param[3]<=0) return(TRUE) 
 breaks=c(min(x),param[2],max(x))
 if(!wideEnoughBins(x=x,b=breaks,...)) return(TRUE)
 
 return(FALSE)
}
# </source>
# </function>


# <function>
# <name>
# calculateBinModel.BIC
# </name>
# <description>
# Calculate Bayes Information Criteria using Wikipedia formula
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
calculateBinModel.BIC=function(fit)
{
 N=dim(fit$model)[1]
 noparam=length(fit$best)+length(fit$bestSD)
 error.var=(1/N)*sum((fit$model$pred-fit$model$y)^2)
 
 return(N*log(error.var)+noparam*log(N))
}
# </source>
# </function>

# Description: Calculate Deviance Information Criteria, using Wikipedia formula
calculateBinModel.DIC=function(fit)
  return((-4)*mean(fit$llike[fit$keep])+2*fit$llike.best)
  
# <function>
# <name>
# calculateBinModel.AIC
# </name>
# <description>
# Calculate AIC of the model, using various log(likelihood) estimators: 
# with optim, the highest likelihood found by optim
# with mean, the mean llikelihood from the Gibbs sampler
# with gibbs, the maximum llikelihood from the Gibbs sampler
# with none, just return the mean Gibbs likelihood
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
calculateBinModel.AIC=function(fit,type='optim')
{
 noparam=length(fit$best)+length(fit$bestSD)
 if(type=='optim') return(fit$optimllike-2*noparam)
 if(type=='mean') return(mean(fit$llike[fit$keep])-2*noparam)
 if(type=='max') return(max(fit$llike[fit$keep])-2*noparam)
 if(type=='none') return(max(fit$llike[fit$keep]))

 return(NA)
}
# </source>
# </function>


# <function>
# <name>
# calculateBinModel.bestpred
# </name>
# <description>
# Calculate mean predicted value at every x using every one of the Gibbs sampler parameter combinations
# (excluding burn in)
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
calculateBinModel.bestpred=function(fit)
{
	allparam=fit$fullparam[fit$keep,]
	noparam=dim(allparam)[1]
	norep=dim(allparam)[2]
	
	for(i in 1:norep)
	{
	 onepred=linearmodel.bin(fit$model$x,param=allparam[i,])
	 if(i==1) meanpred=onepred
	 else meanpred=meanpred+onepred
	}
	
	return(meanpred/norep)
}   
# </source>
# </function>
 

# <function>
# <name>
# assembleBinOutput
# </name>
# <description>
# Use the list output from piecewise regression (growthfit.bin) and converts to a flat table. The single argument 
# inputdata is the output compare.growthbinmodel.
# Written by Adrian Das.
# (excluding burn in)
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# bciresult=compare.growthbinmodel(linearbin.fit.trim3,export=NULL,makegraph=FALSE)
# bcitable=assembleBinOutput(bciresult,sitename='bci_12_spreadSD')
# write.table(bcitable,file='growth/BCIpiecewise.txt',quote=FALSE,sep='\t',row.names=FALSE) 
# </sample>
# <source>
assembleBinOutput=function(inputtable,fulldata,sitename)
{
	holder<-inputtable
	
	spechold<-dimnames(holder$slope)[[1]]
	maxbinhold<-apply(holder$slope, 1, function(x){length(x[is.na(x)==F])})
	
	numbins<-length(holder$slopes[1,])
	spp<-rownames(holder$slopes)
	numspec<-length(spp)
	
	holdcomb<-data.frame(spechold,maxbinhold)
	rownames(holdcomb)=spp
	
	dimnames(holdcomb)[[2]][1:2]<-c("Species","MaxBin")
	
	for(i in 1:numbins)
		{
	     holdcomb<-cbind(holdcomb,holder$slope[,i],holder$upper[,i],holder$lower[,i])
	     dimnames(holdcomb)[[2]][(3*i):(3*i+2)]<-c(paste("slope",i,sep=""),paste("upper",i,sep=""),paste("lower",i,sep=""))
		}

	holdcomb$unit=holdcomb$sample=holdcomb$bigsample=NA
	
	for(i in 1:numspec){
		holdcomb$slopelast[i]<-holdcomb[spp[i],(holdcomb$MaxBin[i]*3)]
		holdcomb$upperlast[i]<-holdcomb[spp[i],(holdcomb$MaxBin[i]*3+1)]
		holdcomb$lowerlast[i]<-holdcomb[spp[i],(holdcomb$MaxBin[i]*3+2)]
	   
		if(!is.null(fulldata[[spp[i]]][[1]]$summary))
		  {
		   summ=fulldata[[spp[i]]][[1]]$summary
		   holdcomb[spp[i],c('sample','bigsample')]=c(summ$totalsample,summ$totalbig)
		   holdcomb[spp[i],'unit']=summ$dbhunit
		  }
	}

	holdcomb$site=sitename
	
	return(holdcomb)
}
# </source>
# </function>
