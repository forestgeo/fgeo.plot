
# <function>
# <name>
# graph.growthmodel.spp
# </name>
# <description>
# Use output of growth.flexbin to graph observed growth and predictions. With conf>0, random draws from posterior parameters
# are used to make predictions and overlay with gray lines, and mean predicted value at each x is calculated.
# The option whichpred determines whether to graph the fitted values based on the median parameters of the Gibbs sampler, or
# the mean fitted value of all the steps in the Gibbs sampler.  
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
graph.growthmodel.spp=function(fit,jiggle=.001,whichpred='pred',xrange=NULL,yrange=NULL,xtitle=NULL,ytitle=NULL,includeaxs=TRUE,
                               withSD='red',regclr="green",modelclr="blue",modellwd=1,graphdiv=10,add=FALSE,addpts=TRUE,maintitle=NULL,conf=0)
{
 size=fit$model$x
 obs=fit$model$y
 if(whichpred=='meanpred') pred=fit$model$meanpred
 else pred=fit$model$pred
 
 if(is.null(yrange)) yrange=range(c(obs,pred))
 if(is.null(xrange)) xrange=range(size)
 
 if(is.null(xtitle)) xtitle='log size'
 if(is.null(ytitle)) ytitle='growth'
 
 xjiggle=rnorm(length(size),mean=0,sd=jiggle)
 yjiggle=rnorm(length(size),mean=0,sd=jiggle)

 if(addpts)
  {
   if(!add) plot(size+xjiggle,obs+yjiggle,xlim=xrange,ylim=yrange,pch=16,cex=.8,xlab=xtitle,ylab=ytitle,axes=includeaxs)
   else points(size+xjiggle,obs+yjiggle,pch=16,cex=.8)
  }
 else
  {
   if(add) lines(size,pred,col=modelclr,lwd=modellwd)
   else plot(size,pred,type='l',xlim=xrange,ylim=yrange,xlab=xtitle,ylab=ytitle,col=modelclr,lwd=modellwd,axes=includeaxs)
  }

 b=seq(min(size),max(size)+.0001,len=graphdiv-1)
 sizecat=cut(size,breaks=b,right=FALSE)
 
 if(conf>0)
  {
   allparam=fit$fullparam[fit$keep,]
   noparam=dim(allparam)[1]
   r=sample(1:noparam,conf)
   # browser()
   for(i in 1:conf)
    {
     onepred=linearmodel.bin(fit$model$x,param=allparam[r[i],])
     lines(fit$model$x,onepred,col='gray10',lwd=0.5)
     if(i==1) meanpred=onepred
     else meanpred=meanpred+onepred
    }
    
   lines(fit$model$x,meanpred/conf,lty='dashed',lwd=1.5,col='purple')
  }

 meansize=tapply(size,sizecat,mean)
 meangrowth=tapply(obs,sizecat,mean)
 
 if(!is.null(regclr)) lines(meansize,meangrowth,col=regclr,lwd=modellwd)
 if(addpts) lines(size,pred,col=modelclr,lwd=modellwd)
 
 if(!is.null(withSD))
  { 
   lines(size,pred+fit$model$predSD,col=withSD)
   lines(size,pred-fit$model$predSD,col=withSD)
  }
 
 ypos=max(obs)-0.3*diff(range(obs))
 xpos=min(size)+0.1*diff(range(size))
 if(!is.null(maintitle)) text(xpos,ypos,maintitle,cex=1.7,pos=4)
 
 return(data.frame(meansize,meangrowth))
}
# </source>
# </function>


# <function>
# <name>
# graph.growthmodel
# </name>
# <description>
# Graph growth rates and model fit. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
graph.growthmodel=function(spp,fitlist,whichbin=1,regclr="green",modelclr="blue",graphdiv=10,export=pdf,outfile="growth/linearbin.fit.pdf",h=8,w=10)
{
 if(!is.null(export)) 
  {
   on.exit(graphics.off())
   export(file=outfile,height=h,width=w)
  }
  
 for(onesp in spp)
  {
   if(is.null(export)) x11(height=5,width=9)
   graph.growthmodel.spp(fit=fitlist[[onesp]][[whichbin]],graphdiv=20,add=FALSE,modelclr="blue",maintitle=onesp)
  } 
}
# </source>
# </function>

# <function>
# <name>
# overlay.growthbinmodel
# </name>
# <description>
# Show model fits for 1, 2, 3, and 4 bins on each species
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
overlay.growthbinmodel=function(fit,bins=1:4,regclr="green",modelclr="blue",graphdiv=15,add=FALSE,newgraph=TRUE,
                                export=pdf,outfile="growth/linearbin.overlay.pdf",h=8,w=10)
{
 if(!is.null(export)) 
  {
   on.exit(graphics.off())
   export(file=outfile,height=h,width=w)
  }
  
 allspp=names(fit)
 
 clrlist=c("blue","red","gray","black","orange")
 for(onesp in allspp)
  {
   if(is.null(export) & newgraph) x11(height=5,width=9)
   
   for(b in bins)
    {
     if(b==bins[[1]]) graph.growthmodel.spp(fit=fit[[onesp]][[b]],whichpred='pred',graphdiv=graphdiv,add=add,modelclr="blue",maintitle=onesp)
     else
       graph.growthmodel.spp(fit=fit[[onesp]][[b]],whichpred='pred',regclr=NULL,graphdiv=graphdiv,add=TRUE,addpts=FALSE,modelclr=clrlist[b],maintitle=NULL)
    } 
  }
}
# </source>
# </function>

# <function>
# <name>
# compare.growthbinmodel
# </name>
# <description>
# Calculates various metrics of fit: DIC, BIC, AIC based on the maximum likelihood, AIC based on the mean of the Gibbs sampler,
# from output of the model fit (all species, all bins 1:4). 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
compare.growthbinmodel=function(fit,bins=1:4,makegraph=TRUE,conflines=0,newgraph=TRUE,export=pdf,outfile="growth/linearbin.bestfit.pdf",h=8,w=10)
{
 spp=names(fit)
 
 bestbin=numeric()
 BIC=DIC=AICopt=AICgibbs=meanllike=optllike=matrix(ncol=length(bins),nrow=length(spp))
 colnames(AICopt)=colnames(AICgibbs)=colnames(BIC)=colnames(DIC)=colnames(meanllike)=colnames(optllike)=pst("bin",bins)
 rownames(AICopt)=rownames(AICgibbs)=rownames(BIC)=rownames(DIC)=rownames(meanllike)=rownames(optllike)=spp
 
 for(i in 1:length(spp))
  {
   fit.onesp=fit[[spp[i]]]

   for(j in 1:length(fit.onesp)) 
     {
      BIC[i,j]=calculateBinModel.BIC(fit.onesp[[j]])
      DIC[i,j]=calculateBinModel.DIC(fit.onesp[[j]])
      AICopt[i,j]=calculateBinModel.AIC(fit.onesp[[j]],type='optim')
      AICgibbs[i,j]=calculateBinModel.AIC(fit.onesp[[j]],type='mean')  
      meanllike[i,j]=mean(fit.onesp[[j]]$llike[fit$keep])
      optllike[i,j]=fit.onesp[[j]]$optimllike
     }
   bestbin[i]=which.max(AICgibbs[i,])
  } 

 if(!is.null(export)) 
  {
   on.exit(graphics.off())
   export(file=outfile,height=h,width=w)
  }

 slope=upper=lower=matrix(ncol=4,nrow=length(spp))
 rownames(slope)=rownames(upper)=rownames(lower)=spp
 
 for(i in 1:length(spp))
  {
   fit.onesp=fit[[spp[i]]]
   best=bins[bestbin[i]]
   
   if(best==1) plural='bin'
   else plural='bins'
   
   if(newgraph & makegraph) x11(height=5,width=9)
   if(makegraph) 
     graph.growthmodel.spp(fit=fit.onesp[[best]],whichpred='pred',graphdiv=15,modelclr="blue",maintitle=paste(spp[i],best,plural),conf=conflines)

   if(best==1) slopecol=2
   else if(best==2) slopecol=2:3
   else if(best==3) slopecol=3:5
   else if(best==4) slopecol=4:7
   
   slope[i,1:length(slopecol)]=fit.onesp[[bestbin[i]]]$best[slopecol]
   upper[i,1:length(slopecol)]=fit.onesp[[bestbin[i]]]$CI[2,slopecol]
   lower[i,1:length(slopecol)]=fit.onesp[[bestbin[i]]]$CI[1,slopecol]
   
  }
   
 return(list(slopes=slope,upper=upper,lower=lower))
}

# </source>
# </function>


# <function>
# <name>
# graph.outliers.spp
# </name>
# <description>
# Pass the output of extract.growth with every individual\'s growth (full), and another after outliers have been trimmed. This finds
# the records trimmed and overlays them on the entire graph. If a model is submitted, then the curves are graphed too.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# <ul>
# <li> full=extract.growthdata(census1=bci.full5,census2=bci.full6,growcol='incgr',growthfunc=growth.biomass.indiv,logit='x',rounddown = FALSE,
#                         mindbh = 100,dbhunit = 'mm',err.limit = 4000,maxgrow = 7500)
# <li> trimmed=extract.growthdata(census1=bci.full5,census2=bci.full6,growcol='incgr',growthfunc=growth.biomass.indiv,logit='x',rounddown = FALSE,
#                            mindbh = 100,dbhunit = 'mm',err.limit = 4,maxgrow = 75)
# </ul>
# </sample>
# <source>
graph.outliers.spp=function(full,trimmed,spname='gustsu',fit=NULL,size='agb',export=NULL,xtitle='log(agb)',ytitle='growth')
{
 full=subset(full,sp==spname)
 trimmed=subset(trimmed,sp==spname)
 
 missing=!(full$treeID %in% trimmed$treeID)
 outliers=full[missing,]
 
 plot(full[,size],full$growth,pch=16,cex=0.5,xlab=xtitle,ylab=ytitle)
 points(outliers[,size],outliers$growth,col='red')
 points(trimmed[,size],trimmed$growth,col='blue')
 
 if(!is.null(fit)) overlay.growthbinmodel(fit=fit[spname],add=TRUE,newgraph=FALSE,export=export)
 
 return(outliers)
}
# </source>
# </function>

# <function>
# <name>
# graph.outliers
# </name>
# <description>
# Graph the outliers overlaid on the model and full data for all species in a model fit result.
# The argument export can be set to a window device (X11, win.graph, quartz) or a graphics option (png, pdf).
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
graph.outliers=function(full,trimmed,fit=NULL,allspp=NULL,size='agb',export=NULL,wind=X11)
{
 if(is.null(allspp)) allspp=names(fit)
 for(onesp in allspp) 
  {
   if(is.null(export)) wind(height=7,width=9)
   graph.outliers.spp(full=full,trimmed=trimmed,spname=onesp,fit=fit,size=size,export=export)
  } 
}
# </source>
# </function>


# <function>
# <name>
# binGraphSampleSpecies
# </name>
# <description>
# Make a single graph, 4 panels, of AGB growth and model fit. Must submit four species names (though they could be
# the same. Enter names of data objects (not the object, just the name in quote marks!): either one data object, or a list of four such objects; 
# if only one, all four graphs are based on the single one. Likewise, enter either one bin number, or four.
# </description>
# <arguments>
# </arguments>
# <sample>
# attach('growth/linearbin.fittrim3.bci.rdata')
# attach('growth/growthfitYiching/linearbin.fit.allspp.rdata')
# attach('growth/linearbin.fit.edoro.rdata')
# attach('growth/linearbin.fit.lenda.rdata')
# binGraphSampleSpecies(fulldata='linearbin.fit.allspp',species=c('pri2co','tri2tu','brosal','jac1co'),whichbin=2,export=NULL)
# binGraphSampleSpecies(fulldata=c('linearbin.fit.allspp','linearbin.fit.trim3','linearbin.fit.edoro3','linearbin.fit.lenda3'),
#                        species=c('PYRESH','pri2co','JULBSE','GILBDE'),whichbin=3,export=NULL)
# binGraphSampleSpecies(fulldata='linearbin.fit.allspp',species=c('pri2co','pri2co','brosal','brosal'),whichbin=c(2,3,2,3),export=NULL)
# </sample>
# <source>
binGraphSampleSpecies=function(fulldataname,species,whichbin,export=pdf,outfile="growth/linearbin.summary.pdf",h=8,w=10)
{
 if(!is.null(export)) 
  {
   on.exit(graphics.off())
   export(file=outfile,height=h,width=w)
  }

 if(length(fulldataname)==1) fulldataname=rep(fulldataname,length(species))
 if(length(whichbin)==1) whichbin=rep(whichbin,length(species))
 
 par(mfcol=c(2,2),mai=c(.75,.75,.1,.1))
 
 for(i in 1:length(species))
  {
   xtitle=ytitle=''
   if(i==1 | i==2) ytitle='AGB growth'
   if(i==2 | i==4) xtitle='DBH'

   onesp=species[i]
   spdata=get(fulldataname[i])[[onesp]]

   onebindata=spdata[[whichbin[i]]]
   graph.growthmodel.spp(fit=onebindata,maintitle=onesp,xtitle=xtitle,ytitle=ytitle)
  }
  
}
# </source>
# </function>

# <function>
# <name>
# binGraphManySpecies.Panel
# </name>
# <description>
# Make a graph, 4 panels, of AGB growth and model fit of many species overlaid, predicted functions only. Must submit names of four data objects 
# (not the object, just the name in quote marks!).
# </description>
# <arguments>
# </arguments>
# <sample>
# binGraphManySpecies.Panel(c('linearbin.fit.allspp','linearbin.fit.trim3','linearbin.fit.edoro3','linearbin.fit.lenda3'),
#                           export=pdf,yrange=c(0,.25),sitename=c('Fushan','BCI','Edoro','Lenda'),darken=5,xrange=c(-3,3.5))
# </sample>
# <source>
binGraphManySpecies.Panel=function(fulldataname,sitename,darken=8,xrange=c(-3,3),yrange=c(0,.1),export=pdf,outfile="growth/linearbin.multi.pdf",h=8,w=10)
{
 if(!is.null(export)) 
  {
   on.exit(graphics.off())
   export(file=outfile,height=h,width=w)
  }

 par(mfcol=c(2,2),mai=c(.75,.75,.1,.1))

 for(i in 1:length(fulldataname))
  {
   xtitle=ytitle=''
   if(i==1 | i==2) ytitle='AGB growth'
   if(i==2 | i==4) xtitle='DBH'

   binGraphManySpecies(fulldataname=fulldataname[i],darken=darken,xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,export=NULL)
   text(-2.5,0.8*max(yrange),sitename[i],cex=1.7,pos=4)
  } 
}
# </source>
# </function>

# <function>
# <name>
# binGraphManySpecies
# </name>
# <description>
# Make a graph of AGB growth and model fit of many species overlaid, predicted functions only. Must submit name of data objects 
# (not the object, just the name in quote marks!).
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
binGraphManySpecies=function(fulldataname,darken=8,xrange=c(-3,3),yrange=c(0,.1),xtitle=NULL,ytitle=NULL,
                             export=pdf,outfile="growth/linearbin.multi.pdf",h=8,w=10)
{
 if(!is.null(export)) 
  {
   on.exit(graphics.off())
   export(file=outfile,height=h,width=w)
  }

 fulldata=get(fulldataname)
 
 result=compare.growthbinmodel(fit=fulldata,export=NULL,makegraph=FALSE)
 sumtable=assembleBinOutput(inputtable=result,fulldata=fulldata,sitename='bci')

 species=names(fulldata) 
 nospp=length(species)
 if(darken<nospp) select=sample.int(nospp,darken)
 else select=1:nospp
 
 if(is.null(ytitle)) ytitle='AGB growth'
 if(is.null(xtitle)) xtitle='DBH'

 for(i in 1:length(species))
  {
   onesp=species[i]
   spdata=fulldata[[onesp]]

   bin=sumtable[onesp,]$MaxBin
   onebindata=spdata[[bin]]
      
   if(i==1) graph.growthmodel.spp(fit=onebindata,xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,
                                  add=FALSE,addpts=FALSE,modelclr='gray',modellwd=.5,withSD=NULL,regclr=NULL)
   
   if(i %in% select) graph.growthmodel.spp(fit=onebindata,add=TRUE,addpts=FALSE,modelclr='black',modellwd=1.5,withSD=NULL,regclr=NULL)
   else graph.growthmodel.spp(fit=onebindata,add=TRUE,addpts=FALSE,modelclr='gray',modellwd=.5,withSD=NULL,regclr=NULL)
  }

  
}
# </source>
# </function>



