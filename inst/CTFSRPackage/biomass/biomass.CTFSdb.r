# <function>
# <name>
# biomass.CTFSdb
# </name>
# <description>
# Calculate biomass from existing R-formatted tables for trees and stems using dbh allometry. By default, it uses the Chave (2005) equations.
# Note that the standard downloads of R Analytical Tables, already has the agb column filled, calculated with this routine using the default parameters for moist forest. 
# This program can be used to repeat or redo the calculation. It can also be used for other tables, always requiring two tables:  one with trees and one with all stems. 
# The function returns the same table as submitted (either tree or stem), with a column agb added; if the agb column was already present, it is replaced. 
# Calculations are done by AGB.ind and the subroutines it call.
# An alternative option is to have AGB calculations already stored in the server's AGB database, setting useChave=FALSE. This function then looks up the AGB for 
# each stem from the table named for the plot. 
# </description>
# <arguments>
# RStemTable: Name of table with one row per stem; must have dbh, species (column sp), treeID<br>
# RTreeTable: Name of table with one row per tree; must have dbh, species (column sp), treeID<br>
# whichtable: Set to "tree" to return the entire tree table with agb, or to "stem" to return stem table<br>
# dbhunit: Set to "mm", "cm", or "inch"<br>
# plot: Name of plot, matching the plot name used in the wood-density table<br>
# wsgdata: Name of R object having wood-density for species in all CTFS plots<br>
# forest: Set to "moist", "dry", or "wet" to use the equations publshed in Chave (2005) for the 3 forest types<br>
# ht.param: A vector of parameters for a formula relating tree height to dbh; if NULL, the biomass formula does not use height<br>
# htmodel: Name of an R function that returns tree height giving a dbh and any number of parameters; if ht.param is NULL, htmodel is ignored<br> 
# </arguments>
# <sample>
# CTFSplot("bci","full",census=1) <br>
# CTFSplot("bci","stem",census=1) <br>
# attach("biomass/wsg.ctfs.Rdata") <br>
# newtable=biomass.CTFSdb(RStemTable=bci.stem1,RTreeTable=bci.full1)
# </sample>
# <source>
biomass.CTFSdb=function(RStemTable,RTreeTable,whichtable='tree',dbhunit='mm',plot='bci',wsgdata=wsg.ctfs2,forest='moist',
                        ht.param=NULL,htmodel=predht.asym,useChave=TRUE,cnsno=NULL,dbname=NULL,fullname=NULL,plotcode=NULL)
{
 if(useChave)
  {
   if(whichtable=='tree')
      {
       agb.pertree=AGB.tree(df=RStemTable,dbhunit=dbhunit,plot=plot,wsgdata=wsgdata,forest=forest,ht.param=ht.param,htmodel=htmodel)   
       m=match(RTreeTable$treeID,agb.pertree$treeID)
       RTreeTable$agb=agb.pertree$agb[m]
       return(RTreeTable)
      }
 
   RStemTable$agb=AGB.ind(df=RStemTable,dbhunit=dbhunit,plot=plot,wsgdata=wsgdata,forest=forest,ht.param=ht.param,htmodel=htmodel)
   return(RStemTable)
  }

 RStemTable=AGB.dbtable(df=RStemTable,dbname=dbname,plot=fullname,code=plotcode,censusno=cnsno)
 if(whichtable=='stem') return(RStemTable)
 # browser()
 
 AGB.tree=tapply(RStemTable$agb,RStemTable$treeID,sum,na.rm=TRUE)
 m=match(RTreeTable$treeID,names(AGB.tree))
 RTreeTable$agb=AGB.tree[m]

 return(RTreeTable)   
}
# </source>
# </function>


# <function>
# <name>
# density.ind
# </name>
# <description>
# Create a vector of wood density for each individual tree based on the species name and plot. The table of individuals, called df,
# must include a dbh and a species name, the latter named sp. There must be a table of wood density submitted (wsgdata), and this
# table must have a column sp with species names, a column plot, plus the wood density in a column called wsg (though the
# name of that column can be changed using the argument denscol). The CTFS wood-density table has this structure, but any table with those
# columns will work. If a species in the df table has a matching species name in the correct plot, its wood density is taken.
# If a species is not found in the correct plot, then the mean wood density of all species in the same plot is taken. The function
# fails (returns only NAs) if there are no entries for the selected plot in the wood-density table.  
# Returns a vector of wood density of the same size as the df table submitted. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# wooddens=density.ind(df=bci.full1,plot="bci",wsg=wsg.ctfs2) #<br>
# mean(wooddens,na.rm=TRUE) #<br>
# length(which(is.na(wooddens)))
# </sample>
# <source>
density.ind=function(df,plot,wsgdata,denscol='wsg')
{
 wsgdatamatch=which(wsgdata$site %in% plot)
 if(length(wsgdatamatch)==0) return(rep(NA,dim(df)[1]))
 
 wsgdata=unique(wsgdata[wsgdatamatch,])
 meanwsg=mean(subset(wsgdata,idlevel=='species')[,denscol],na.rm=TRUE)
 
 m=match(df$sp,wsgdata$sp)
 
 result=wsgdata[m,denscol]
 result[is.na(m)]=meanwsg
 result[is.na(result)]=meanwsg
 
 return(result)
}
# </source>
# </function>


# <function>
# <name>
# AGB.ind
# </name>
# <description>
# Compute biomass (agb) based on one of the Chave (Oecologia, 2005) models for tropical forest types. 
# Requires a table (df) with dbh and species names, a wood-density table (described under density.ind), a plot name, 
# dbh units, and a forest type (for most lowland tropical plots, the default moist is recommended.)
# The height parameters default to NULL, and the Chave equations without height are then used. Alternatively, height parameters
# and a height function can be supplied, the latter to calculate height from diameter for every tree, in which case the
# Chave model with height is used. Returns a vector of biomass in kg for every individual in the submitted table df. 
# This is called by AGB.tree in the standard calculation of biomass for CTFS R tables. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# biomass=AGB.ind(df=bci.full1) #<br>
# hist(log(biomass),breaks=100) #<br>
# sum(biomass,na.rm=TRUE)/50
# </sample>
# <source>
AGB.ind=function(df,dbhunit='mm',plot='bci',wsgdata=wsg.ctfs2,forest='moist',ht.param=NULL,htmodel=predht.asym)
{
 wsg=density.ind(df=df,plot=plot,wsgdata=wsgdata,denscol='wsg')

 if(dbhunit=='mm') dbh=df$dbh/10
 else if(dbhunit=='inch') dbh=df$dbh/2.54
 else if(dbhunit=='cm') dbh=df$dbh
 
 return(Chave.AGB(dbh=dbh,density=wsg,forest=forest,htparam=ht.param,heightmodel=htmodel)/1000)
}
# </source>
# </function>


# <function>
# <name>
# AGB.tree
# </name>
# <description>
# Computes AGB of each tree in a table, grouping all stems of one tree and adding there agbs. 
# The submitted table, df, must have dbh, species name (sp),
# and a treeID to identify which tree every stem belong to. There must be just one dbh for each stem.  Returns
# a dataframe with one row per tree, including the treeID and total agb per tree. Note that it will have fewer rows
# than the table submitted. This is called by biomass.CTFSdb in the standard calculation of biomass for CTFS R tables. 
# </description>
# <arguments>
# biomasstbl=AGB.tree(df=bci.stem1)
# dim(bci.stem1)
# dim(biomasstbl)
# head(biomasstbl)
# </arguments>
# <sample>
# 
# </sample>
# <source>
AGB.tree=function(df,dbhunit='mm',plot='bci',wsgdata=wsg.ctfs2,forest='moist',ht.param=NULL,htmodel=predht.asym)
{
 AGB.stem=AGB.ind(df=df,dbhunit=dbhunit,plot=plot,wsgdata=wsgdata,forest=forest,ht.param=ht.param,htmodel=htmodel)
 
 AGB.tree=tapply(AGB.stem,df$treeID,sum,na.rm=TRUE)

 result=data.frame(treeID=as.numeric(names(AGB.tree)),agb=AGB.tree)
 return(result)
}
# </source>
# </function>


# <function>
# <name>
# Chave.AGB
# </name>
# <description>
# The Chave 2005 Oecologia model for calculating biomass from dbh in cm. All dbhs are submitted as a vector, and a vector of wood density
# of the same length must also be submitted (or a single wood density can be passed, to be used for every tree). 
# Parameter values for the 3 forest types according to Chave 2005 are hard-coded in the function.
# The recommended CTFS use is with htparam=NULL, so height is not used. If height parameters and a height model are passed,
# then the height of every tree is calculated, and the Chave AGB formula that includes height is used. The default height parameters are 
# from Chave et al 2003 on BCI biomass, and the default height function is predht.asym, provided in this file. But any height model can be
# substituted, providing the function name is passed and the necessary number of parameters included as htparam. 
# Returns a vector of biomass of same length as vector of dbh submitted. 
# This is called by AGB.tree in the standard calculation of biomass for CTFS R tables.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# testdbh=c(1,2,5,10,20,30,50,100,200) #<br>
# AGBmoist=Chave.AGB(dbh=testdbh,forest="moist") #<br>
# AGBwet=Chave.AGB(dbh=testdbh,forest="wet") #<br>
# plot(testdbh,AGBmoist,col="green",type="l") #<br>
# lines(testdbh,AGBwet,col="blue")
# </sample>
# <source>
Chave.AGB=function(dbh,density=0.62,htparam=c(41.7,.057,.748),heightmodel=predht.asym,forest='moist')
{
 if(is.null(htparam))
  {
   if(forest=="moist") param=c(-1.499,2.148,0.207,-0.0281)
   else if(forest=="dry") param=c(-.667,1.784,0.207,-.0281)
   else if(forest=="wet") param=c(-1.239,1.98,0.207,-0.0281)
  
   AGB=agb.dbhmodel(dbh,density,param)
  }
 else
  {
   if(forest=="moist") param=c(.0501,1)
   else if(forest=="dry") param=c(.112,.91)
   else if(forest=="wet") param=c(.077,.94)

   ht=heightmodel(dbh,htparam)
   AGB=agb.model(dbh,density,ht,param)
  }
  
 return(AGB)
}
# </source>
# </function>


# <function>
# <name>
# agb.model
# </name>
# <description>
# Calculates biomass from density, height, and dbh. Requires just two parameters, following Chave (2005). The parameters can be
# changed, but the formula cannot be. Returns a vector of biomass as long as vector of dbh submitted. 
# This is called by Chave.AGB in the standard calculation of biomass for CTFS R tables.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# agb.model(dbh=c(1,1,2),density=c(.6,.6,.5),height=c(2,3,4),param=c(.0501,1))
# </sample>
# <source>
agb.model=function(dbh,density,height,param)
 return(param[1]*(density*dbh^2*height)^param[2])
# </source>
# </function>


# <function>
# <name>
# agb.dbhmodel
# </name>
# <description>
# Calculates biomass from density and diameter, without height. Requires four parameters, following Chave (2005). 
# The parameters can be changed, but the formula cannot be. Returns a vector of biomass as long as vector of dbh submitted. 
# This is called by Chave.AGB in the standard calculation of biomass for CTFS R tables.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# agb.dbhmodel(dbh=c(1,1,2),density=c(.6,.6,.5),param=c(-1.499,2.148,0.207,-0.0281)) 
# </sample>
# <source>
agb.dbhmodel=function(dbh,density,param)
 return(density*exp(param[1]+param[2]*log(dbh)+param[3]*log(dbh)^2+param[4]*log(dbh)^3))
# </source>
# </function>

# <function>
# <name>
# predht.asym
# </name>
# <description>
# An allometric model predicting an asymptote at large size, used in estimating tree height as a function of dbh. 
# The model uses 3 parameters, submitted as argument param. The matrix form of param allows a different set of parameters to
# be submitted for every species. The default parameters given in the function Chave.AGB assume dbh is in cm, as do all the biomass
# allometry functions. 
# </description>
# <arguments>
# dbh: Vector of dbh<br>
# param: Either a vector of length 3, or a matrix of 3 columns; if the latter, there must be one row for each dbh<br>
# </arguments>
# <sample>
# htparam=c(41.7,.057,.748) #<br>
# d=c(1,2,5,10,20,50) #<br>
# ht=predht.asym(dbh=d,param=htparam)
# </sample>
# <source>
predht.asym=function(dbh,param)
{
 if(is.null(dim(param)))
  {
   ymax=param[1]
   a=param[2]
   b=param[3]
  }
 else
  {
   ymax=param[,1]
   a=param[,2]
   b=param[,3]
  }

 return(ymax*(1-exp(-a*dbh^b)))
}
# </source>
# </function>


# <function>
# <name>
# biomass.change
# </name>
# <description>
# Finds biomass in two censuses and change between them. The submitted dataframes are exactly the standard CTFS R Analytical tables,
# with a column for biomass (agb) already calculated. Each dataframe has a record for every tree in a single census (or a stem
# table can be passed, with one record for each stem). Biomass for all living trees is summed over whatever grouping variables are 
# submitted (split1 and
# split2) in both censuses, along with the annual rate of change in each category. Returns a list with 6 components:<br><br>
# N.1: Total biomass in first census submitted<br>
# N.2: Total biomass in second census submitted<br>
# date1: Mean date in first census<br>
# date2: Mean date in second census<br>
# interval: The mean census interval in years<br>
# little.r: The rate of biomass change, or (log(N.2)-log(N.1))/interval<br>
# If no split variables are submitted (split1=split2=NULL), each component of the list is a single number, for the entire plot. 
# If split1 is submitted but not split2, each component is a vector, one value for each category of split. If both splits are 
# submitted, each component of the list is a matrix. 
# Based closely on the function pop.change in abundance.r, and differs only in taking sum of agb instead of counting individuals.
# </description>
# <arguments>
# census1: The R Analytical Table for a single census, either tree or stem<br>
# census2: The matching R Analytical Table for a later census<br>
# alivecode: A vector of status codes that indicate a tree is alive (usually just "A" in most CTFS R tables)<br>
# mindbh: The minimum dbh to include (if NULL, all dbhs included); biomass is summed in trees larger that this<br>
# split1: A vector of exactly the same length as the number of rows in census1 and census2, with a grouping variable for each tree;
# a common use is the species name<br>
# split2: Another split vector of the same length, for example a dbh category<br>
# </arguments>
# <sample>
# CTFSplot("bci","full",census=c(3,7)) #<br>
# deltaAGB=biomass.change(bci.full3,bci.full7) #<br>
# deltaAGB.spp=biomass.change(bci.full3,bci.full7,split1=bci.full3$sp) #<br>
# deltaAGB.table=assemble.demography(deltaAGB.spp,type="a")  #<br>
# rate=deltaAGB.table$little.r #<br>
# hist(rate,breaks=50) #<br>
# summary(rate[is.finite(rate)]) #<br>
# subset(deltaAGB.table,is.infinite(rate)) #<br>
# </sample>
# <source>
biomass.change=function(census1,census2,alivecode=c("A"),mindbh=NULL,split1=NULL,split2=NULL)
{
 if(is.null(split1)) split1=rep("all",dim(census1)[1])
 if(is.null(split2)) split2=rep("all",dim(census2)[1])

 if(!is.null(mindbh))
  {
   inc1=census1$dbh>=mindbh 
   inc2=census2$dbh>=mindbh  
   incboth=census1$dbh>=mindbh | census2$dbh>=mindbh
  }
 else inc1=inc2=incboth=rep(TRUE,length(census1$dbh)) 

 alive1=alive2=rep(FALSE,dim(census1)[1])
 for(i in 1:length(alivecode))
  {
   alive1[census1$status==alivecode[i]]=TRUE
   alive2[census2$status==alivecode[i]]=TRUE
  }
 aliveboth=alive1 | alive2
 groupvar1=list(split1[inc1&alive1],split2[inc1&alive1])
 groupvar2=list(split1[inc2&alive2],split2[inc2&alive2])
 groupvarboth=list(split1[incboth&aliveboth],split2[incboth&aliveboth])

 ab1=tapply(census1$agb[inc1&alive1],groupvar1,sum,na.rm=TRUE)
 ab2=tapply(census2$agb[inc2&alive2],groupvar2,sum,na.rm=TRUE)
 dt1=tapply(census1$date[incboth&aliveboth],groupvarboth,mean,na.rm=T)
 dt2=tapply(census2$date[incboth&aliveboth],groupvarboth,mean,na.rm=T)
 startdate=tapply(census1$date[incboth&aliveboth],groupvarboth,mean,na.rm=T)
 enddate=tapply(census2$date[incboth&aliveboth],groupvarboth,mean,na.rm=T)

 class1=sort(unique(split1))
 class2=sort(unique(split2))
 
 ab1=fill.dimension(ab1,class1,class2)
 ab2=fill.dimension(ab2,class1,class2)
 dt1=fill.dimension(dt1,class1,class2,fill=NA)
 dt2=fill.dimension(dt2,class1,class2,fill=NA)
 startdate=fill.dimension(startdate,class1,class2,fill=NA)
 enddate=fill.dimension(enddate,class1,class2,fill=NA)
 
 interval=(dt2-dt1)/365.25
 little.r=(log(ab2)-log(ab1))/interval

 return(list(N.1=ab1,N.2=ab2,date1=startdate,date2=enddate,interval=interval,little.r=little.r))
}
# </source>
# </function>
#
#
#
# <function>
# <name>
# AGB.dbtable
# </name>
# <description>
# This function looks up the database named AGB in the MySQL server to get a table of biomass per stem. The MySQL table must have treeID, stemID, censusID,
# and a column agb with biomass in tons. This is used in cases where biomass for a plot has been calculated separately, using a method other than one of
# the Chave allometric equations. The alternative AGB calculation is stored for each plot in this database. The name of the table matches the plot name. 
# </description>
# <arguments>
# df: A table of individual stems.
# plot: The plot name
# </arguments>
# <sample>
#
# </sample>
# <source>
AGB.dbtable=function(df,dbname,plot,code,censusno)
{
 db=odbcConnect('mysql')
 on.exit(odbcClose(db))

 qry=pst("SELECT censusID FROM ", dbname, ".Census JOIN ", dbname, ".Site USING(PlotID) WHERE PlotCensusNumber=", censusno, " AND PlotName='", plot, "'")
 censusID=sqlQuery(db,qry)[1,1]
 
 agbqry=pst("SELECT treeID, stemID, AGB AS agb FROM agb.", code, " WHERE censusID=", censusID)
 agb=sqlQuery(db,agbqry)
 
 merged.agb=merge(df,agb,by=c('treeID','stemID'),all.x=TRUE)

 keepcol=colnames(df)
 if(! 'agb' %in% keepcol) keepcol=c(keepcol,'agb')
 
 return(merged.agb[,keepcol])
}
# </source>
# </function>
