# <function>
# <name>
# abundance
# </name>
# <description>
# Calculates total abundance or basal area, dividing data with 1 or 2 categorical variables. 
# The categorical variables must be submitted as vectors whose length
# matches exactly as the number of rows in the plot data submitted (so one per tree or stem). 
# The first vector should be the one with the most categories
# (for instances, split1=species, split2=dbhcategory). <br><br>
# The mean measurement date for all individuals in each category is
# also calculated. <br><br>

# For abundance, set type='abund' (the default), for basal area, set type='ba'. Be sure to use the R Analytical
# Stem Table for basal area. For abundance, either the stem table or full table can be used, the former to count stems, the latter trees.<br><br> 

# The object returned is a list, the first element named either abund or ba, the second named meandate. Each is an array of 
# one or two dimensions, depending on how many split variables were submitted:
# each of the dimensions of the array handles one of the categorical variables.  
# </description>
# <arguments>
# <ul>
# <li> censdata: an R Analytical Table for a full plot census, either full or stem
# <li> type: must be either 'abund', 'ba', or 'agb'
# <li> alivecode: all codes defining alive; the default 'A' is the standard CTFS designation for living trees or stems
# <li> mindbh: the minimum diameter above which the counts are done; if NULL, all (living) are included
# <li> dbhunit: 'cm' or 'mm', only used for basal area
# <li> split1: a vector of categories, one per individual
# <li> split2: another vector of categories, one per individual
# </ul>
# </arguments>
# <sample>
# CTFSplot('bci',5:6,'full')<br>
# CTFSplot('bci',5:6,'stem')<br>
# total=abundance(bci.full5,mindbh=10)<br>
# total$abund<br>
# total$meandate<br>
# totalstem=abundance(bci.stem5,mindbh=10)<br>
# BAperSpecies=abundance(bci.stem5,type='ba',mindbh=10,split1=bci.stem5$sp)<br>
# head(BAperSpecies$ba)<br>
# head(BAperSpecies$meandate)
# </sample>
# <source>
abundance=function(censdata,type='abund',alivecode=c("A"),mindbh=NULL,dbhunit='mm',split1=NULL,split2=NULL)
{
 if(is.null(split1)) split1=rep("all",dim(censdata)[1])
 if(is.null(split2)) split2=rep("all",dim(censdata)[1])

 if(!is.null(mindbh)) inc=censdata$dbh>=mindbh
 else inc=rep(TRUE,length(censdata$dbh))
 
 alive=rep(FALSE,length(censdata$dbh))
 for(i in 1:length(alivecode)) alive[censdata$status==alivecode[i]]=TRUE

 class1=sort(unique(split1))
 class2=sort(unique(split2))
 groupvar=list(split1[alive&inc],split2[alive&inc])
 
 if(type=='abund') abund=tapply(censdata$dbh[alive&inc],groupvar,length)
 else if(type=='ba') abund=tapply(censdata$dbh[alive&inc],groupvar,basum,mindbh=mindbh,dbhunit=dbhunit)
 else if(type=='agb') abund=tapply(censdata$agb[alive&inc],groupvar,sum,na.rm=TRUE)
 # browser()
 
 meandate=tapply(censdata$date[alive&inc],groupvar,mean,na.rm=TRUE)
 
 abund=fill.dimension(abund,class1,class2)
 meandate=fill.dimension(meandate,class1,class2,fill=NA)

 result=list(abund=abund,meandate=meandate)
 if(type=='ba') names(result)[1]='ba'
 else if(type=='agb') names(result)[1]='agb'
 
 return(result)
}
# </source>
# </function>



# <function>
# <name>
# abundanceperquad
# </name>
# <description>
# Finds abundance, basal area, or agb of every species per square quadrat of any size; plotdim is the x dimension then y dimension of the plot and
# must be set correctly; gridsize is the quadrat dimension. The plot is divided into a checkerboard of non-overlapping, space-filling squares.
# If the plot dimensions is not an exact multiple of the quadrat size, then a strip at the upper edge of the plot (north and east if plot
# is on cardinal directions) is omitted. For example, if gridsize=40 and plotdim=500, then there are an extra 20 meters at the upper boundary
# omitted from the calculations. <br><br>
# See abundance() for description of the other arguments and return value. The array of abundances per quadrat is useful for similarity, counting
# species and stems per quadrat, etc.
# </description>
# <arguments>
# </arguments>
# <sample>
# Nperquad=abundanceperquad(bci.full6,plotdim=c(1000,500),gridsize=100,type='abund')<br>
# colSums(Nperquad$abund)<br>
# apply(Nperquad$abund,2,countspp)<br>
# plot(colSums(Nperquad$abund),apply(Nperquad$abund,2,countspp))
# </sample>
# <source>
abundanceperquad=function(censdata,mindbh=10,plotdim=c(1000,500),gridsize=100,type='abund',dbhunit='mm')
{
 sp=censdata$sp

 quadno=gxgy.to.index(censdata$gx,censdata$gy,gridsize=gridsize,plotdim=plotdim)
 result=abundance(censdata,type=type,mindbh=mindbh,dbhunit=dbhunit,split1=sp,split2=quadno)

 allspp=unique(censdata$sp)
 maxquad=floor(plotdim[1]/gridsize)*floor(plotdim[2]/gridsize)
 allquad=1:maxquad

 if(dim(result[[type]])[1]<length(allspp) | dim(result[[type]])[2]<length(allquad))
     result[[type]]=fill.dimension(result[[type]],class1=allspp,class2=allquad,fill=0)
  
 return(result)
}
# </source>
# </function>

# <function>
# <name>
# abundance.spp
# </name>
# <description>
# A wrapper to calculate total abundance (or ba or agb) for each species in given dbh categories. The dbh categories
# are set with dbhbreaks. See abundance() for description of the other arguments and return value.
# </description>
# <arguments>
# <ul>
# <li> dbhbreaks: a vector of dbhs to define divisions of categories; the last category will be >= the final division
# </ul>
# </arguments>
# <sample>
# </sample>
# <source>
abundance.spp=function(censdata,type='abund',dbhunit='mm',alivecode=c("A"),dbhbreaks=c(10,100,300))
{
 allbreaks=c(dbhbreaks,10000)
 dbhclass=as.numeric(as.character(cut(censdata$dbh,breaks=allbreaks,right=F,labels=dbhbreaks)))

 sp=censdata$sp

 return(abundance(censdata=censdata,type=type,dbhunit=dbhunit,alivecode=alivecode,split1=sp,split2=dbhclass))
}
# </source>
# </function>



# <function>
# <name>
# pop.change
# </name>
# <description>
# Finds abundance, basal area, or agb in two censuses and the rate of change between them. 
# Accepts two dataframes, each an R Analytical Table for one census, the earlier census first. <br><br>

# Do not use this function with diameter categories as a split variable! The results won't make sense. 
# The categories need to be permanent attributes, such as species, genus, quadrat. Use instead pop.change.dbh to
# find population change of dbh categories. <br><br>

# Mean census date for a species is not the mean 
# census date for all living individuals in that census, but the mean census 
# date for all individuals alive in either census. Plants recruited between the two censuses get a first census date equal to
# the date on which the quadrat they later appear in was censused in the first
# census. Plants dead in the second census get a census date equal to the date on which their quadrat was censused <br><br>

# The return value is a list of 6 components:
# <ul>
# <li> N.1 (or BA.1 or AGB.1): an array of abundance (or basal area or agb) in the first census; one dimension of the array for split1, the second for split2
# <li> N.2 (or BA.2 or AGB.2): abundance (or basal area  or agb) in the second census in a matching array
# <li> date1: mean date of first census in a matching array
# <li> date2: mean date of second census in a matching array
# <li> interval: the time interval in years in a matching array
# <li> little.r: the rate of population change in a matching array, (log(N2)-log(N1))/time
# </ul>
# This list can be submitted to <i>assemble.demography</i> (topic utilitiesCTFS) to convert into a convenient table.
# </description>
# <arguments>
# See abundance()
# </arguments>
# <sample>
# bcichange=pop.change(bci.full5,bci.full6,type='abund',split1=bci.full5$sp,mindbh=10)<br>
# str(bcichange)<br>
# head(bcichange$N.1)<br>
# change.table=assemble.demography(bcichange,type='a')<br>
# head(change.table)
# </sample>
# <source>
pop.change=function(census1,census2,type='abund',dbhunit='mm',alivecode=c("A"),mindbh=NULL,split1=NULL,split2=NULL)
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

 if(type=='abund')
  {
   ab1=tapply(census1$dbh[inc1&alive1],groupvar1,length)
   ab2=tapply(census2$dbh[inc2&alive2],groupvar2,length)
  }
 else if(type=='ba')
  {
   ab1=tapply(census1$dbh[inc1&alive1],groupvar1,basum,dbhunit=dbhunit)
   ab2=tapply(census2$dbh[inc2&alive2],groupvar2,basum,dbhunit=dbhunit)
  }
 else if(type=='agb')
  {
   ab1=tapply(census1$agb[inc1&alive1],groupvar1,sum,na.rm=TRUE)
   ab2=tapply(census2$agb[inc2&alive2],groupvar2,sum,na.rm=TRUE)
  }
  
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

 result=list(N.1=ab1,N.2=ab2,date1=startdate,date2=enddate,interval=interval,little.r=little.r)
 if(type=='ba') names(result)[1:2]=paste('BA',1:2,sep='.')
 else if(type=='agb') names(result)[1:2]=paste('AGB',1:2,sep='.')
 
 return(result)
}
# </source>
# </function>
#
#  
# <function>
# <name>
# pop.change.dbh
# </name>
# <description>
# Finds abundance or basal area in two censuses and the rate of change between them, in several dbh categories. 
# Accepts two dataframes, each an R Analytical Table for one census, the earlier census first. 
# Only one additional splitting variable (other than dbh category) is allowed. Typically, this is species, but genus or quadrat are other examples.<br><br>
# The return value is a list of two elements, one name abund (or ba) and the other meandate, just as other abundance results. 
# Each is a table having
# one pair of columns for every dbh category: the first for census 1, the second for census 2. So if
# there are 3 dbh categories, the table has 6 columns. The rows of the table are the splitting variable (eg, species). 
# </description>
# <arguments>
# See abundance()
# </arguments>
# <sample>
# Nchange=pop.change.dbh(bci.full5,bci.full6,classbreak=c(10,100,300))<br>
# Nchange$abund<br>
# BAchangePerSpp=pop.change.dbh(bci.full5,bci.full6,classbreak=c(10,100),split=bci.full5$sp)<br>
# head(BAchangePerSpp$ba)
# </sample>
# <source>
pop.change.dbh=function(census1,census2,type='abund',dbhunit='mm',alivecode=c("A"),classbreak=c(10,100,300),split=NULL)
{
 if(is.null(split)) split=rep("all",dim(census1)[1])
 allsplit=sort(unique(split))
 
 allbreak=c(classbreak,10000)
 alldbhcat=paste(allbreak[-length(allbreak)],allbreak[-1],sep=".")
 nocats=length(alldbhcat)
 
 dbhclass1=as.numeric(as.character(cut(census1$dbh,breaks=allbreak,right=FALSE,labels=classbreak)))
 dbhclass2=as.numeric(as.character(cut(census2$dbh,breaks=allbreak,right=FALSE,labels=classbreak)))

 part1=abundance(census1,type=type,dbhunit=dbhunit,alivecode=alivecode,split1=split,split2=dbhclass1) 
 part2=abundance(census2,type=type,dbhunit=dbhunit,alivecode=alivecode,split1=split,split2=dbhclass2)
 
 abund1=fill.dimension(part1[[1]],class1=allsplit,class2=classbreak)
 abund2=fill.dimension(part2[[1]],class1=allsplit,class2=classbreak)
 date1=fill.dimension(part1$meandate,class1=allsplit,class2=classbreak,fill=NA)
 date2=fill.dimension(part2$meandate,class1=allsplit,class2=classbreak,fill=NA)
 
 if(!is.null(split)) 
   cols=as.vector(t(outer(classbreak,1:2,paste,sep="_")))
 else cols=paste("all",1:2,sep="_")

 abund=data.frame(abund1[,1],abund2[,1])
 meandate=data.frame(date1[,1],date2[,1])
 
 if(nocats>1)
   for(j in 2:nocats)
    {
     abund=data.frame(abund,abund1[,j],abund2[,j])
     meandate=data.frame(meandate,date1[,j],date2[,j])
    }

 colnames(abund)=colnames(meandate)=cols
 rownames(abund)=rownames(meandate)=allsplit

 result=list(abund=abund,meandate=meandate)
 if(type=='ba') names(result)[1]='ba'
 else if(type=='agb') names(result)[1]='agb'
 
 return(result)
}
# </source>
# </function>


# <function>
# <name>
# ba
# </name>
# <description>
# Calculates the individual basal areas (in square meters) for all submitted dbhs. The dbh units must be submitted, either
# 'cm' or 'millimeters'. The return value is a vector of basal area values of same length as the submitted vector of dbhs.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
ba=function(dbh,dbhunit='mm') 
 {
  if(dbhunit=='mm') return(pi*(dbh/2000)^2)
  if(dbhunit=='cm') return(pi*(dbh/200)^2)
 }
# </source>
# </function>


# <function>
# <name>
# basum
# </name>
# <description>
# Returns the basal area summed over all submitted dbhs. NAs can be included, as sum will be completed with na.rm=TRUE.
# </description>
# <arguments>
# </arguments>
# <sample>
# </sample>
# <source>
basum=function(dbh,mindbh=10,dbhunit='mm')
{
 if(!is.null(mindbh)) dbh=dbh[dbh>=mindbh]

 if(length(dbh)==0) return(0)
 return(sum(ba(dbh,dbhunit=dbhunit),na.rm=TRUE))
}
# </source>
# </function>

# <function>
# <name>
# abund.manycensus
# </name>
# <description>
# Collect abundances of all species across several censuses. The full R census tables are submitted as a list, as many as desired. The
# argument type can be used to choose basal area or agb, or the default for number of individuals. The mindbh to include must be given
# as an argument, but it can be NULL. If the latter, trees are counted if they have no dbh, as long as status=A. By default, any tree
# ever given code M is not counted in any census, but set excludestatus=NULL to include them.<br><br>
# A character vector of species codes can be submitted as excludespp, for instance those for unidentified trees. 
# </description>
# <arguments>
# </arguments>
# <sample>
# N=abund.manycensus(allcns=list(bci.full1,bci.full2,bci.full3,bci.full4),mindbh=10,type='abund',excludespp='uniden',excludestatus='M')<br>
# head(N)<br>
# colSums(N)<br>
# apply(N,2,countspp)<br>
# N=abund.manycensus(allcns=list(bci.full5,bci.full6),mindbh=10,type='abund',excludespp=c('uniden','tremxx'),excludestatus=NULL)<br>
# </sample>
# <source>
abund.manycensus=function(allcns=list(bci.full1,bci.full2,bci.full3),mindbh,dbhunit='mm',type='abund',excludespp=NULL,excludestatus=NULL)
{
 nocns=length(allcns)
 exclude=numeric()
 if(!is.null(excludestatus)) 
     {
      for(j in 1:nocns) for(i in 1:length(excludestatus)) exclude=c(exclude,which(allcns[[j]]$status==excludestatus[i]))
      exclude=unique(exclude)
      if(length(exclude)>0) for(j in 1:nocns) allcns[[j]]=allcns[[j]][-exclude,]
     }
   
 if(type=='abund') innertype='a'
 else innertype=type
 
 if(type=='abund') symb='N'
 else if(type=='agb') symb='AGB'
 else symb='BA'
 getcol=pst(symb,'.',as.character(1:2))
 
 result=assemble.demography(pop.change(allcns[[1]],allcns[[2]],split1=allcns[[1]]$sp,mindbh=mindbh,dbhunit=dbhunit,type=type),type=innertype)
 final=subset(result,select=getcol)
 colnames(final)=getcol
 
 if(nocns>2) for(j in 2:(nocns-1))
   final[,pst(symb,as.character(j+1))]=
     assemble.demography(pop.change(allcns[[j]],allcns[[j+1]],split1=allcns[[1]]$sp,mindbh=mindbh,dbhunit=dbhunit,type=type),type=innertype)[,pst(symb,'.2')]
   
 if(!is.null(excludespp)) 
  {
   exclude=unidentified.species(rownames(final),exactstr=excludespp)
   final=subset(final,!exclude)
  }
  
 return(final)
}  
# </source>
# </function>
